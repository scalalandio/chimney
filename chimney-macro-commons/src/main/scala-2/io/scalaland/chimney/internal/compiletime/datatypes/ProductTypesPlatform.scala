package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.compat.*
import scala.collection.immutable.ListMap

trait ProductTypesPlatform extends ProductTypes { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected object ProductType extends ProductTypesModule {

    object platformSpecific {

      def isParameterless(method: MethodSymbol): Boolean = method.paramLists.flatten.isEmpty

      def isDefaultConstructor(ctor: Symbol): Boolean =
        ctor != NoSymbol && ctor.isPublic && ctor.isConstructor && isParameterless(ctor.asMethod)

      def isAccessor(accessor: MethodSymbol): Boolean =
        accessor.isPublic && isParameterless(accessor)

      // assuming isAccessor was tested earlier
      def isCaseClassField(field: MethodSymbol): Boolean =
        field.isCaseAccessor

      // assuming isAccessor was tested earlier
      def isJavaGetter(getter: MethodSymbol): Boolean =
        isGetterName(getter.name.toString)

      def isJavaSetter(setter: MethodSymbol): Boolean =
        setter.isPublic && setter.paramLists.size == 1 && setter.paramLists.head.size == 1 &&
          isSetterName(setter.asMethod.name.toString)

      def isVar(setter: Symbol): Boolean =
        setter.isPublic && setter.isTerm && setter.asTerm.name.toString.endsWith("_$eq")

      def isJavaSetterOrVar(setter: Symbol): Boolean =
        (setter.isMethod && isJavaSetter(setter.asMethod)) || isVar(setter)
    }

    import platformSpecific.*
    import Type.platformSpecific.*

    def isPOJO[A](implicit A: Type[A]): Boolean = {
      val sym = A.tpe.typeSymbol
      !A.isPrimitive && !(A <:< Type[String]) && !sym.isJavaEnum && sym.isClass && !sym.isAbstract &&
      sym.asClass.primaryConstructor != NoSymbol && sym.asClass.primaryConstructor.isPublic
    }
    def isCaseClass[A](implicit A: Type[A]): Boolean =
      isPOJO[A] && A.tpe.typeSymbol.asClass.isCaseClass
    def isCaseObject[A](implicit A: Type[A]): Boolean = {
      val sym = A.tpe.typeSymbol
      def isScala2Enum = sym.asClass.isCaseClass
      def isScala3Enum = sym.isStatic && sym.isFinal // parameterless case in S3 cannot be checked for "case"
      def isScalaEnum = sym.isModuleClass && (isScala2Enum || isScala3Enum)
      sym.isPublic && (isScalaEnum || isJavaEnumValue(A.tpe))
    }
    def isJavaBean[A](implicit A: Type[A]): Boolean = {
      val mem = A.tpe.members
      isPOJO[A] && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetterOrVar)
    }

    def parseExtraction[A: Type]: Option[Product.Extraction[A]] = Some(
      Product.Extraction(
        ListMap.from[String, Existential[Product.Getter[A, *]]] {
          val localDefinitions = Type[A].tpe.decls.to(Set)
          Type[A].tpe.members.sorted
            .to(List)
            .filterNot(isGarbageSymbol)
            .collect { case method if method.isMethod => method.asMethod }
            .filter(isAccessor)
            .map { getter =>
              val name = getDecodedName(getter)
              val tpe = ExistentialType(fromUntyped(returnTypeOf(Type[A].tpe, getter)))
              def conformToIsGetters = !name.take(2).equalsIgnoreCase("is") || tpe.Underlying <:< Type[Boolean]
              name -> tpe.mapK[Product.Getter[A, *]] { implicit Tpe: Type[tpe.Underlying] => _ =>
                val termName = getter.asMethod.name.toTermName
                Product.Getter[A, tpe.Underlying](
                  sourceType =
                    if (isCaseClassField(getter)) Product.Getter.SourceType.ConstructorVal
                    else if (isJavaGetter(getter) && conformToIsGetters) Product.Getter.SourceType.JavaBeanGetter
                    else if (getter.isStable) Product.Getter.SourceType.ConstructorVal // Hmm...
                    else Product.Getter.SourceType.AccessorMethod,
                  isLocal = localDefinitions(getter),
                  get =
                    // TODO: handle pathological cases like getName[Unused]()()()
                    if (getter.asMethod.paramLists.isEmpty) (in: Expr[A]) => c.Expr[tpe.Underlying](q"$in.$termName")
                    else
                      (in: Expr[A]) =>
                        c.Expr[tpe.Underlying](
                          q"$in.$termName(...${getter.paramLists.map(_.map(_.asInstanceOf[Tree]))})"
                        )
                )
              }
            }
        }
      )
    )

    def parseConstructor[A: Type]: Option[Product.Constructor[A]] = {
      val A = Type[A].tpe
      val sym = A.typeSymbol

      if (isJavaEnumValue(A)) {
        Some(Product.Constructor(ListMap.empty, _ => c.Expr[A](q"$A")))
      } else if (isCaseObject[A]) {
        Some(Product.Constructor(ListMap.empty, _ => c.Expr[A](q"${sym.asClass.module}")))
      } else if (isPOJO[A]) {
        val primaryConstructor =
          Option(sym).filter(_.isClass).map(_.asClass.primaryConstructor).filter(_.isPublic).getOrElse {
            assertionFailed(s"Expected public constructor of ${Type.prettyPrint[A]}")
          }
        val paramss = paramListsOf(A, primaryConstructor)
        val paramNames = paramss.flatMap(_.map(param => param -> getDecodedName(param))).toMap
        val paramTypes = paramsWithTypes(A, primaryConstructor)
        lazy val companion = companionSymbol[A]
        val defaultValues = paramss.flatten.zipWithIndex.collect {
          case (param, idx) if param.asTerm.isParamWithDefault =>
            val scala2default = caseClassApplyDefaultScala2(idx + 1)
            val scala3default = caseClassApplyDefaultScala3(idx + 1)
            val foundDefault = companion.typeSignature.decls
              .to(List)
              .collectFirst {
                case method if getDecodedName(method) == scala2default => TermName(scala2default)
                case method if getDecodedName(method) == scala3default => TermName(scala3default)
              }
              .getOrElse(
                assertionFailed(
                  s"Expected that ${Type.prettyPrint[A]}'s constructor parameter `$param` would have default value: attempted `$scala2default` and `$scala3default`, found: ${companion.typeSignature.decls}"
                )
              )
            paramNames(param) -> q"$companion.$foundDefault"
        }.toMap
        val constructorParameters = ListMap.from(paramss.flatMap(_.map { param =>
          val name = paramNames(param)
          val tpe = ExistentialType(fromUntyped(param.typeSignatureIn(A)))
          name ->
            tpe.mapK { implicit Tpe: Type[tpe.Underlying] => _ =>
              Product.Parameter(
                Product.Parameter.TargetType.ConstructorParameter,
                defaultValues.get(name).map(value => c.Expr[tpe.Underlying](value))
              )
            }
        }))

        val setters =
          A.decls.sorted
            .to(List)
            .filterNot(isGarbageSymbol)
            .collect { case m if m.isMethod => m.asMethod }
            .filter(isJavaSetterOrVar)
            .map { setter =>
              // Scala 3's JB setters _are_ methods ending with _= due to change in @BeanProperty behavior.
              // We have to drop that suffix to align names, so that comparing is possible.
              val n: String = getDecodedName(setter)
              val name =
                if (isVar(setter)) n.substring(0, n.length - "_$eq".length) else n
              name -> setter
            }
            .filter { case (name, _) => !paramTypes.keySet.exists(areNamesMatching(_, name)) }
            .map { case (name, setter) =>
              val termName = setter.asTerm.name.toTermName
              val tpe = ExistentialType(fromUntyped(paramListsOf(Type[A].tpe, setter).flatten.head.typeSignature))
              (
                name,
                termName,
                tpe.mapK[Product.Parameter](_ =>
                  _ =>
                    Product.Parameter(
                      targetType = Product.Parameter.TargetType.SetterParameter,
                      defaultValue = None
                    )
                )
              )
            }
        val setterParameters = ListMap.from(setters.map { case (name, _, param) => name -> param })
        type Setter[B] = (Expr[A], Expr[B]) => Expr[Unit]
        val setterExprs = setters.map { case (name, termName, param) =>
          name ->
            param.mapK[Setter] {
              implicit Param: Type[param.Underlying] => _ => (exprA: Expr[A], exprArg: Expr[param.Underlying]) =>
                c.Expr[Unit](q"$exprA.$termName($exprArg)")
            }
        }.toMap

        val parameters: Product.Parameters = constructorParameters ++ setterParameters

        val constructor: Product.Arguments => Expr[A] = arguments => {
          val (constructorArguments, setterArguments) = checkArguments[A](parameters, arguments)

          def newExpr =
            c.Expr[A](q"new $A(...${paramss.map(_.map(param => constructorArguments(paramNames(param)).value))})")

          if (setterArguments.isEmpty) {
            newExpr
          } else {
            PrependDefinitionsTo
              .prependVal[A](newExpr, ExprPromise.NameGenerationStrategy.FromType)
              .use { exprA =>
                Expr.block(
                  setterArguments.map { case (name, exprArg) =>
                    val setter = setterExprs(name)
                    assert(exprArg.Underlying =:= setter.Underlying)
                    import setter.value as setterExpr
                    setterExpr(exprA, exprArg.value.asInstanceOf[Expr[setter.Underlying]])
                  }.toList,
                  exprA
                )
              }
          }
        }

        Some(Product.Constructor(parameters, constructor))
      } else None
    }

    private val getDecodedName = (s: Symbol) => s.name.decodedName.toString

    private val isGarbageSymbol = getDecodedName andThen isGarbage

    // Borrowed from jsoniter-scala: https://github.com/plokhotnyuk/jsoniter-scala/blob/b14dbe51d3ae6752e5a9f90f1f3caf5bceb5e4b0/jsoniter-scala-macros/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L462
    private def companionSymbol[A: Type]: Symbol = {
      val sym = Type[A].tpe.typeSymbol
      val comp = sym.companion
      if (comp.isModule) comp
      else {
        val ownerChainOf: Symbol => Iterator[Symbol] =
          s => Iterator.iterate(s)(_.owner).takeWhile(x => x != null && x != NoSymbol).toVector.reverseIterator
        val path = ownerChainOf(sym)
          .zipAll(ownerChainOf(c.internal.enclosingOwner), NoSymbol, NoSymbol)
          .dropWhile { case (x, y) => x == y }
          .takeWhile(_._1 != NoSymbol)
          .map(_._1.name.toTermName)
        // $COVERAGE-OFF$
        if (path.isEmpty) assertionFailed(s"Cannot find a companion for ${Type.prettyPrint[A]}")
        else c.typecheck(path.foldLeft[Tree](Ident(path.next()))(Select(_, _)), silent = true).symbol
        // $COVERAGE-ON$
      }
    }
  }
}
