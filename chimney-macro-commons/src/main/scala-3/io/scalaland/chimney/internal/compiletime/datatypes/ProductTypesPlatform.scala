package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.immutable.ListMap

private[compiletime] trait ProductTypesPlatform extends ProductTypes { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object ProductType extends ProductTypesModule {

    object platformSpecific {

      private val abstractFlags = Flags.Abstract | Flags.Trait
      private val privateFlags = Flags.Private | Flags.PrivateLocal | Flags.Protected

      def isAbstract(sym: Symbol): Boolean =
        (sym.flags & abstractFlags) != Flags.EmptyFlags

      def isPublic(sym: Symbol): Boolean =
        (sym.flags & privateFlags) == Flags.EmptyFlags

      def isParameterless(method: Symbol): Boolean =
        method.paramSymss.filterNot(_.exists(_.isType)).flatten.isEmpty

      def isDefaultConstructor(ctor: Symbol): Boolean =
        isPublic(ctor) && ctor.isClassConstructor && isParameterless(ctor)

      def isAccessor(accessor: Symbol): Boolean =
        isPublic(accessor) && accessor.isDefDef && isParameterless(accessor)

      // assuming isAccessor was tested earlier
      def isJavaGetter(getter: Symbol): Boolean =
        isGetterName(getter.name)

      def isJavaSetter(setter: Symbol): Boolean =
        isPublic(setter) && setter.isDefDef && setter.paramSymss.flatten.size == 1 && isSetterName(setter.name)

      def isVar(setter: Symbol): Boolean =
        isPublic(setter) && setter.isValDef && setter.flags.is(Flags.Mutable)

      def isJavaSetterOrVar(setter: Symbol): Boolean =
        isJavaSetter(setter) || isVar(setter)
    }

    import platformSpecific.*
    import Type.platformSpecific.*
    import Type.Implicits.*

    def isPOJO[A](implicit A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      val mem = sym.declarations
      sym.isClassDef && !isAbstract(sym) && isPublic(sym.primaryConstructor)
    }
    def isCaseClass[A](implicit A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      sym.isClassDef && sym.flags.is(Flags.Case) && !isAbstract(sym) && isPublic(sym.primaryConstructor)
    }
    def isCaseObject[A](implicit A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      def isScala2Enum = sym.flags.is(Flags.Case | Flags.Module)
      def isScala3Enum = sym.flags.is(Flags.Case | Flags.Enum | Flags.JavaStatic)
      isPublic(sym) && (isScala2Enum || isScala3Enum)
    }
    def isJavaBean[A](implicit A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      val mem = sym.declarations
      isPOJO[A] && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetterOrVar)
    }

    def parseExtraction[A: Type]: Option[Product.Extraction[A]] = Some(
      Product.Extraction(ListMap.from[String, Existential[Product.Getter[A, *]]] {
        import Type.platformSpecific.*
        val A = TypeRepr.of[A]
        val sym = A.typeSymbol

        // case class fields appear once in sym.caseFields as vals and once in sym.declaredMethods as methods
        // additionally sometimes they appear twice! once as "val name" and once as "method name " (notice space at the end
        // of name). This breaks matching by order (tuples) but has to be fixed in a way that doesn't filter out fields
        // for normal cases.
        val caseFields = sym.caseFields.zipWithIndex
          .groupBy(_._1.name.trim)
          .view
          .map {
            case (_, Seq(fieldIdx, _)) if fieldIdx._1.isDefDef => fieldIdx
            case (_, Seq(_, fieldIdx)) if fieldIdx._1.isDefDef => fieldIdx
            case (_, fieldIdxs)                                => fieldIdxs.head
          }
          .toList
          .sortBy(_._2)
          .map(_._1)
          .toList
        val caseFieldNames = caseFields.map(_.name.trim).toSet

        def isCaseFieldName(sym: Symbol) = caseFieldNames(sym.name.trim)

        val accessorsAndGetters = sym.methodMembers
          .filterNot(_.paramSymss.exists(_.exists(_.isType))) // remove methods with type parameters
          .filterNot(isGarbageSymbol)
          .filterNot(isCaseFieldName)
          .filter(isAccessor)

        // if we are taking caseFields but then we also are using ALL fieldMembers shouldn't we just use fieldMembers?
        (caseFields ++ sym.fieldMembers ++ accessorsAndGetters).filter(isPublic).distinct.map { getter =>
          val name = getter.name
          val tpe = ExistentialType(returnTypeOf[Any](A.memberType(getter)))
          name -> tpe.mapK[Product.Getter[A, *]] { implicit Tpe: Type[tpe.Underlying] => _ =>
            Product.Getter(
              sourceType =
                if isCaseFieldName(getter) then Product.Getter.SourceType.ConstructorVal
                else if isJavaGetter(getter) then Product.Getter.SourceType.JavaBeanGetter
                else if getter.isValDef then Product.Getter.SourceType.ConstructorVal
                else Product.Getter.SourceType.AccessorMethod,
              get =
                // TODO: pathological cases like def foo[Unused]()()()
                if getter.paramSymss.isEmpty then
                  (in: Expr[A]) => in.asTerm.select(getter).appliedToArgss(Nil).asExprOf[tpe.Underlying]
                else (in: Expr[A]) => in.asTerm.select(getter).appliedToNone.asExprOf[tpe.Underlying]
            )
          }
        }
      })
    )

    def parseConstructor[A: Type]: Option[Product.Constructor[A]] =
      if isCaseObject[A] then {
        val A = TypeRepr.of[A]
        val sym = A.typeSymbol

        if sym.flags.is(Flags.Case | Flags.Enum | Flags.JavaStatic) then
          // Scala 3 case object (enum's case without parameters)
          Some(Product.Constructor(ListMap.empty, _ => Ref(sym).asExprOf[A]))
        else
          // Scala 2 case object
          Some(Product.Constructor(ListMap.empty, _ => Ref(sym.companionModule).asExprOf[A]))
      } else if isPOJO[A] then {
        val A = TypeRepr.of[A]
        val sym = A.typeSymbol

        val primaryConstructor =
          Option(sym.primaryConstructor).filterNot(_.isNoSymbol).filter(isPublic).getOrElse {
            assertionFailed(s"Expected public constructor of ${Type.prettyPrint[A]}")
          }
        val paramss = paramListsOf(primaryConstructor)
        val paramNames = paramss.flatMap(_.map(param => param -> param.name)).toMap
        val paramTypes = paramsWithTypes(A, primaryConstructor)
        val defaultValues = paramss.flatten.zipWithIndex.collect {
          case (param, idx) if param.flags.is(Flags.HasDefault) =>
            val mod = sym.companionModule
            val default = (mod.declaredMethod(caseClassApplyDefaultScala2(idx + 1)) ++
              mod.declaredMethod(caseClassApplyDefaultScala3(idx + 1))).head
            paramNames(param) -> Ref(mod).select(default)
        }.toMap
        val constructorParameters = ListMap.from(paramss.flatMap(_.map { param =>
          val name = paramNames(param)
          val tpe = ExistentialType(paramTypes(name).asType.asInstanceOf[Type[Any]])
          name ->
            tpe.mapK { implicit Tpe: Type[tpe.Underlying] => _ =>
              Product.Parameter(
                Product.Parameter.TargetType.ConstructorParameter,
                defaultValues.get(name).map(_.asExprOf[tpe.Underlying])
              )
            }
        }))

        val setters = sym.methodMembers
          .filterNot(isGarbageSymbol)
          .filter(isJavaSetterOrVar)
          .map { setter =>
            setter.name -> setter
          }
          .filter { case (name, _) => !paramTypes.keySet(name) }
          .map { case (name, setter) =>
            val tpe = ExistentialType(paramsWithTypes(A, setter)(name).asType.asInstanceOf[Type[Any]])
            (
              name,
              setter,
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
        val setterExprs = setters.map { case (name, symbol, param) =>
          name ->
            param.mapK[Setter] {
              implicit Param: Type[param.Underlying] => _ => (exprA: Expr[A], exprArg: Expr[param.Underlying]) =>
                exprA.asTerm.select(symbol).appliedTo(exprArg.asTerm).asExprOf[Unit]
            }
        }.toMap

        val parameters: Product.Parameters = constructorParameters ++ setterParameters

        val constructor: Product.Arguments => Expr[A] = arguments => {
          val (constructorArguments, setterArguments) = checkArguments[A](parameters, arguments)

          ExprPromise
            .promise[A](ExprPromise.NameGenerationStrategy.FromType)
            .fulfilAsVal {
              // new A
              val select = New(TypeTree.of[A]).select(primaryConstructor)
              // new A[B1, B2, ...] vs new A
              val tree = if A.typeArgs.nonEmpty then select.appliedToTypes(A.typeArgs) else select
              // new A... or new A() or new A(b1, b2), ...
              tree
                .appliedToArgss(paramss.map(_.map(param => constructorArguments(paramNames(param)).value.asTerm)))
                .asExprOf[A]
            }
            .use { exprA =>
              Expr.block(
                setterArguments.map { case (name, exprArg) =>
                  val setter = setterExprs(name)
                  assert(exprArg.Underlying =:= setter.Underlying)
                  Existential.use(setter) {
                    implicit Setter: Type[setter.Underlying] => (setterExpr: Setter[setter.Underlying]) =>
                      setterExpr(exprA, exprArg.value.asInstanceOf[Expr[setter.Underlying]])
                  }
                }.toList,
                exprA
              )
            }
        }

        Some(Product.Constructor(parameters, constructor))
      } else None

    private val isGarbageSymbol = ((s: Symbol) => s.name) andThen isGarbage
  }
}
