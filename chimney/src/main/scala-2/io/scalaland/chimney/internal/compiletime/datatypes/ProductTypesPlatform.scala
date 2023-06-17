package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ProductTypesPlatform extends ProductTypes { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected object ProductType extends ProductTypesModule {

    object platformSpecific {

      def isDefaultConstructor(ctor: Symbol): Boolean =
        ctor.isPublic && ctor.isConstructor && ctor.asMethod.paramLists.flatten.isEmpty

      def isCaseClassField(field: Symbol): Boolean =
        field.isMethod && field.asMethod.isGetter && field.asMethod.isCaseAccessor

      def isJavaGetter(getter: Symbol): Boolean =
        getter.isPublic && getter.isMethod && getter.asMethod.paramLists.flatten.isEmpty && isGetterName(
          getter.asMethod.name.toString
        )

      def isJavaSetter(setter: Symbol): Boolean =
        setter.isPublic && setter.isMethod && setter.asMethod.paramLists.flatten.size == 1 && isSetterName(
          setter.asMethod.name.toString
        )

      def isVar(setter: Symbol): Boolean =
        setter.isPublic && setter.isTerm && setter.asTerm.name.toString.endsWith("_$eq")

      def isJavaSetterOrVar(setter: Symbol): Boolean =
        isJavaSetter(setter) || isVar(setter)
    }

    import platformSpecific.*

    def isCaseClass[A](implicit A: Type[A]): Boolean = {
      val sym = A.typeSymbol
      sym.isClass && sym.asClass.isCaseClass && !sym.isAbstract && sym.asClass.primaryConstructor.isPublic
    }
    def isCaseObject[A](implicit A: Type[A]): Boolean = {
      val sym = A.typeSymbol
      def isScala2Enum = sym.asClass.isCaseClass
      def isScala3Enum = sym.isStatic && sym.isFinal // paramless case in S3 cannot be checked for "case"
      sym.isPublic && sym.isModuleClass && (isScala2Enum || isScala3Enum)
    }
    def isJavaBean[A](implicit A: Type[A]): Boolean = {
      val sym = A.typeSymbol
      val mem = A.members
      sym.isClass && !sym.isAbstract && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetterOrVar)
    }

    def parse[A: Type]: Option[Product[A]] =
      if (isCaseClass[A] || isCaseObject[A] || isJavaBean[A]) {
        import Type.platformSpecific.{fromUntyped, paramListsOf, returnTypeOf}
        import Expr.platformSpecific.asExpr
        import scala.collection.compat.*
        import scala.collection.immutable.ListMap

        val extractors: Product.Getters[A] = ListMap.from[String, Existential[Product.Getter[A, *]]](
          Type[A].decls
            .to(List)
            .filterNot(isGarbageSymbol)
            .filter(m => isCaseClassField(m) || isJavaGetter(m))
            .map { getter =>
              val name = getter.name.toString
              val tpe = ExistentialType(fromUntyped(returnTypeOf(Type[A], getter)))
              name -> tpe.mapK[Product.Getter[A, *]] { implicit Tpe: Type[tpe.Underlying] => _ =>
                val termName = getter.asMethod.name.toTermName
                Product.Getter[A, tpe.Underlying](
                  sourceType = Product.Getter.SourceType.ConstructorVal, // TODO
                  get =
                    // TODO: handle pathological cases like getName[Unused]()()()
                    if (getter.asMethod.paramLists.isEmpty) (in: Expr[A]) => asExpr[tpe.Underlying](q"$in.$termName")
                    else (in: Expr[A]) => asExpr[tpe.Underlying](q"$in.$termName()")
                )
              }
            }
        )

        val constructor: Product.Constructor[A] = {
          if (isJavaBean[A]) {
            val defaultConstructor =
              Type[A].decls
                .to(List)
                .filterNot(isGarbageSymbol)
                .find(isDefaultConstructor)
                .map(_ => asExpr[A](q"new ${Type[A]}()"))
                .getOrElse(assertionFailed(s"Expected default constructor for ${Type.prettyPrint[A]}"))

            val setters =
              Type[A].decls
                .to(List)
                .filterNot(isGarbageSymbol)
                .filter(s => isJavaSetter(s) || isVar(s))
                .map { setter =>
                  // Scala 3's JB setters _are_ methods ending with _= due to change in @BeanProperty behavior.
                  // We have to drop that suffix to align names, so that comparing is possible.
                  val n: String = setter.name.toString
                  val name = if (isVar(setter)) n.substring(0, n.length - "_$eq".length) else n

                  val termName = setter.asTerm.name.toTermName
                  val tpe = ExistentialType(fromUntyped(paramListsOf(Type[A], setter).flatten.head.typeSignature))
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

            val parameters: Product.Parameters = ListMap.from(setters.map { case (name, _, param) => name -> param })

            val termNames = setters.map { case (name, termName, _) => name -> termName }.toMap

            val constructor: Product.Arguments => Expr[A] = arguments => {
              parameters.foreach { case (name, param) =>
                Existential.use(param) { implicit Param: Type[param.Underlying] => _ =>
                  val argument = arguments.getOrElse(
                    name,
                    assertionFailed(s"Constructor of ${Type.prettyPrint[A]} expected expr for parameter $name")
                  )
                  if (!(argument.Underlying <:< Param)) {
                    assertionFailed(
                      s"Constructor of ${Type.prettyPrint[A]} expected expr for parameter $param of type ${Type
                          .prettyPrint[param.Underlying]}, instead got ${Type.prettyPrint(argument.Underlying)}"
                    )
                  }
                }
              }

              val beanTermName: TermName = ExprPromise.provideFreshName[A](ExprPromise.NameGenerationStrategy.FromType)

              val statements = q"val $beanTermName: ${Type[A]} = $defaultConstructor" +: arguments.view
                .filterKeys(parameters.keySet)
                .map { case (name, e) =>
                  ExistentialExpr.use(e) { implicit E: Type[e.Underlying] => expr =>
                    q"$beanTermName.${termNames(name)}($expr)"
                  }
                }
                .toList

              asExpr(q"..$statements; $beanTermName")
            }

            Product.Constructor(parameters, constructor)
          } else if (isCaseObject[A]) {
            Product.Constructor(Map.empty, _ => asExpr(q"${Type[A].typeSymbol.asClass.module}"))
          } else {
            val primaryConstructor = Option(Type[A].typeSymbol)
              .filter(_.isClass)
              .map(_.asClass.primaryConstructor)
              .filter(_.isPublic)
              .getOrElse {
                assertionFailed(s"Expected public constructor of ${Type.prettyPrint[A]}")
              }

            // default value for case class field n (1 indexed) is obtained from Companion.apply$default$n
            val defaultValues =
              primaryConstructor.typeSignature.paramLists.headOption.toList.flatten.zipWithIndex.collect {
                case (param, idx) if param.asTerm.isParamWithDefault =>
                  param.name.toString -> q"${Type[A].typeSymbol.companion}.${TermName("apply$default$" + (idx + 1))}"
              }.toMap

            val parametersRaw = paramListsOf(Type[A], primaryConstructor).map { params =>
              params
                .map { param =>
                  val name = param.name.toString
                  val tpe = ExistentialType(fromUntyped(param.typeSignatureIn(Type[A])))
                  name ->
                    tpe.mapK { implicit Tpe: Type[tpe.Underlying] => _ =>
                      Product.Parameter(
                        Product.Parameter.TargetType.ConstructorParameter,
                        defaultValues.get(name).map { value =>
                          asExpr[tpe.Underlying](value)
                        }
                      )
                    }
                }
            }

            val parameters: Product.Parameters = ListMap.from(parametersRaw.flatten)

            val constructor: Product.Arguments => Expr[A] = arguments => {
              val checkedArguments = parametersRaw.map { params =>
                params.map { case (name, param) =>
                  Existential.use(param) { implicit Param: Type[param.Underlying] => _ =>
                    val argument = arguments.getOrElse(
                      name,
                      assertionFailed(s"Constructor of ${Type.prettyPrint[A]} expected expr for parameter $name")
                    )
                    if (!(argument.Underlying <:< Param)) {
                      assertionFailed(
                        s"Constructor of ${Type.prettyPrint[A]} expected expr for parameter $param of type ${Type
                            .prettyPrint[param.Underlying]}, instead got ${Type.prettyPrint(argument.Underlying)}"
                      )
                    }
                    argument.asInstanceOf[Expr[Any]]
                  }
                }
              }

              asExpr(q"new ${Type[A]}(...$checkedArguments)")
            }

            Product.Constructor(parameters, constructor)
          }
        }

        Some(Product(extractors, constructor))
      } else None

    private val isGarbageSymbol = ((s: Symbol) => s.name.toString) andThen isGarbage
  }
}
