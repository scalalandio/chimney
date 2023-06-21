package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ProductTypesPlatform extends ProductTypes { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object ProductType extends ProductTypesModule {

    object platformSpecific {

      private val nonPrivateFlags = Flags.Private | Flags.PrivateLocal | Flags.Protected

      def isPublic(sym: Symbol): Boolean = !(sym.flags & nonPrivateFlags).is(nonPrivateFlags)

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

    def isCaseClass[A](implicit A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      sym.isClassDef && sym.flags.is(Flags.Case) && !sym.flags.is(Flags.Abstract) && isPublic(sym.primaryConstructor)
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
      sym.isClassDef && !sym.flags.is(Flags.Abstract) && mem.exists(isDefaultConstructor) && mem.exists(
        isJavaSetterOrVar
      )
    }

    def parse[A: Type]: Option[Product[A]] = if isCaseClass[A] || isCaseObject[A] || isJavaBean[A] then {
      import Type.platformSpecific.*
      import scala.collection.immutable.ListMap

      val sym = TypeRepr.of[A].typeSymbol

      val extractors: Product.Getters[A] = ListMap.from[String, Existential[Product.Getter[A, *]]] {
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
          val tpe = ExistentialType(returnTypeOf[Any](TypeRepr.of[A].memberType(getter)))
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
      }

      val constructor: Product.Constructor[A] =
        if isJavaBean[A] then {
          val defaultConstructor = sym.declarations
            .find(isDefaultConstructor)
            .map { ctor =>
              ctor.paramSymss match {
                // new Bean[...]
                case typeArgs :: Nil if typeArgs.exists(_.isType) =>
                  New(TypeTree.of[A])
                    .select(ctor)
                    .appliedToTypes(TypeRepr.of[A].typeArgs)
                    .appliedToArgss(Nil)
                    .asExprOf[A]
                // new Bean[...]()
                case typeArgs :: Nil :: Nil if typeArgs.exists(_.isType) =>
                  New(TypeTree.of[A])
                    .select(ctor)
                    .appliedToTypes(TypeRepr.of[A].typeArgs)
                    .appliedToNone
                    .asExprOf[A]
                // new Bean
                case Nil =>
                  New(TypeTree.of[A]).select(ctor).appliedToArgss(Nil).asExprOf[A]
                // new Bean()
                case Nil :: Nil =>
                  New(TypeTree.of[A]).select(ctor).appliedToNone.asExprOf[A]
                case _ =>
                  ??? // should never happen due to isDefaultConstructor filtering
              }
            }
            .getOrElse(assertionFailed(s"Expected default constructor for ${Type.prettyPrint[A]}"))

          val setters = sym.methodMembers
            .filterNot(isGarbageSymbol)
            .filter(isJavaSetterOrVar)
            .map { setter =>
              val name = setter.name
              val tpe = ExistentialType {
                val MethodType(_, List(tpe), _) = TypeRepr.of[A].memberType(setter): @unchecked
                tpe.asType.asInstanceOf[Type[Any]]
              }
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

          val parameters: Product.Parameters = ListMap.from(setters.map { case (name, _, param) => name -> param })

          val methodSymbols = setters.map { case (name, symbol, _) => name -> symbol }.toMap

          val constructor: Product.Arguments => Expr[A] = arguments => {
            val beanSymbol: Symbol =
              ExprPromise.provideFreshName[A](ExprPromise.NameGenerationStrategy.FromType, ExprPromise.UsageHint.None)
            val beanRef = Ref(beanSymbol)

            val checkedArguments = checkArguments(parameters, arguments)
              .map[Term] { case (name, e) =>
                beanRef.select(methodSymbols(name)).appliedTo(e.value.asTerm)
              }
              .toList

            val statements = ValDef(beanSymbol, Some(defaultConstructor.asTerm)) +: checkedArguments

            Block(statements, beanRef).asExprOf[A]
          }

          Product.Constructor(parameters, constructor)
        } else if isCaseObject[A] then {
          if sym.flags.is(Flags.Case | Flags.Enum | Flags.JavaStatic) then
            // Scala 3 case object (enum's case without parameters)
            Product.Constructor(ListMap.empty, _ => Ref(sym).asExprOf[A])
          else
            // Scala 2 case object
            Product.Constructor(ListMap.empty, _ => Ref(sym.companionModule).asExprOf[A])
        } else {
          val primaryConstructor =
            Option(TypeRepr.of[A].typeSymbol.primaryConstructor).filter(s => !s.isNoSymbol).filter(isPublic).getOrElse {
              assertionFailed(s"Expected public constructor of ${Type.prettyPrint[A]}")
            }

          val (typeByName, typeParams) = resolveTypeArgsForMethodArguments(TypeRepr.of[A], primaryConstructor)

          val defaultValues = {
            val ps = primaryConstructor.paramSymss
            // TODO: assumes type parameters are always at the beginning
            if typeParams.nonEmpty then ps.tail else ps
          }.headOption.toList.flatten.zipWithIndex.collect {
            case (param, idx) if param.flags.is(Flags.HasDefault) =>
              val mod = TypeRepr.of[A].typeSymbol.companionModule
              val default = mod.declaredMethod(caseClassApplyDefaultScala2(idx + 1)) ++
                mod.declaredMethod(caseClassApplyDefaultScala3(idx + 1))
              val sym = default.head
              param.name -> Ref(mod).select(sym)
          }.toMap

          val parametersRaw = primaryConstructor.paramSymss.filterNot(_.exists(_.isType)).map { params =>
            params
              .map { param =>
                val name = param.name.toString
                val tpe = ExistentialType(typeByName(name).asType.asInstanceOf[Type[Any]])
                name ->
                  tpe.mapK { implicit Tpe: Type[tpe.Underlying] => _ =>
                    Product.Parameter(
                      Product.Parameter.TargetType.ConstructorParameter,
                      defaultValues.get(name).map { value =>
                        value.asExprOf[tpe.Underlying]
                      }
                    )
                  }
              }
          }

          // TODO: print parameters when list is empty

          val parameters: Product.Parameters = ListMap.from(parametersRaw.flatten)

          val constructor: Product.Arguments => Expr[A] = arguments => {
            val unadaptedCheckedArguments = checkArguments(parameters, arguments)

            val checkedArguments = parametersRaw.map { params =>
              params.map { case (name, _) =>
                unadaptedCheckedArguments(name).value.asTerm
              }
            }

            {
              val tree = New(TypeTree.of[A]).select(primaryConstructor)
              if typeParams.nonEmpty then tree.appliedToTypes(typeParams) else tree
            }.appliedToArgss(checkedArguments).asExprOf[A]
          }

          Product.Constructor(parameters, constructor)
        }

      Some(Product(extractors, constructor))
    } else None

    private val isGarbageSymbol = ((s: Symbol) => s.name) andThen isGarbage
  }
}
