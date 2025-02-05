package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*
  import Type.platformSpecific.*

  protected object WrapperClassType extends WrapperClassTypeModule {

    private type Cached[A] = Option[Existential[WrapperClass[A, *]]]
    private val wrapperClassCache = new Type.Cache[Cached]
    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = wrapperClassCache(Type[A]) {
      val A: TypeRepr = TypeRepr.of[A]
      val sym: Symbol = A.typeSymbol

      val getterOpt: Option[Symbol] = sym.declarations.filter(_.isPublic).headOption
      val unambiguousConstructorOpt: Option[Symbol] = publicPrimaryOrOnlyPublicConstructor(sym)
      val argumentOpt: Option[Symbol] = unambiguousConstructorOpt.flatMap { unambiguousConstructor =>
        paramListsOf(A, unambiguousConstructor).flatten match {
          case argument :: Nil => Some(argument)
          case _               => None
        }
      }

      (getterOpt, unambiguousConstructorOpt, argumentOpt) match {
        case (Some(getter), Some(unambiguousConstructor), Some(argument))
            if !Type[A].isPrimitive && getter.name == argument.name =>
          val Argument =
            fromUntyped[Any](paramsWithTypes(A, unambiguousConstructor, isConstructor = true)(argument.name))
          val inner = returnTypeOf[Any](A, getter).as_??
          import inner.Underlying as Inner
          assert(
            Argument <:< Inner,
            s"Wrapper/AnyVal ${Type.prettyPrint[A]} only property's type (${Type
                .prettyPrint(Argument)}) was expected to be the same as only constructor argument's type (${Type
                .prettyPrint(Inner)})"
          )
          Some(
            Existential[WrapperClass[A, *], Inner](
              WrapperClass[A, Inner](
                fieldName = getter.name,
                unwrap = (expr: Expr[A]) => expr.asTerm.select(getter).appliedToArgss(Nil).asExprOf[Inner],
                wrap = (expr: Expr[Inner]) => {
                  val select = New(TypeTree.of[A]).select(unambiguousConstructor)
                  val tree = if A.typeArgs.nonEmpty then select.appliedToTypes(A.typeArgs) else select
                  tree.appliedToArgss(List(List(expr.asTerm))).asExprOf[A]
                }
              )
            )
          )
        case _ => None
      }
    }
  }
}
