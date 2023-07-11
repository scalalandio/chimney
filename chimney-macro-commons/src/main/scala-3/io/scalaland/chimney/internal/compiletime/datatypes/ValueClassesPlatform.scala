package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*
  import Type.platformSpecific.*

  protected object WrapperClassType extends WrapperClassTypeModule {

    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = {
      val A: TypeRepr = TypeRepr.of[A]
      val sym: Symbol = A.typeSymbol

      val getterOpt: Option[Symbol] = sym.declarations.headOption
      val primaryConstructorOpt: Option[Symbol] = Option(sym.primaryConstructor).filter(_.isClassConstructor)
      val argumentOpt: Option[Symbol] = primaryConstructorOpt.flatMap { primaryConstructor =>
        paramListsOf(primaryConstructor).flatten match {
          case argument :: Nil => Some(argument)
          case _               => None
        }
      }

      (getterOpt, primaryConstructorOpt, argumentOpt) match {
        case (Some(getter), Some(primaryConstructor), Some(argument)) if !Type[A].isPrimitive =>
          val typeByName = paramsWithTypes(A, primaryConstructor, isConstructor = true)
          val inner = ExistentialType(typeByName(argument.name).asType.asInstanceOf[Type[Any]])
          import inner.Underlying as Inner
          assert(
            typeByName(argument.name).asType.asInstanceOf[Type[Any]] =:= Inner,
            s"Wrapper/AnyVal ${Type.prettyPrint[A]} only parameter's type (${Type
                .prettyPrint(typeByName(argument.name).asType.asInstanceOf[Type[Any]])}) was expected to be the same as only constructor argument's type (${Type
                .prettyPrint(Inner)})"
          )
          Some(
            Existential[WrapperClass[A, *], inner.Underlying](
              WrapperClass[A, inner.Underlying](
                fieldName = getter.name, // TODO: use utility from Products
                unwrap = (expr: Expr[A]) => expr.asTerm.select(getter).appliedToArgss(Nil).asExprOf[inner.Underlying],
                wrap = (expr: Expr[inner.Underlying]) => {
                  val select = New(TypeTree.of[A]).select(primaryConstructor)
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
