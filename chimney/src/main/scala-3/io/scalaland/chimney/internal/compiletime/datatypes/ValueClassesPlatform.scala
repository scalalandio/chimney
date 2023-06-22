package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*
  import Type.platformSpecific.*

  protected object ValueClassType extends ValueClassTypeModule {

    def parse[A: Type]: Option[Existential[ValueClass[A, *]]] = if Type[A].isAnyVal && !Type[A].isPrimitive then {
      val A: TypeRepr = TypeRepr.of[A]
      val sym: Symbol = A.typeSymbol

      val getter: Symbol = sym.declarations.headOption.getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 parameter")
      }

      val primaryConstructor: Symbol = Option(sym.primaryConstructor).filter(_.isClassConstructor).getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 public constructor")
      }
      val typeByName = paramsWithTypes(A, primaryConstructor)
      val argument = paramListsOf(primaryConstructor).flatten match {
        case argument :: Nil => argument
        case _ => assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have public constructor with 1 argument")
      }

      val inner = ExistentialType(typeByName(argument.name).asType.asInstanceOf[Type[Any]])
      Some(inner.mapK[ValueClass[A, *]] { implicit Inner: Type[inner.Underlying] => _ =>
        assert(
          typeByName(argument.name).asType.asInstanceOf[Type[Any]] =:= Inner,
          s"AnyVal ${Type.prettyPrint[A]} only parameter's type (${Type
              .prettyPrint(typeByName(argument.name).asType.asInstanceOf[Type[Any]])}) was expected to be the same as only constructor argument's type (${Type
              .prettyPrint(Inner)})"
        )

        ValueClass[A, inner.Underlying](
          fieldName = getter.name,
          unwrap = (expr: Expr[A]) => expr.asTerm.select(getter).appliedToArgss(Nil).asExprOf[inner.Underlying],
          wrap = (expr: Expr[inner.Underlying]) => {
            val select = New(TypeTree.of[A]).select(primaryConstructor)
            val tree = if A.typeArgs.nonEmpty then select.appliedToTypes(A.typeArgs) else select
            tree.appliedToArgss(List(List(expr.asTerm))).asExprOf[A]
          }
        )
      })
    } else None
  }
}
