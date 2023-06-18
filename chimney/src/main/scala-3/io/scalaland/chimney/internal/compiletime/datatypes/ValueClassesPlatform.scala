package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object ValueClassType extends ValueClassTypeModule {

    def parse[A: Type]: Option[Existential[ValueClass[A, *]]] = if Type[A].isAnyVal && !Type[A].isPrimitive then {
      val repr: TypeRepr = TypeRepr.of[A]
      val sym: Symbol = repr.typeSymbol

      val getter: Symbol = sym.declarations.headOption.getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 parameter")
      }

      val primaryConstructor: Symbol =
        Option(sym.primaryConstructor).filter(_.isClassConstructor).getOrElse {
          assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 public constructor")
        }
      val (typeByName, typeParams) =
        Type.platformSpecific.resolveTypeArgsForMethodArguments(repr, primaryConstructor)
      val argument = (if typeParams.nonEmpty then primaryConstructor.paramSymss.tail
                      else primaryConstructor.paramSymss).flatten match {
        case argument :: Nil => argument
        case els =>
          assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have public constructor with 1 argument")
      }

      // TODO: ask Janek about the best way of computing tpe.typeSignatureIn(tpe2)
      val inner = ExistentialType(typeByName(argument.name).asType.asInstanceOf[Type[Any]])
      // val inner = ExistentialType(Type.platformSpecific.returnTypeOf[Any](repr.memberType(getter)))
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
            val tree = if typeParams.nonEmpty then select.appliedToTypes(typeParams) else select
            tree.appliedToArgss(List(List(expr.asTerm))).asExprOf[A]
          }
        )
      })
    } else None
  }
}
