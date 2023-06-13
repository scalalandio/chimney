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

      type Inner
      implicit val Inner: Type[Inner] = Type.platformSpecific.returnType[Inner](repr.memberType(getter))
      assert(
        typeByName(argument.name).asType.asInstanceOf[Type[Inner]] =:= Inner,
        s"AnyVal ${Type.prettyPrint[A]} only parameter's type was expected to be the same as only constructor argument's type"
      )

      Some(
        Existential(
          ValueClass[A, Inner](
            fieldName = getter.name,
            unwrap = (expr: Expr[A]) => expr.asTerm.select(getter).appliedToArgss(Nil).asExpr.asInstanceOf[Expr[Inner]],
            wrap = (expr: Expr[Inner]) => {
              val select = New(TypeTree.of[A]).select(primaryConstructor)
              val tree = if typeParams.nonEmpty then select.appliedToTypes(typeParams) else select
              tree.appliedToArgss(List(List(expr.asTerm))).asExpr.asExprOf[A]
            }
          )
        )
      )
    } else None
  }
}
