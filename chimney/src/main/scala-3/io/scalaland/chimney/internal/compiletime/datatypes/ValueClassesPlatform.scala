package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object ValueClass extends ValueClassModule {

    def unapply[A](implicit A: Type[A]): Option[ValueClass[A]] = if A.isAnyVal && !A.isPrimitive then {
      Some(
        new ValueClass[A] {

          private val repr: TypeRepr = TypeRepr.of[A]
          private val sym: Symbol = repr.typeSymbol

          private val getter: Symbol = sym.declarations.headOption.getOrElse {
            assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 parameter")
          }

          private val primaryConstructor: Symbol =
            Option(sym.primaryConstructor).filter(_.isClassConstructor).getOrElse {
              assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 public constructor")
            }
          private val (typeByName, typeParams) =
            Type.platformSpecific.resolveTypeArgsForMethodArguments(repr, primaryConstructor)
          private val argument = (if typeParams.nonEmpty then primaryConstructor.paramSymss.tail
                                  else primaryConstructor.paramSymss).flatten match {
            case argument :: Nil => argument
            case els =>
              println(primaryConstructor.paramSymss)
              println(els)
              assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have public constructor with 1 argument")
          }

          val Inner: Type[Inner] = Type.platformSpecific.returnType[Inner](repr.memberType(getter))
          assert(
            typeByName(argument.name).asType.asInstanceOf[Type[Inner]] =:= Inner,
            s"AnyVal ${Type.prettyPrint[A]} only parameter's type was expected to be the same as only constructor argument's type"
          )

          val fieldName: String = getter.name

          def unwrap(expr: Expr[A]): Expr[Inner] =
            expr.asTerm.select(getter).appliedToArgss(Nil).asExpr.asInstanceOf[Expr[Inner]]

          def wrap(expr: Expr[Inner]): Expr[A] = {
            val select = New(TypeTree.of[A]).select(primaryConstructor)
            val tree = if typeParams.nonEmpty then select.appliedToTypes(typeParams) else select
            tree.appliedToArgss(List(List(expr.asTerm))).asExpr.asExprOf[A]
          }
        }
      )
    } else None
  }
}
