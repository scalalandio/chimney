package io.scalaland.chimney.internal.compiletime.derivation.transformer.datatypes

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

trait OptionalValues { this: Derivation =>

  import Type.Implicits.*

  abstract protected class OptionalValue[Optional, Value] {
    def empty: Expr[Optional]

    def of(value: Expr[Value]): Expr[Optional]

    def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A]

    def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value]

    def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional]
  }
  object OptionalValue {

    def unapply[Optional](implicit Optional: Type[Optional]): Option[Existential[OptionalValue[Optional, *]]] =
      summonOptionalValue[Optional]
        .map { optionalValue =>
          import optionalValue.{Underlying as Value, value as optionalValueExpr}
          Existential[OptionalValue[Optional, *], Value](
            new OptionalValue[Optional, Value] {
              def empty: Expr[Optional] = optionalValueExpr.empty

              def of(value: Expr[Value]): Expr[Optional] = optionalValueExpr.of(value)

              def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
                optionalValueExpr.fold(optional, onNone, onSome)

              def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
                optionalValueExpr.getOrElse(optional, onNone)

              def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
                optionalValueExpr.orElse(optional, optional2)
            }
          )
        }
        .orElse {
          Type[Optional] match {
            case Type.Option(value) =>
              import value.Underlying as Value
              Some(
                Existential[OptionalValue[Optional, *], Value](
                  new OptionalValue[Optional, Value] {
                    def empty: Expr[Optional] = Expr.Option.empty[Value].upcastToExprOf[Optional]

                    def of(value: Expr[Value]): Expr[Optional] = Expr.Option(value).upcastToExprOf[Optional]

                    def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
                      Expr.Option.fold(optional.upcastToExprOf[Option[Value]])(onNone)(onSome)

                    def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
                      Expr.Option.getOrElse(optional.upcastToExprOf[Option[Value]])(onNone)

                    def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
                      Expr.Option
                        .orElse(optional.upcastToExprOf[Option[Value]], optional2.upcastToExprOf[Option[Value]])
                        .upcastToExprOf[Optional]
                  }
                )
              )
            case _ => None
          }
        }
  }
}
