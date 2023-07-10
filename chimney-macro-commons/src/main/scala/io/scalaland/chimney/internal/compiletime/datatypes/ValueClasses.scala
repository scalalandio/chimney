package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

trait ValueClasses { this: Definitions =>

  /** Let us unwrap and wrap value in any class that wraps a single value (not only AnyVals) */
  final protected case class WrapperClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  /** Let us unwrap and wrap value in AnyVal value class */
  final protected case class ValueClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  protected val WrapperClassType: WrapperClassTypeModule
  protected trait WrapperClassTypeModule { this: WrapperClassType.type =>

    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]]
    final def unapply[A](tpe: Type[A]): Option[Existential[WrapperClass[A, *]]] = parse(tpe)
  }

  protected object ValueClassType {
    def parse[A: Type]: Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] =
      if (Type[A].isAnyVal)
        WrapperClassType.parse[A].map {
          _.asInstanceOf[Existential.UpperBounded[AnyVal, WrapperClass[A, *]]].mapK[ValueClass[A, *]] { _ =>
            {
              case WrapperClass(fieldName, unwrap, wrap) => ValueClass(fieldName, unwrap, wrap)
              case _                                     => ???
            }
          }
        }
      else None
    def unapply[A](tpe: Type[A]): Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] = parse(tpe)
  }
}
