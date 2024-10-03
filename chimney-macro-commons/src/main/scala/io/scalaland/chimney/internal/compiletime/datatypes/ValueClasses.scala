package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

trait ValueClasses { this: Definitions =>

  /** Let us unwrap and wrap value in any class that wraps a single value (not only `AnyVal`s)
    *
    * For a class to be considered wrapper it has to:
    *   - have a public unary constructor
    *   - expose a getter of the same name and type as constructor's argument
    *
    * Basically, it is a value class without the need to extends AnyVal. This is useful since sometimes we have a type
    * which is basically a wrapper but not an `AnyVal` and we would like to unwrap it and attempt to derive code as if
    * it was `AnyVal`. Since it is very contextual, we need to have a separate utility for that.
    */
  final protected case class WrapperClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  /** Let us unwrap and wrap value in `AnyVal` value class */
  final protected case class ValueClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  protected val WrapperClassType: WrapperClassTypeModule
  protected trait WrapperClassTypeModule { this: WrapperClassType.type =>

    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]]
    final def unapply[A](tpe: Type[A]): Option[Existential[WrapperClass[A, *]]] = parse(using tpe)
  }

  protected object ValueClassType {
    def parse[A: Type]: Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] =
      if (Type[A].isAnyVal)
        WrapperClassType.parse[A].map {
          _.asInstanceOf[Existential.UpperBounded[AnyVal, WrapperClass[A, *]]].mapK[ValueClass[A, *]] { _ =>
            { case WrapperClass(fieldName, unwrap, wrap) =>
              ValueClass(fieldName, unwrap, wrap)
            }
          }
        }
      else None
    def unapply[A](tpe: Type[A]): Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] = parse(using tpe)
  }
}
