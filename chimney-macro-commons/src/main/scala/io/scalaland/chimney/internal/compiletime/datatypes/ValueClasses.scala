package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait ValueClasses { this: Definitions =>

  /** Let us unwrap and wrap value in AnyVal value class */
  final protected case class ValueClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  protected val ValueClassType: ValueClassTypeModule
  protected trait ValueClassTypeModule { this: ValueClassType.type =>

    def parse[A: Type]: Option[Existential[ValueClass[A, *]]]
    final def unapply[A](tpe: Type[A]): Option[Existential[ValueClass[A, *]]] = parse(tpe)
  }
}
