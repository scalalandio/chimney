package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

trait ValueClasses { this: Definitions =>

  protected trait ValueClass[Outer] {

    type Inner
    val Inner: Type[Inner]

    val fieldName: String

    def unwrap(expr: Expr[Outer]): Expr[Inner]

    def wrap(expr: Expr[Inner]): Expr[Outer]
  }

  protected val ValueClass: ValueClassModule
  protected trait ValueClassModule { this: ValueClass.type =>

    def unapply[A](implicit A: Type[A]): Option[ValueClass[A]]
  }
}
