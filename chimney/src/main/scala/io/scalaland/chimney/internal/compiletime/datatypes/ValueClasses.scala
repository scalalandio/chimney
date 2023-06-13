package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait ValueClasses { this: Definitions =>

  protected case class ValueClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )
  protected object ValueClass {

    def unapply[A](implicit A: Type[A]): Option[Existential[ValueClass[A, *]]] = ValueClassType.parse(A)
  }

  protected val ValueClassType: ValueClassTypeModule
  protected trait ValueClassTypeModule { this: ValueClassType.type =>

    def parse[A: Type]: Option[Existential[ValueClass[A, *]]]
  }
}
