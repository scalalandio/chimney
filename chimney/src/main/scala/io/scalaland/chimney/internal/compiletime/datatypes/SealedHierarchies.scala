package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait SealedHierarchies { this: Definitions =>

  final protected case class Enum[A](components: Enum.Elements[A])
  protected object Enum {

    final def unapply[A](implicit tpe: Type[A]): Option[Enum[A]] = SealedHierarchy.parse[A]

    final case class Element[Of, A](name: String, upcast: Expr[A] => Expr[Of])
    final type Elements[Of] = List[Existential[Element[Of, *]]]
  }

  protected val SealedHierarchy: SealedHierarchyModule
  protected trait SealedHierarchyModule { this: SealedHierarchy.type =>

    def parse[A: Type]: Option[Enum[A]]

    def isSealed[A](A: Type[A]): Boolean
  }

  implicit class SealedHierarchyOps[A](private val tpe: Type[A]) {

    def isSealed: Boolean = SealedHierarchy.isSealed(tpe)
  }
}
