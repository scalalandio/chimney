package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

trait SealedHierarchies { this: Definitions =>

  /** Let us obtain a list of types implementing the sealed hierarchy.
    *
    * It describes: Scala 2's "normal" sealed hierarchies, Scala 3's enums as well as Java's enums.
    */
  final protected case class Enum[A](elements: Enum.Elements[A])
  protected object Enum {

    final case class Element[Of, A](name: String, upcast: Expr[A] => Expr[Of])
    final type Elements[Of] = List[Existential.UpperBounded[Of, Element[Of, *]]]
  }

  protected val SealedHierarchy: SealedHierarchyModule
  protected trait SealedHierarchyModule { this: SealedHierarchy.type =>

    def parse[A: Type]: Option[Enum[A]]
    final def unapply[A](tpe: Type[A]): Option[Enum[A]] = parse(using tpe)

    def isJavaEnum[A: Type]: Boolean

    def isSealed[A: Type]: Boolean
  }

  implicit class SealedHierarchyOps[A](private val tpe: Type[A]) {

    def isSealed: Boolean = SealedHierarchy.isSealed(using tpe)
  }
}
