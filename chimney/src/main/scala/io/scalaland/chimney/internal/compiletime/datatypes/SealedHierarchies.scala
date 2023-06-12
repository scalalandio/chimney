package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait SealedHierarchies { this: Definitions =>

  final protected case class SealedHierarchy[A](components: SealedHierarchy.Components[A])
  protected object SealedHierarchy {

    final def unapply[A](implicit tpe: Type[A]): Option[SealedHierarchy[A]] = parseAsSealedHierarchy[A]

    final case class Component[Of, A](name: String, upcast: Expr[A] => Expr[Of])
    final type Components[Of] = List[Existential[Component[Of, *]]]
  }

  protected def parseAsSealedHierarchy[A: Type]: Option[SealedHierarchy[A]]
}
