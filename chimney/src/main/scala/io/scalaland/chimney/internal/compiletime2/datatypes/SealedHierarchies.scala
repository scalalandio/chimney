package io.scalaland.chimney.internal.compiletime2.datatypes

import io.scalaland.chimney.internal.compiletime2.ChimneyDefinitions

/** Hearth-based port of `io.scalaland.chimney.internal.compiletime.datatypes.SealedHierarchies` built on Hearth's
  * `Type.directChildren`.
  *
  * IMPORTANT RENAME: macro-commons named its view `Enum[A]`, but Hearth's cake already defines
  * `hearth.typed.Classes#Enum` (class + companion) - mixing both into one cake would be a "inherits conflicting
  * members" error. The macro-commons view is therefore exposed as [[SealedEnum]] here, and ported rules have to
  * rename `Enum` -> `SealedEnum` (a mechanical sed; `Enum.Element` -> `SealedEnum.Element`,
  * `Enum.Elements` -> `SealedEnum.Elements`).
  *
  * Semantic judgment calls (macro-commons semantics preserved unless noted):
  *   - like macro-commons, `parse` matches only sealed types (incl. Scala 3 enums) and Java enums - Hearth's extra
  *     `Enum` capabilities (Scala `Enumeration`, Scala 3 union types) are deliberately NOT exposed (rules and their
  *     tests do not expect them); TODO(hearth-migration): consider exposing them as a new feature after the flip,
  *   - subtypes are flattened recursively through nested sealed hierarchies in shared code (Hearth's `directChildren`
  *     is direct-only on Scala 3, already flattened on Scala 2 - the recursion is a no-op there); abstract non-sealed
  *     leaves are kept as elements (macro-commons behavior; Hearth's own `exhaustiveChildren` would refuse them),
  *   - like macro-commons, subtypes not conforming to `A` (GADTs) are filtered out,
  *   - on Scala 2 Hearth's per-level `ListMap` may merge same-named leaves from different nested branches (Hearth
  *     flattens before we can interleave) - same-named leaves across branches survive on Scala 3.
  */
private[compiletime2] trait SealedHierarchies { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Let us obtain a list of types implementing the sealed hierarchy.
    *
    * It describes: Scala 2's "normal" sealed hierarchies, Scala 3's enums as well as Java's enums.
    *
    * (macro-commons named this view `Enum` - see the trait's ScalaDoc for why it had to be renamed.)
    */
  final protected case class SealedEnum[A](elements: SealedEnum.Elements[A])
  protected object SealedEnum {

    final case class Element[Of, A](name: String, upcast: Expr[A] => Expr[Of])
    final type Elements[Of] = List[Existential.UpperBounded[Of, Element[Of, *]]]
  }

  protected object SealedHierarchy {

    private type Cached[A] = Option[SealedEnum[A]]
    private val enumCache = new TypeCache[Cached]
    def parse[A: Type]: Option[SealedEnum[A]] = enumCache(Type[A]) {
      if (isSealed[A] || isJavaEnum[A]) subtypesToEnum[A](flattenedSubtypes[A])
      else None
    }
    def unapply[A](tpe: Type[A]): Option[SealedEnum[A]] = parse(using tpe)

    def isJavaEnum[A: Type]: Boolean = Type.isJavaEnum[A]

    def isSealed[A: Type]: Boolean = Type.isSealed[A]

    private def flattenedSubtypes[A: Type]: List[(String, ??<:[A])] =
      Type[A].directChildren.fold(List.empty[(String, ??<:[A])]) {
        _.toList.flatMap { case (name, child) =>
          import child.Underlying as Subtype
          // Stable singleton subtypes (case objects, Scala 3 enum case vals, Java enum values) are leaves: we must
          // NOT recurse into them even when their type symbol points at a sealed parent (e.g. Color.Red.type's type
          // symbol is the sealed enum class Color).
          if (Type.isObject[Subtype] || Type.isVal[Subtype] || Type.isJavaEnumValue[Subtype]) List(name -> child)
          else if (Type.isSealed[Subtype])
            // The bound-widening cast is safe: Underlying <: Subtype <: A.
            flattenedSubtypes[Subtype].map { case (n, s) => n -> s.asInstanceOf[??<:[A]] }
          else List(name -> child)
        }
      }

    private def subtypesToEnum[A: Type](subtypes: List[(String, ??<:[A])]): Option[SealedEnum[A]] = {
      // `children` returns duplicates for multiply-inherited types - dedup the flattened list like macro-commons did.
      val deduplicated = subtypes.foldLeft(Vector.empty[(String, ??<:[A])]) { case (acc, subtype @ (name, child)) =>
        if (acc.exists { case (n, c) => n == name && c.Underlying =:= child.Underlying }) acc
        else acc :+ subtype
      }
      Some(
        SealedEnum(
          deduplicated.toList
            .map { case (name, child) =>
              import child.Underlying as Subtype
              Existential.UpperBounded[A, SealedEnum.Element[A, *], Subtype](
                SealedEnum.Element[A, Subtype](name = name, upcast = (expr: Expr[Subtype]) => expr.upcast[A])
              )
            }
            // with GADT we can have subtypes that shouldn't appear in pattern matching
            .filter(_.Underlying <:< Type[A])
        )
      )
    }
  }
}
