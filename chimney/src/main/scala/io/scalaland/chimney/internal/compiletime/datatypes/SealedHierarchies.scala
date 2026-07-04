package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

/** Thin view over Hearth's `Type.directChildren` (the substance of Hearth's `Enum[A]` class view).
  *
  * The view is named [[SealedEnum]] (not `Enum`) because Hearth's cake already defines `hearth.typed.Classes#Enum` -
  * mixing both into one cake would be an "inherits conflicting members" error.
  *
  * What stays Chimney's (rule semantics, not old-engine parity):
  *   - subtypes are flattened recursively down to LEAVES - Chimney's subtype matching is leaf-name-based across
  *     hierarchies of different nesting (Hearth's `directChildren` is direct-only on Scala 3; `exhaustiveChildren`
  *     would refuse abstract non-sealed leaves, which Chimney keeps as elements),
  *   - subtypes not conforming to `A` (GADTs) are filtered out,
  *   - `parse` matches sealed types (incl. Scala 3 enums) and Java enums; Hearth's extra `Enum` capabilities (Scala
  *     `Enumeration` values, Scala 3 union types) are not wired into the rule yet.
  */
private[compiletime] trait SealedHierarchies { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Let us obtain a list of types implementing the sealed hierarchy.
    *
    * It describes: Scala 2's "normal" sealed hierarchies, Scala 3's enums as well as Java's enums.
    *
    * (See the trait's ScalaDoc for why this is not named `Enum`.)
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

    /** Extracts the flattened list of sealed subtypes / Java enum values, PRESERVING same-named entries.
      *
      * `Type.directChildrenList` (hearth#309, added in 0.4.1) keeps duplicate simple names and the extraction order -
      * the name-keyed `directChildren` `ListMap` used to collapse same-named subtypes from different scopes (e.g.
      * `colors4.Green` vs `colors4.Color.Green`) and lose the ambiguity Chimney must detect. On Scala 2 the list comes
      * pre-flattened (recursion passes leaves through); on Scala 3 it is direct-only and the recursion below flattens
      * down to leaves.
      */
    private def flattenedSubtypes[A: Type]: List[(String, ??<:[A])] =
      Type.directChildrenList[A].fold(List.empty[(String, ??<:[A])]) {
        _.flatMap { case (name, child) =>
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
      // `children` returns duplicates for multiply-inherited types - dedup the flattened list.
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
