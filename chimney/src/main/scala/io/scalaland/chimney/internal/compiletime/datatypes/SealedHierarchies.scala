package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

/** Hearth-based port of the pre-Hearth `io.scalaland.chimney.internal.compiletime.datatypes.SealedHierarchies` built on
  * Hearth's `Type.directChildren`.
  *
  * IMPORTANT RENAME: macro-commons named its view `Enum[A]`, but Hearth's cake already defines
  * `hearth.typed.Classes#Enum` (class + companion) - mixing both into one cake would be a "inherits conflicting
  * members" error. The macro-commons view is therefore exposed as [[SealedEnum]] here, and ported rules have to rename
  * `Enum` -> `SealedEnum` (a mechanical sed; `Enum.Element` -> `SealedEnum.Element`, `Enum.Elements` ->
  * `SealedEnum.Elements`).
  *
  * Semantic judgment calls (macro-commons semantics preserved unless noted):
  *   - like macro-commons, `parse` matches only sealed types (incl. Scala 3 enums) and Java enums - Hearth's extra
  *     `Enum` capabilities (Scala `Enumeration`, Scala 3 union types) are deliberately NOT exposed (rules and their
  *     tests do not expect them); TODO(hearth-migration): consider exposing them as a new, opt-in feature,
  *   - subtypes are flattened recursively through nested sealed hierarchies in shared code (Hearth's `directChildren`
  *     is direct-only on Scala 3, already flattened on Scala 2 - the recursion is a no-op there); abstract non-sealed
  *     leaves are kept as elements (macro-commons behavior; Hearth's own `exhaustiveChildren` would refuse them),
  *   - like macro-commons, subtypes not conforming to `A` (GADTs) are filtered out,
  *   - on Scala 2 Hearth's per-level `ListMap` may merge same-named leaves from different nested branches (Hearth
  *     flattens before we can interleave) - same-named leaves across branches survive on Scala 3.
  */
private[compiletime] trait SealedHierarchies { this: ChimneyDefinitions & hearth.MacroCommons =>

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

  /** Extracts the flattened list of sealed subtypes / Java enum values, PRESERVING same-named entries.
    *
    * HEARTH 0.4.0 API-SHAPE WORKAROUND (Scala 2): Hearth's `Type.directChildren` returns a
    * `ListMap[String, UntypedType]` keyed by the SIMPLE subtype name, and on Scala 2 it flattens nested sealed
    * hierarchies itself - so two same-named subtypes defined in different scopes (e.g. `colors4.Green` and
    * `colors4.Color.Green`) collapse into one entry and the ambiguity detection silently disappears. The Scala 2
    * `PlatformBridge` overrides this with a port of the old macro-commons `extractSealedSubtypes` (symbol-based,
    * position+name ordered, duplicates preserved). The shared default (used on Scala 3, where `directChildren` is
    * direct-only and per-level) keeps the Hearth-based recursion.
    */
  protected def sealedSubtypesCompat[A: Type]: List[(String, ??<:[A])] =
    Type[A].directChildren.fold(List.empty[(String, ??<:[A])]) {
      _.toList.flatMap { case (name, child) =>
        import child.Underlying as Subtype
        // Stable singleton subtypes (case objects, Scala 3 enum case vals, Java enum values) are leaves: we must
        // NOT recurse into them even when their type symbol points at a sealed parent (e.g. Color.Red.type's type
        // symbol is the sealed enum class Color).
        if (Type.isObject[Subtype] || Type.isVal[Subtype] || Type.isJavaEnumValue[Subtype]) List(name -> child)
        else if (Type.isSealed[Subtype])
          // The bound-widening cast is safe: Underlying <: Subtype <: A.
          sealedSubtypesCompat[Subtype].map { case (n, s) => n -> s.asInstanceOf[??<:[A]] }
        else List(name -> child)
      }
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

    private def flattenedSubtypes[A: Type]: List[(String, ??<:[A])] = sealedSubtypesCompat[A]

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
