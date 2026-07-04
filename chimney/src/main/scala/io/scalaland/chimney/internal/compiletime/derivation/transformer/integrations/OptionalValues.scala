package io.scalaland.chimney.internal.compiletime.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation

/** NB: `buildInOptionSupport` is hoisted into a helper method with regular type params ([[mkBuildInOptionSupport]]) -
  * cross-quotes inside an existential-importing lambda trip the plugin's workaround-method generation.
  *
  * EXTENSION FALLBACK: [[OptionalValue.parse]] has a THIRD alternative consulting Hearth's `IsOption` providers
  * (built-ins AND ServiceLoader-registered `StandardMacroExtension`s). Precedence and guards:
  *   - it ranks BELOW `providedSupport` (the [[io.scalaland.chimney.integrations.OptionalValue]] implicit) - e.g.
  *     `java.util.Optional` resolves through chimney-java-collections' implicit when its import is present, and only
  *     falls back to Hearth's `IsOptionProviderForJavaOptional` without it,
  *   - it ranks below `buildInOptionSupport`, so every `scala.Option`-shaped type (incl. `Some`/`None.type`/
  *     `Option[Nothing]` edge cases where Hearth's provider semantics differ) keeps the built-in handling,
  *   - it is SKIPPED when a `TotallyBuildIterable`/`PartiallyBuildIterable` implicit exists for the type: integrations
  *     implicits beat extension providers, and because OptionToOption/ToOption rules run BEFORE
  *     MapToMap/IterableToIterable, an extension-provided option-ness would otherwise hijack a type whose owner
  *     declared it a collection via implicits,
  *   - Hearth's `fold` takes `Expr[Item] => Expr[A]` where chimney's takes `Expr[Value => A]` - adapted by emitting a
  *     function application (extension types only).
  * On the JVM, Hearth's built-in `IsOptionProviderForJavaOptional` makes `java.util.Optional` derivable WITHOUT the
  * chimney-java-collections import. (On JS/Native Hearth ships no Java providers - the fallback never matches there.)
  */
trait OptionalValues { this: Derivation & hearth.MacroCommons & hearth.std.StdExtensions =>

  // Cross-quotes helper in a method with regular type parameters (the cross-quotes helper-def pattern).
  private def applyFunctionCompat[A: Type, B: Type](fn: Expr[A => B], a: Expr[A]): Expr[B] = Expr.quote {
    Expr.splice(fn).apply(Expr.splice(a))
  }

  /** Something allowing us to share the logic which handles [[scala.Option]], [[java.util.Optional]] and whatever we
    * want to support.
    *
    * Tries to use [[io.scalaland.chimney.integrations.OptionalValue]] and then falls back on [[scala.Option]] hardcoded
    * support, if type is eligible.
    */
  abstract protected class OptionalValue[Optional, Value] {
    def empty: Expr[Optional]

    def of(value: Expr[Value]): Expr[Optional]

    def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A]

    def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value]

    def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional]
  }
  protected object OptionalValue {

    private lazy val OptionCtor: Type.Ctor1[Option] = Type.Ctor1.of[Option]

    private type Cached[A] = Option[Existential[OptionalValue[A, *]]]
    private val optionalCache = new TypeCache[Cached]
    def parse[Optional](implicit Optional: Type[Optional]): Option[Existential[OptionalValue[Optional, *]]] =
      optionalCache(Optional)(
        providedSupport[Optional].orElse(buildInOptionSupport[Optional]).orElse(hearthProviderSupport[Optional])
      )
    def unapply[Optional](Optional: Type[Optional]): Option[Existential[OptionalValue[Optional, *]]] = parse(using
      Optional
    )

    private def providedSupport[Optional: Type]: Option[Existential[OptionalValue[Optional, *]]] =
      summonOptionalValue[Optional].map { optionalValue =>
        import optionalValue.{Underlying as Value, value as optionalValueExpr}
        Existential[OptionalValue[Optional, *], Value](
          new OptionalValue[Optional, Value] {
            def empty: Expr[Optional] = optionalValueExpr.empty

            def of(value: Expr[Value]): Expr[Optional] = optionalValueExpr.of(value)

            def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
              optionalValueExpr.fold(optional, onNone, onSome)

            def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
              optionalValueExpr.getOrElse(optional, onNone)

            def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
              optionalValueExpr.orElse(optional, optional2)

            override def toString: String = s"support provided by ${Expr.prettyPrint(optionalValueExpr)}"
          }
        )
      }

    private def buildInOptionSupport[Optional: Type]: Option[Existential[OptionalValue[Optional, *]]] =
      OptionCtor.unapply(Type[Optional]).map { value =>
        import value.Underlying as Value
        mkBuildInOptionSupport[Optional, Value]
      }

    // Kept in a separate method (regular type parameters) - cross-quotes referencing existential-imported types
    // directly inside the `.map` lambda trip the plugin's workaround-method generation.
    @scala.annotation.nowarn("msg=is never used")
    private def mkBuildInOptionSupport[Optional: Type, Value: Type]: Existential[OptionalValue[Optional, *]] =
      Existential[OptionalValue[Optional, *], Value](
        new OptionalValue[Optional, Value] {
          implicit private val OptionValue: Type[Option[Value]] = Type.of[Option[Value]]

          def empty: Expr[Optional] = Expr.quote(Option.empty[Value]).upcast[Optional]

          def of(value: Expr[Value]): Expr[Optional] = Expr.quote(Option(Expr.splice(value))).upcast[Optional]

          def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
            Expr.quote(Expr.splice(optional.upcast[Option[Value]]).fold(Expr.splice(onNone))(Expr.splice(onSome)))

          def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
            Expr.quote(Expr.splice(optional.upcast[Option[Value]]).getOrElse(Expr.splice(onNone)))

          def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
            Expr
              .quote(Expr.splice(optional.upcast[Option[Value]]).orElse(Expr.splice(optional2.upcast[Option[Value]])))
              .upcast[Optional]

          override def toString: String = s"support build-in for Option-type ${Type.prettyPrint[Optional]}"
        }
      )

    /** Fallback consulting Hearth `IsOption` providers - see the trait's ScalaDoc for guards and their rationale. */
    private def hearthProviderSupport[Optional: Type]: Option[Existential[OptionalValue[Optional, *]]] = {
      ensureStandardExtensionsLoaded()
      IsOption.unapply(Type[Optional]).flatMap { isOption =>
        // Integrations implicits beat extension providers (see the trait's ScalaDoc) - only summoned when a provider
        // actually matched, so the extra implicit searches don't slow down the common (non-optional-type) path.
        if (summonTotallyBuildIterable[Optional].isDefined || summonPartiallyBuildIterable[Optional].isDefined) None
        else {
          import isOption.{Underlying as Value, value as isOptionOf}
          Some(mkHearthOptionSupport[Optional, Value](isOptionOf))
        }
      }
    }

    // Kept in a separate method (regular type parameters) for the same cross-quotes reason as mkBuildInOptionSupport.
    private def mkHearthOptionSupport[Optional: Type, Value: Type](
        isOptionOf: IsOptionOf[Optional, Value]
    ): Existential[OptionalValue[Optional, *]] =
      Existential[OptionalValue[Optional, *], Value](
        new OptionalValue[Optional, Value] {
          def empty: Expr[Optional] = isOptionOf.empty

          def of(value: Expr[Value]): Expr[Optional] = isOptionOf.of(value)

          def fold[A: Type](optional: Expr[Optional], onNone: Expr[A], onSome: Expr[Value => A]): Expr[A] =
            // Hearth's fold takes an Expr-level function - adapt chimney's function Expr by application.
            isOptionOf.fold(optional)(onNone, value => applyFunctionCompat(onSome, value))

          def getOrElse(optional: Expr[Optional], onNone: Expr[Value]): Expr[Value] =
            isOptionOf.getOrElse(optional)(onNone)

          def orElse(optional: Expr[Optional], optional2: Expr[Optional]): Expr[Optional] =
            isOptionOf.orElse(optional)(optional2)

          override def toString: String =
            s"support provided by Hearth extension IsOption for ${Type.prettyPrint[Optional]}"
        }
      )
  }
}
