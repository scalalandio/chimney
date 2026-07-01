package io.scalaland.chimney.internal.compiletime2.derivation.transformer.integrations

import io.scalaland.chimney.internal.compiletime2.derivation.transformer.Derivation

/** Hearth-based port of `...compiletime.derivation.transformer.integrations.OptionalValues`.
  *
  * Differences vs the old version:
  *   - `Type.Option.unapply` (macro-commons `Ctor1`) becomes a `Type.Ctor1.fromUntyped[Option]` extractor (same
  *     `baseType`-aware matching, see [[datatypes.IterableOrArrays]] for the rationale),
  *   - `buildInOptionSupport`'s `Expr.Option.*` calls (macro-commons expr module) become direct cross-quotes emitting
  *     the same code (`Option.empty[Value]`, `Option(value)`, `.fold(onNone)(onSome)`, `.getOrElse`, `.orElse`),
  *   - it is hoisted into a helper method with regular type params ([[mkBuildInOptionSupport]]) - cross-quotes inside
  *     an existential-importing lambda trip the plugin's workaround-method generation (known 0.4.0 gotcha),
  *   - `upcastToExprOf[B]` becomes Hearth's `upcast[B]` (same compile-time-checked, no-runtime-cast semantics).
  */
trait OptionalValues { this: Derivation & hearth.MacroCommons =>

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

    private lazy val OptionCtor: Type.Ctor1[Option] =
      Type.Ctor1.fromUntyped[Option](Type.Ctor1.of[Option].asUntyped)

    private type Cached[A] = Option[Existential[OptionalValue[A, *]]]
    private val optionalCache = new TypeCache[Cached]
    def parse[Optional](implicit Optional: Type[Optional]): Option[Existential[OptionalValue[Optional, *]]] =
      optionalCache(Optional)(providedSupport[Optional].orElse(buildInOptionSupport[Optional]))
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
  }
}
