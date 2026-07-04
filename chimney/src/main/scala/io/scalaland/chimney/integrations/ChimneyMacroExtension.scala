package io.scalaland.chimney.integrations

import io.scalaland.chimney.internal.compiletime.derivation.transformer.ChimneyEngineExtensionApi

/** Chimney's engine-aware macro-extension SPI.
  *
  * Integration authors extend this class (and register it via `META-INF/services/`[[ChimneyMacroExtension]]) to add
  * PAIR-SPECIFIC transformations that need access to the derivation engine - including recursive derivation of inner
  * values. It is loaded reflectively (`java.util.ServiceLoader`) at macro-expansion time, reusing Hearth's
  * [[hearth.MacroExtension]]/loader machinery, but with a Chimney-specific engine surface
  * ([[ChimneyEngineExtensionApi]]) rather than Hearth's `StdExtensions`.
  *
  * This is the right mechanism when a conversion:
  *   - is specific to a PAIR of types (not a shape like `IsCollection`/`IsValueType` that Hearth std extensions cover),
  *   - may need to transform the OUTER layer itself while DEFERRING inner values back to Chimney's mechanics (via
  *     `deriveInner`, which supports N inner derivations), and/or
  *   - has total/partial asymmetry or multiple conversion partners per type (which `IsValueType`, allowing exactly one
  *     inner type, cannot express).
  *
  * Example (registering a handler in `extend`):
  * {{{
  * final class MyExtension extends ChimneyMacroExtension {
  *   def extend(ctx: hearth.MacroCommons & ChimneyEngineExtensionApi): Unit = {
  *     import ctx.*
  *     registerSpecialCase(new SpecialCaseHandler {
  *       def apply[From, To](implicit From: Type[From], To: Type[To]): Option[SpecialCasedTransformation[From, To]] =
  *         if (From =:= Type.of[my.Outer] && To =:= Type.of[my.Target])
  *           Some(handler.asInstanceOf[SpecialCasedTransformation[From, To]])
  *         else None
  *     })
  *   }
  * }
  * }}}
  *
  * Precedence: the rule that consults these handlers sits BELOW the four implicit rules (`Transformer`/
  * `PartialTransformer`/`TotalOuterTransformer`/`PartialOuterTransformer` implicits, incl.
  * `io.scalaland.chimney.integrations` ones) and ABOVE Chimney's built-in structural rules - so a user implicit
  * `Transformer` for the same pair still wins.
  *
  * NOTE: currently `private[chimney]` (usable by IN-TREE integration modules). Promoting to fully public API is a
  * planned follow-up once the shape is signed off.
  */
abstract private[chimney] class ChimneyMacroExtension
    extends hearth.MacroExtension[hearth.MacroCommons & ChimneyEngineExtensionApi]
