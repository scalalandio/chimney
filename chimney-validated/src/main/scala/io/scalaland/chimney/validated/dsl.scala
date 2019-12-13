package io.scalaland.chimney.validated

import cats.data.ValidatedNec
import io.scalaland.chimney.internal._
import io.scalaland.chimney.validated.internal.{ChimneyVBlackboxMacros, ChimneyVWhiteboxMacros, VCfg, VEmpty}

import scala.language.experimental.macros

object dsl {
  implicit class VTransformerOps[From](val source: From) extends AnyVal {
    final def intoV[To]: TransformerVInto[From, To, Empty, VEmpty] =
      new TransformerVInto[From, To, Empty, VEmpty](source, Map.empty, Map.empty, Map.empty)

    final def tryTransformInto[To](implicit T: VTransformer[From, To]): ValidatedNec[VTransformer.Error, To] =
      T.transform(source)
  }

  final class TransformerVInto[From, To, C <: Cfg, VC <: VCfg](val source: From,
                                                               val overrides: Map[String, Any],
                                                               val instances: Map[(String, String), Any],
                                                               val overridesV: Map[String, Any]) {

    def disableDefaultValues: TransformerVInto[From, To, DisableDefaultValues[C], VC] =
      new TransformerVInto[From, To, DisableDefaultValues[C], VC](source, overrides, instances, overridesV)

    def enableBeanGetters: TransformerVInto[From, To, EnableBeanGetters[C], VC] =
      new TransformerVInto[From, To, EnableBeanGetters[C], VC](source, overrides, instances, overridesV)

    def enableBeanSetters: TransformerVInto[From, To, EnableBeanSetters[C], VC] =
      new TransformerVInto[From, To, EnableBeanSetters[C], VC](source, overrides, instances, overridesV)

    def enableOptionDefaultsToNone: TransformerVInto[From, To, EnableOptionDefaultsToNone[C], VC] =
      new TransformerVInto[From, To, EnableOptionDefaultsToNone[C], VC](source, overrides, instances, overridesV)

    def withFieldConst[T, U](selector: To => T, value: U): TransformerVInto[From, To, _, VC] =
      macro ChimneyVWhiteboxMacros.withFieldConstImpl[From, To, T, U, C, VC]

    def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerVInto[From, To, _, VC] =
      macro ChimneyVWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C, VC]

    def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerVInto[From, To, _, VC] =
      macro ChimneyVWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C, VC]

    def withFieldConstV[T, U](selector: To => T,
                              value: ValidatedNec[VTransformer.Error, U]): TransformerVInto[From, To, C, _] =
      macro ChimneyVWhiteboxMacros.withFieldConstVImpl[From, To, T, U, C, VC]

    def withFieldComputedV[T, U](selector: To => T,
                                 map: From => ValidatedNec[VTransformer.Error, U]): TransformerVInto[From, To, C, _] =
      macro ChimneyVWhiteboxMacros.withFieldComputedVImpl[From, To, T, U, C, VC]

    def tryTransform: ValidatedNec[VTransformer.Error, To] =
      macro ChimneyVBlackboxMacros.transformVImpl[From, To, C, VC]
  }
}
