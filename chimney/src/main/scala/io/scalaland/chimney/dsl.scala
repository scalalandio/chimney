package io.scalaland.chimney

import io.scalaland.chimney.internal._

import scala.language.experimental.macros

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, Empty] =
      new TransformerInto[From, To, Empty](source, Map.empty, Map.empty)

    final def defineInto[To]: TransformerInto[From, To, EnableOptionDefaultsToNone[Empty]] =
      new TransformerInto[From, To, EnableOptionDefaultsToNone[Empty]](source, Map.empty, Map.empty)

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  final class TransformerInto[From, To, C <: Cfg](val source: From,
                                                  val overrides: Map[String, Any],
                                                  val instances: Map[(String, String), Any]) {

    def disableDefaultValues: TransformerInto[From, To, DisableDefaultValues[C]] =
      new TransformerInto[From, To, DisableDefaultValues[C]](source, overrides, instances)

    def enableBeanGetters: TransformerInto[From, To, EnableBeanGetters[C]] =
      new TransformerInto[From, To, EnableBeanGetters[C]](source, overrides, instances)

    def enableBeanSetters: TransformerInto[From, To, EnableBeanSetters[C]] =
      new TransformerInto[From, To, EnableBeanSetters[C]](source, overrides, instances)

    def enableOptionDefaultsToNone: TransformerInto[From, To, EnableOptionDefaultsToNone[C]] =
      new TransformerInto[From, To, EnableOptionDefaultsToNone[C]](source, overrides, instances)

    def disableLocalImplicitLookup: TransformerInto[From, To, DisableLocalImplicitLookup[C]] =
      new TransformerInto[From, To, DisableLocalImplicitLookup[C]](source, overrides, instances)

    def withFieldConst[T, U](selector: To => T, value: U): TransformerInto[From, To, _] =
      macro ChimneyWhiteboxMacros.withFieldConstImpl[From, To, T, U, C]

    def withFieldComputed[T, U](selector: To => T, map: From => U): TransformerInto[From, To, _] =
      macro ChimneyWhiteboxMacros.withFieldComputedImpl[From, To, T, U, C]

    def withFieldRenamed[T, U](selectorFrom: From => T, selectorTo: To => U): TransformerInto[From, To, _] =
      macro ChimneyWhiteboxMacros.withFieldRenamedImpl[From, To, T, U, C]

    def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _] =
      macro ChimneyWhiteboxMacros.withCoproductInstanceImpl[From, To, Inst, C]

    def transform: To =
      macro ChimneyBlackboxMacros.transformImpl[From, To, C]
  }

  implicit class PatcherOps[T](val obj: T) extends AnyVal {

    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)
  }
}
