package io.scalaland.chimney.inlined

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.runtime.{PatcherCfg, PatcherFlags, TransformerCfg, TransformerFlags}
import io.scalaland.chimney.partial

import scala.util.Try

/** Provides transformer operations on values of any type.
  *
  * @tparam From type of source value
  * @param source wrapped source value
  *
  * @since 0.4.0
  */
extension [From](source: From) {

  /** Allows to customize transformer generation to your target type.
    *
    * @tparam To target type
    * @return [[io.scalaland.chimney.dsl.TransformerInto]]
    *
    * @since 0.1.0
    */
  transparent inline def into[To]: TransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new TransformerInto(source, new TransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore))
}

/** Provides partial transformer operations on values of any type.
  *
  * @tparam From type of source value
  * @param source wrapped source value
  *
  * @since 0.7.0
  */
extension [From](source: From) {

  /** Allows to customize partial transformer generation to your target type.
    *
    * @tparam To target success type
    * @return [[io.scalaland.chimney.dsl.PartialTransformerInto]]
    *
    * @since 0.7.0
    */
  transparent inline def intoPartial[To]
      : PartialTransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerInto(
      source,
      new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)
    )
}

/** Provides patcher operations on values of any type
  *
  * @param obj wrapped object to patch
  * @tparam T type of object to patch
  *
  * @since 0.1.3
  */
extension [T](obj: T) {

  /** Allows to customize patcher generation
    *
    * @tparam P type of patch object
    * @param patch patch object value
    * @return [[io.scalaland.chimney.dsl.PatcherUsing]]
    *
    * @since 0.4.0
    */
  transparent inline def using[P](patch: P): PatcherUsing[T, P, PatcherCfg.Empty, PatcherFlags.Default] =
    new PatcherUsing[T, P, PatcherCfg.Empty, PatcherFlags.Default](obj, patch)
}
