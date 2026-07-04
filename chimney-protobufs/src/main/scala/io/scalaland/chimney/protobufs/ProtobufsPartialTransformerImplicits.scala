package io.scalaland.chimney.protobufs

import io.scalaland.chimney.{partial, PartialTransformer}

/** Kept as implicits because none of these is expressible through a std-extension provider NOR through a
  * [[io.scalaland.chimney.integrations.ChimneyMacroExtension]] handler (which special-cases a concrete `(From, To)`
  * PAIR): the empty `GeneratedOneof`/`SealedOneof` and `UnrecognizedEnum` instances match a BOUNDED `From` type family
  * for ANY `To`, hooking the Implicit rule for whole type families - one of them
  * ([[partialTransformerFromEmptySealedOneOfInstance]]) is also summoned directly by a spec.
  *
  * The `com.google.protobuf.empty.Empty` and `com.google.protobuf.duration.Duration` conversions that used to live here
  * are, since 2.0.0, provided by [[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsChimneyMacroExtension]]
  * (no import required).
  *
  * @since 0.8.0
  */
trait ProtobufsPartialTransformerImplicits {

  /** @since 0.8.0 */
  implicit def partialTransformerFromEmptyOneOfInstance[From <: scalapb.GeneratedOneof { type ValueType = Nothing }, To]
      : PartialTransformer[From, To] =
    PartialTransformer(_ => partial.Result.fromEmpty)

  /** @since 1.3.0 */
  implicit def partialTransformerFromEmptySealedOneOfInstance[From <: scalapb.GeneratedSealedOneof with Singleton, To]
      : PartialTransformer[From, To] =
    PartialTransformer(_ => partial.Result.fromEmpty)

  /** @since 1.3.0 */
  implicit def partialTransformerFromUnrecognizedEnumInstance[From <: scalapb.UnrecognizedEnum, To]
      : PartialTransformer[From, To] =
    PartialTransformer(_ => partial.Result.fromEmpty)
}
