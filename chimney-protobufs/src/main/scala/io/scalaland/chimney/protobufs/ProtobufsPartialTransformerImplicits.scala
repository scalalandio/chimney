package io.scalaland.chimney.protobufs

import io.scalaland.chimney.{partial, PartialTransformer}

/** Kept as implicits (NOT provided by [[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsMacroExtension]])
  * because none of them is expressible through a std-extension provider:
  *
  *   - the empty `GeneratedOneof`/`SealedOneof` and `UnrecognizedEnum` instances match a BOUNDED `From` for ANY `To` -
  *     they hook the Implicit rule for whole type families, which no `IsValueType`/`IsCollection`/`IsOption` shape can
  *     express,
  *   - `PartialTransformer[Empty, A]` for ANY `A` - same reason,
  *   - `scala.concurrent.duration.Duration` -> proto `Duration` (rejecting `Duration.Infinite`) COULD in isolation be a
  *     smart-constructor `IsValueType` (`EitherStringOrValue` wrap), but `IsValueType` allows only ONE inner type per
  *     outer type and proto `Duration`'s other conversion partners (`java.time.Duration`, `FiniteDuration`) must stay
  *     total implicits anyway - see the [[ProtobufsTransformerImplicits]] ScalaDoc for the full verdict.
  *
  * @since 0.8.0
  */
trait ProtobufsPartialTransformerImplicits extends ProtobufsPartialTransformerImplicitsLowPriorityImplicits1 {

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

private[protobufs] trait ProtobufsPartialTransformerImplicitsLowPriorityImplicits1 {
  this: ProtobufsPartialTransformerImplicits =>

  // com.google.protobuf.empty.Empty

  /** @since 0.8.0 */
  implicit def partialTransformerFromEmptyInstance[A]: PartialTransformer[com.google.protobuf.empty.Empty, A] =
    PartialTransformer(_ => partial.Result.fromEmpty)

  // com.google.protobuf.duration.Duration

  /** @since 0.8.0 */
  implicit val partialTransformerFromScalaDurationToDurationInstance
      : PartialTransformer[scala.concurrent.duration.Duration, com.google.protobuf.duration.Duration] =
    PartialTransformer {
      case _: scala.concurrent.duration.Duration.Infinite =>
        partial.Result.fromErrorString(
          "scala.concurrent.duration.Duration.Infinite cannot be encoded as com.google.protobuf.duration.Duration"
        )
      case duration: scala.concurrent.duration.FiniteDuration =>
        partial.Result.fromValue(totalTransformerFromScalaFiniteDurationToDurationInstance.transform(duration))
    }
}
