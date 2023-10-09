package io.scalaland.chimney.protobufs

import io.scalaland.chimney.{partial, PartialTransformer}

/** @since 0.8.0 */
trait ProtobufsPartialTransformerImplicits extends ProtobufsPartialTransformerImplicitsLowPriorityImplicits1 {

  /** @since 0.8.0 */
  implicit def partialTransformerFromEmptyOneOfInstance[From <: scalapb.GeneratedOneof { type ValueType = Nothing }, To]
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
