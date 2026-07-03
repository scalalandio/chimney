package io.scalaland.chimney.protobufs

import io.scalaland.chimney.Transformer

/** Since 2.0.0 this trait contains ONLY the implicits that std-extension providers cannot express - everything else was
  * fix-by-deletion replaced by the Hearth `StandardMacroExtension` shipped in this jar (see
  * [[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsMacroExtension]]), which requires NO import at all:
  *
  *   - `com.google.protobuf.ByteString` <-> collections of `Byte` -> `IsCollection` provider (was a
  *     `TotallyBuildIterable` implicit),
  *   - `com.google.protobuf.wrappers.*Value` <-> their unwrapped values -> `IsValueType` providers (were 16
  *     `Transformer` implicits + a `TotallyBuildIterable` for `BytesValue`; `BytesValue`'s inner type is `ByteString`,
  *     which composes transitively with the `ByteString` collection support),
  *   - `com.google.protobuf.timestamp.Timestamp` <-> `java.time.Instant` -> `IsValueType` provider (were 2
  *     `Transformer` implicits; the "inner type" is a computed conversion, which Hearth's contract permits).
  *
  * What stays here and WHY it cannot be a provider:
  *
  *   - `com.google.protobuf.duration.Duration`: `IsValueType` allows exactly ONE inner type per outer type, but proto
  *     `Duration` has THREE conversion partners (`java.time.Duration`, `scala.concurrent.duration.FiniteDuration`,
  *     `scala.concurrent.duration.Duration`) with a total/partial asymmetry on the last one (`Duration.Infinite` cannot
  *     be encoded - see [[ProtobufsPartialTransformerImplicits]]). Whichever partner became the provider's inner type
  *     would orphan the other two as implicits anyway, splitting one type's support across two mechanisms with
  *     confusing precedence - so ALL `Duration` conversions stay implicits,
  *   - `com.google.protobuf.empty.Empty`: `Transformer[A, Empty]` works for ANY `A` - an `IsValueType[Empty]` (inner
  *     `Unit`) could only wrap from types transformable to `Unit`, which is strictly weaker.
  */
trait ProtobufsTransformerImplicits extends ProtobufsTransformerImplicitsLowPriorityImplicits1 {}

private[protobufs] trait ProtobufsTransformerImplicitsLowPriorityImplicits1 { this: ProtobufsTransformerImplicits =>

  // com.google.protobuf.empty.Empty

  /** @since 0.8.0 */
  implicit val totalTransformerFromEmptyToUnitInstance: Transformer[com.google.protobuf.empty.Empty, Unit] = _ => ()

  /** @since 0.8.0 */
  implicit def totalTransformerToEmptyInstance[A]: Transformer[A, com.google.protobuf.empty.Empty] = _ =>
    com.google.protobuf.empty.Empty.of()

  // com.google.protobuf.duration.Duration

  /** @since 0.8.0 */
  implicit val totalTransformerFromDurationToJavaDurationInstance
      : Transformer[com.google.protobuf.duration.Duration, java.time.Duration] =
    duration => java.time.Duration.ofSeconds(duration.seconds, duration.nanos.toLong)

  /** @since 0.8.0 */
  implicit val totalTransformerFromJavaDurationToDurationInstance
      : Transformer[java.time.Duration, com.google.protobuf.duration.Duration] =
    duration => com.google.protobuf.duration.Duration.of(duration.getSeconds, duration.getNano)

  /** @since 0.8.0 */
  implicit val totalTransformerFromDurationToScalaFiniteDurationInstance
      : Transformer[com.google.protobuf.duration.Duration, scala.concurrent.duration.FiniteDuration] =
    duration =>
      scala.concurrent.duration.FiniteDuration(duration.seconds, scala.concurrent.duration.SECONDS) +
        scala.concurrent.duration.FiniteDuration(duration.nanos, scala.concurrent.duration.NANOSECONDS)

  /** @since 0.8.0 */
  implicit val totalTransformerFromScalaFiniteDurationToDurationInstance
      : Transformer[scala.concurrent.duration.FiniteDuration, com.google.protobuf.duration.Duration] = duration => {
    val nanosInSecond = 1000000000
    val seconds = duration.toNanos / nanosInSecond
    val nanos = duration.toNanos - (seconds * nanosInSecond)
    com.google.protobuf.duration.Duration.of(seconds, nanos.toInt)
  }

  /** @since 0.8.0 */
  implicit val totalTransformerFromDurationToScalaDurationInstance
      : Transformer[com.google.protobuf.duration.Duration, scala.concurrent.duration.Duration] =
    totalTransformerFromDurationToScalaFiniteDurationInstance.transform(_) // upcast
}
