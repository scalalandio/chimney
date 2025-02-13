package io.scalaland.chimney.protobufs

import io.scalaland.chimney.Transformer
// format: off
import io.scalaland.chimney.integrations._
// format: on

// format: off
import scala.collection.compat._
// format: on
import scala.collection.mutable

/** @since 0.8.0 */
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

  // com.google.protobuf.timestamp.Timestamp

  /** @since 0.8.0 */
  implicit val totalTransformerFromTimestampToJavaInstantInstance
      : Transformer[com.google.protobuf.timestamp.Timestamp, java.time.Instant] =
    timestamp => java.time.Instant.ofEpochSecond(timestamp.seconds, timestamp.nanos.toLong)

  /** @since 0.8.0 */
  implicit val totalTransformerFromJavaInstantToTimestampInstance
      : Transformer[java.time.Instant, com.google.protobuf.timestamp.Timestamp] =
    instant => com.google.protobuf.timestamp.Timestamp.of(instant.getEpochSecond, instant.getNano)

  // com.google.protobuf.ByteString

  implicit val protobufByteStringIsTotallyBuildIterable: TotallyBuildIterable[com.google.protobuf.ByteString, Byte] =
    new TotallyBuildIterable[com.google.protobuf.ByteString, Byte] {
      def totalFactory: Factory[Byte, com.google.protobuf.ByteString] =
        new FactoryCompat[Byte, com.google.protobuf.ByteString] {
          def newBuilder: mutable.Builder[Byte, com.google.protobuf.ByteString] =
            new FactoryCompat.Builder[Byte, com.google.protobuf.ByteString] {
              private val impl = mutable.ListBuffer.empty[Byte]
              def clear(): Unit = impl.clear()
              def result(): com.google.protobuf.ByteString =
                com.google.protobuf.ByteString.copyFrom(impl.iterator.toArray)
              def addOne(elem: Byte): this.type = { impl += elem; this }
            }
        }
      def iterator(byteString: com.google.protobuf.ByteString): Iterator[Byte] = byteString.toByteArray().iterator
    }

  // com.google.protobuf.wrappers.BoolValue

  /** @since 0.8.0 */
  implicit val totalTransformerFromBoolValueToBoolean: Transformer[com.google.protobuf.wrappers.BoolValue, Boolean] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromBooleanToBoolValue: Transformer[Boolean, com.google.protobuf.wrappers.BoolValue] =
    bool => com.google.protobuf.wrappers.BoolValue.of(bool)

  // com.google.protobuf.wrappers.BytesValue

  implicit val protobufBytesValueIsTotallyBuildIterable
      : TotallyBuildIterable[com.google.protobuf.wrappers.BytesValue, Byte] =
    new TotallyBuildIterable[com.google.protobuf.wrappers.BytesValue, Byte] {
      def totalFactory: Factory[Byte, com.google.protobuf.wrappers.BytesValue] =
        new FactoryCompat[Byte, com.google.protobuf.wrappers.BytesValue] {
          def newBuilder: mutable.Builder[Byte, com.google.protobuf.wrappers.BytesValue] =
            new FactoryCompat.Builder[Byte, com.google.protobuf.wrappers.BytesValue] {
              private val impl = protobufByteStringIsTotallyBuildIterable.totalFactory.newBuilder
              def clear(): Unit = impl.clear()
              def result(): com.google.protobuf.wrappers.BytesValue =
                com.google.protobuf.wrappers.BytesValue.of(impl.result())
              def addOne(elem: Byte): this.type = { impl.addOne(elem); this }
            }
        }
      def iterator(byteValue: com.google.protobuf.wrappers.BytesValue): Iterator[Byte] =
        protobufByteStringIsTotallyBuildIterable.iterator(byteValue.value)
    }

  // com.google.protobuf.wrappers.DoubleValue

  /** @since 0.8.0 */
  implicit val totalTransformerFromDoubleValueToDouble: Transformer[com.google.protobuf.wrappers.DoubleValue, Double] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromDoubleToDoubleValue: Transformer[Double, com.google.protobuf.wrappers.DoubleValue] =
    double => com.google.protobuf.wrappers.DoubleValue.of(double)

  // com.google.protobuf.wrappers.FloatValue

  /** @since 0.8.0 */
  implicit val totalTransformerFromFloatValueToFloat: Transformer[com.google.protobuf.wrappers.FloatValue, Float] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromFloatToFloatValue: Transformer[Float, com.google.protobuf.wrappers.FloatValue] =
    float => com.google.protobuf.wrappers.FloatValue.of(float)

  // com.google.protobuf.wrappers.Int32Value

  /** @since 0.8.0 */
  implicit val totalTransformerFromInt32ValueToInt: Transformer[com.google.protobuf.wrappers.Int32Value, Int] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromIntToInt32Value: Transformer[Int, com.google.protobuf.wrappers.Int32Value] =
    int => com.google.protobuf.wrappers.Int32Value.of(int)

  // com.google.protobuf.wrappers.Int64Value

  /** @since 0.8.0 */
  implicit val totalTransformerFromInt64ValueToLong: Transformer[com.google.protobuf.wrappers.Int64Value, Long] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromLongToInt64Value: Transformer[Long, com.google.protobuf.wrappers.Int64Value] =
    long => com.google.protobuf.wrappers.Int64Value.of(long)

  // com.google.protobuf.wrappers.UInt32Value

  /** @since 0.8.0 */
  implicit val totalTransformerFromUInt32ValueToInt: Transformer[com.google.protobuf.wrappers.UInt32Value, Int] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromIntToUInt32Value: Transformer[Int, com.google.protobuf.wrappers.UInt32Value] =
    int => com.google.protobuf.wrappers.UInt32Value.of(int)

  // com.google.protobuf.wrappers.Int64Value

  /** @since 0.8.0 */
  implicit val totalTransformerFromUInt64ValueToLong: Transformer[com.google.protobuf.wrappers.UInt64Value, Long] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromLongToUInt64Value: Transformer[Long, com.google.protobuf.wrappers.UInt64Value] =
    long => com.google.protobuf.wrappers.UInt64Value.of(long)

  // com.google.protobuf.wrappers.StringValue

  /** @since 0.8.0 */
  implicit val totalTransformerFromStringValueToString: Transformer[com.google.protobuf.wrappers.StringValue, String] =
    wrapper => wrapper.value

  /** @since 0.8.0 */
  implicit val totalTransformerFromStringToStringValue: Transformer[String, com.google.protobuf.wrappers.StringValue] =
    string => com.google.protobuf.wrappers.StringValue.of(string)
}
