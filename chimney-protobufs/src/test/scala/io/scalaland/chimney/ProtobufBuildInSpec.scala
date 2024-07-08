package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.protobufs._
// format: on

import io.scalaland.chimney.examples.pb
import io.scalaland.chimney.fixtures.wrappers

import scala.collection.compat.immutable.ArraySeq

class ProtobufBuildInSpec extends ChimneySpec {

  group("com.google.protobuf.empty") {
    val protobuf = pb.wrappers
      .WithEmpty(Some(com.google.protobuf.empty.Empty.of()))
    val domain = wrappers.WithEmpty(())

    test("totally transform from Unit") {
      domain.transformInto[pb.wrappers.WithEmpty] ==> protobuf
      domain.transformIntoPartial[pb.wrappers.WithEmpty].asOption ==> Some(protobuf)
    }

    test("partially transform into Unit (with conflict resolution provided)") {
      locally {
        implicit val cfg = TransformerConfiguration.default.enableImplicitConflictResolution(PreferTotalTransformer)
        protobuf.transformIntoPartial[wrappers.WithEmpty].asOption ==> Some(domain)
      }
      locally {
        implicit val cfg = TransformerConfiguration.default.enableImplicitConflictResolution(PreferPartialTransformer)
        protobuf.transformIntoPartial[wrappers.WithEmpty].asOption ==> None
      }
    }
  }

  group("com.google.protobuf.wrappers") {
    val bytes = ArraySeq(0.toByte, 1.toByte)
    val protobuf = pb.wrappers.Wrappers.of(
      Some(com.google.protobuf.wrappers.BoolValue.of(true)),
      Some(
        com.google.protobuf.wrappers.BytesValue
          .of(com.google.protobuf.ByteString.copyFrom(bytes.toArray))
      ),
      Some(com.google.protobuf.wrappers.DoubleValue.of(4.0)),
      Some(com.google.protobuf.wrappers.FloatValue.of(5.0f)),
      Some(com.google.protobuf.wrappers.Int32Value.of(10)),
      Some(com.google.protobuf.wrappers.Int64Value.of(20L)),
      Some(com.google.protobuf.wrappers.UInt32Value.of(100)),
      Some(com.google.protobuf.wrappers.UInt64Value.of(200L)),
      Some(com.google.protobuf.wrappers.StringValue.of("value"))
    )
    val domain = wrappers.Wrappers(
      true,
      bytes,
      4.0,
      5.0f,
      10,
      20L,
      100,
      200L,
      "value"
    )

    test("totally transform from unwrapped values") {
      domain.transformInto[pb.wrappers.Wrappers] ==> protobuf
      domain.transformIntoPartial[pb.wrappers.Wrappers].asOption ==> Some(protobuf)
    }

    test("partially transform into unwrapped value") {
      protobuf.transformIntoPartial[wrappers.Wrappers].asOption ==> Some(domain)
    }
  }

  group("com.google.protobuf.duration/com.google.protobuf.timestamp") {
    val protobuf = pb.wrappers.TimeInstances.of(
      Some(com.google.protobuf.duration.Duration.of(10L, 0)),
      Some(com.google.protobuf.duration.Duration.of(0L, 100)),
      Some(com.google.protobuf.timestamp.Timestamp.of(12L, 34))
    )
    val domain = wrappers.TimeInstances(
      scala.concurrent.duration.Duration.fromNanos(10000000000L),
      scala.concurrent.duration.Duration.fromNanos(100L),
      java.time.Instant.ofEpochSecond(12L, 34L)
    )

    test("partially transform from Duration/Instant") {
      // Duration.Infinite cannot be encoded as PB duration!
      domain.transformIntoPartial[pb.wrappers.TimeInstances].asOption ==> Some(protobuf)
    }

    test("totally transform into Duration/Instant") {
      protobuf.transformIntoPartial[wrappers.TimeInstances].asOption ==> Some(domain)
    }
  }
}
