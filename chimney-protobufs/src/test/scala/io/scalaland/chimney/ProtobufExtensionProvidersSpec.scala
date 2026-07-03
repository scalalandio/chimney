package io.scalaland.chimney

// format: off
import io.scalaland.chimney.dsl._
// format: on

import scala.collection.immutable.ArraySeq

/** Proves that the Hearth `StandardMacroExtension` shipped INSIDE the chimney-protobufs jar
  * ([[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsMacroExtension]], registered via
  * `META-INF/services`) serves the conversions that used to be implicits - note that this file has NO
  * `import io.scalaland.chimney.protobufs.*` at all:
  *
  *   - `ByteString` <-> collections of `Byte` (`IsCollection` provider),
  *   - `wrappers.*Value` <-> unwrapped values (`IsValueType` providers, no `nonAnyValWrappers` flag needed -
  *     extension-provided value types skip it by design),
  *   - `Timestamp` <-> `java.time.Instant` (`IsValueType` provider with a COMPUTED inner-type conversion),
  *   - TRANSITIVELY: collections of `Byte` <-> `BytesValue` (TypeToValueClass/ValueClassToType over the `BytesValue`
  *     value type composing with the `ByteString` collection),
  *
  * that the documented precedence holds (user implicits and `integrations` implicits both OVERRIDE the providers), and
  * that the deliberately-kept implicits (`Duration`, `Empty` - see `ProtobufsTransformerImplicits`) really do still
  * require the import (pinned via compileErrors).
  */
class ProtobufExtensionProvidersSpec extends ChimneySpec {

  private val bytes = ArraySeq(0.toByte, 1.toByte, 2.toByte)
  private val byteString = com.google.protobuf.ByteString.copyFrom(bytes.toArray)

  group("without any import: ByteString as a collection of Byte (IsCollection provider)") {

    test("totally transform between Byte collections and ByteString") {
      bytes.transformInto[com.google.protobuf.ByteString] ==> byteString
      bytes.toList.transformInto[com.google.protobuf.ByteString] ==> byteString
      byteString.transformInto[List[Byte]] ==> bytes.toList
      byteString.transformInto[Vector[Byte]] ==> bytes.toVector
    }

    test("partially transform between Byte collections and ByteString") {
      bytes.toList.transformIntoPartial[com.google.protobuf.ByteString].asOption ==> Some(byteString)
      byteString.transformIntoPartial[List[Byte]].asOption ==> Some(bytes.toList)
    }
  }

  group("without any import: wrappers.*Value as value types (IsValueType providers)") {

    test("totally transform into and from every wrapper type (no nonAnyValWrappers flag needed)") {
      true.transformInto[com.google.protobuf.wrappers.BoolValue] ==>
        com.google.protobuf.wrappers.BoolValue.of(true)
      com.google.protobuf.wrappers.BoolValue.of(true).transformInto[Boolean] ==> true

      4.0.transformInto[com.google.protobuf.wrappers.DoubleValue] ==>
        com.google.protobuf.wrappers.DoubleValue.of(4.0)
      com.google.protobuf.wrappers.DoubleValue.of(4.0).transformInto[Double] ==> 4.0

      5.0f.transformInto[com.google.protobuf.wrappers.FloatValue] ==>
        com.google.protobuf.wrappers.FloatValue.of(5.0f)
      com.google.protobuf.wrappers.FloatValue.of(5.0f).transformInto[Float] ==> 5.0f

      10.transformInto[com.google.protobuf.wrappers.Int32Value] ==>
        com.google.protobuf.wrappers.Int32Value.of(10)
      com.google.protobuf.wrappers.Int32Value.of(10).transformInto[Int] ==> 10

      20L.transformInto[com.google.protobuf.wrappers.Int64Value] ==>
        com.google.protobuf.wrappers.Int64Value.of(20L)
      com.google.protobuf.wrappers.Int64Value.of(20L).transformInto[Long] ==> 20L

      100.transformInto[com.google.protobuf.wrappers.UInt32Value] ==>
        com.google.protobuf.wrappers.UInt32Value.of(100)
      com.google.protobuf.wrappers.UInt32Value.of(100).transformInto[Int] ==> 100

      200L.transformInto[com.google.protobuf.wrappers.UInt64Value] ==>
        com.google.protobuf.wrappers.UInt64Value.of(200L)
      com.google.protobuf.wrappers.UInt64Value.of(200L).transformInto[Long] ==> 200L

      "value".transformInto[com.google.protobuf.wrappers.StringValue] ==>
        com.google.protobuf.wrappers.StringValue.of("value")
      com.google.protobuf.wrappers.StringValue.of("value").transformInto[String] ==> "value"

      byteString.transformInto[com.google.protobuf.wrappers.BytesValue] ==>
        com.google.protobuf.wrappers.BytesValue.of(byteString)
      com.google.protobuf.wrappers.BytesValue.of(byteString).transformInto[com.google.protobuf.ByteString] ==>
        byteString
    }

    test("partially transform into and from a wrapper type") {
      true.transformIntoPartial[com.google.protobuf.wrappers.BoolValue].asOption ==> Some(
        com.google.protobuf.wrappers.BoolValue.of(true)
      )
      com.google.protobuf.wrappers.BoolValue.of(true).transformIntoPartial[Boolean].asOption ==> Some(true)
    }

    test("TRANSITIVELY transform Byte collections into and from BytesValue (value type over collection)") {
      // Byte collection -> (IsCollection) -> ByteString -> (IsValueType wrap) -> BytesValue and back:
      // no direct support for the pair exists anywhere - this is TypeToValueClass/ValueClassToType composing with
      // IterableToIterable through the two providers.
      bytes.transformInto[com.google.protobuf.wrappers.BytesValue] ==>
        com.google.protobuf.wrappers.BytesValue.of(byteString)
      bytes.toList.transformInto[com.google.protobuf.wrappers.BytesValue] ==>
        com.google.protobuf.wrappers.BytesValue.of(byteString)
      com.google.protobuf.wrappers.BytesValue.of(byteString).transformInto[List[Byte]] ==> bytes.toList
      com.google.protobuf.wrappers.BytesValue.of(byteString).transformIntoPartial[Vector[Byte]].asOption ==> Some(
        bytes.toVector
      )
    }
  }

  group("without any import: Timestamp <-> java.time.Instant (IsValueType provider, computed inner type)") {

    test("totally transform in both directions") {
      val instant = java.time.Instant.ofEpochSecond(12L, 34L)
      val timestamp = com.google.protobuf.timestamp.Timestamp.of(12L, 34)

      instant.transformInto[com.google.protobuf.timestamp.Timestamp] ==> timestamp
      timestamp.transformInto[java.time.Instant] ==> instant

      instant.transformIntoPartial[com.google.protobuf.timestamp.Timestamp].asOption ==> Some(timestamp)
      timestamp.transformIntoPartial[java.time.Instant].asOption ==> Some(instant)
    }

    test("transform inside Options and products") {
      val instant = java.time.Instant.ofEpochSecond(12L, 34L)
      val timestamp = com.google.protobuf.timestamp.Timestamp.of(12L, 34)

      instant.transformInto[Option[com.google.protobuf.timestamp.Timestamp]] ==> Some(timestamp)
      Option(timestamp).transformIntoPartial[java.time.Instant].asOption ==> Some(instant)
    }
  }

  group("precedence: implicits override the std-extension providers") {

    test("user-provided implicit Transformer overrides the extension-provided value type support") {
      implicit val marker: Transformer[Boolean, com.google.protobuf.wrappers.BoolValue] =
        (bool: Boolean) => com.google.protobuf.wrappers.BoolValue.of(!bool) // marker behavior: negates

      true.transformInto[com.google.protobuf.wrappers.BoolValue] ==>
        com.google.protobuf.wrappers.BoolValue.of(false)
    }

    test("user-provided implicit Transformer overrides the extension-provided collection support") {
      implicit val marker: Transformer[List[Byte], com.google.protobuf.ByteString] =
        (list: List[Byte]) => com.google.protobuf.ByteString.copyFrom(list.reverse.toArray) // marker: reverses

      bytes.toList.transformInto[com.google.protobuf.ByteString] ==>
        com.google.protobuf.ByteString.copyFrom(bytes.reverse.toArray)
    }

    test("integrations-implicit TotallyBuildIterable overrides the extension-provided collection support") {
      import ProtobufExtensionProvidersSpec.ReversingSupport.reversingByteStringFactory

      bytes.toList.transformInto[com.google.protobuf.ByteString] ==>
        com.google.protobuf.ByteString.copyFrom(bytes.reverse.toArray)
    }
  }

  group("kept-implicit boundary: what providers deliberately do NOT cover still needs the import") {

    test("Duration conversions are NOT extension-provided (IsValueType allows only one inner type)") {
      // With `import io.scalaland.chimney.protobufs.*` these derive through the KEPT implicits - see the
      // ProtobufsTransformerImplicits ScalaDoc for why Duration cannot be a provider (three conversion partners,
      // total/partial asymmetry on scala.concurrent.duration.Duration).
      // NOTE: snippets use `._` imports (this module compiles without -Xsource:3 on 2.13) and the type-name checks
      // avoid full prefixes (Scala 2 Hearth shortens them in error messages, Scala 3 keeps them).
      compileErrors(
        """
        import io.scalaland.chimney.dsl._
        com.google.protobuf.duration.Duration.of(10L, 0).transformInto[java.time.Duration]
        """
      ).check("Chimney can't derive transformation from", "Duration")

      compileErrors(
        """
        import io.scalaland.chimney.dsl._
        java.time.Duration.ofSeconds(10L).transformInto[com.google.protobuf.duration.Duration]
        """
      ).check("Chimney can't derive transformation from", "Duration")
    }

    test("Empty conversions are NOT extension-provided (Transformer[A, Empty] works for ANY A)") {
      compileErrors(
        """
        import io.scalaland.chimney.dsl._
        "anything".transformInto[com.google.protobuf.empty.Empty]
        """
      ).check("Chimney can't derive transformation from", "String")
    }
  }
}
object ProtobufExtensionProvidersSpec {

  /** Scoped to a single test - ambient it would override the extension support everywhere in this spec. */
  object ReversingSupport {
    // scalafmt would rewrite `._` to `.*`, but this module compiles without -Xsource:3 (see build.sbt)
    // format: off
    import io.scalaland.chimney.integrations._
    // format: on

    import scala.collection.mutable

    implicit val reversingByteStringFactory: TotallyBuildIterable[com.google.protobuf.ByteString, Byte] =
      new TotallyBuildIterable[com.google.protobuf.ByteString, Byte] {

        def totalFactory: scala.collection.Factory[Byte, com.google.protobuf.ByteString] =
          new scala.collection.Factory[Byte, com.google.protobuf.ByteString] {
            def fromSpecific(it: IterableOnce[Byte]): com.google.protobuf.ByteString =
              newBuilder.addAll(it).result()
            def newBuilder: mutable.Builder[Byte, com.google.protobuf.ByteString] =
              new mutable.Builder[Byte, com.google.protobuf.ByteString] {
                private val impl = mutable.ListBuffer.empty[Byte]
                override def clear(): Unit = impl.clear()
                override def result(): com.google.protobuf.ByteString =
                  com.google.protobuf.ByteString.copyFrom(impl.reverse.toArray) // marker behavior: reverses
                override def addOne(elem: Byte): this.type = { impl += elem; this }
              }
          }

        override def iterator(collection: com.google.protobuf.ByteString): Iterator[Byte] =
          collection.toByteArray().iterator
      }
  }
}
