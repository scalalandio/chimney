package io.scalaland.chimney.protobufs.internal.runtime

/** Runtime helpers for [[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsMacroExtension]]'s quotes.
  *
  * Why this object exists (the "runtime helper pattern" from the Kindlings `hearth-collection-map` skill, plus the
  * Scala 2 gotcha discovered in the engine-test-extension work): a cross-quote living in a SEPARATELY COMPILED module
  * that references a companion object of a class (e.g. `BoolValue.of(...)`) produces a tree whose qualifier symbol
  * resolves to the CLASS instead of the MODULE at the downstream macro-expansion site on Scala 2. An object WITHOUT a
  * companion class (like this one), referenced FULLY QUALIFIED inside the quote, is immune. So every companion/static
  * call the providers need is wrapped here, and the quotes only ever call these helpers.
  *
  * NOT intended to be called by user code - it is `public` only because the generated code has to reference it.
  */
object ProtobufsConversions {

  // com.google.protobuf.ByteString (collection of Byte)

  def byteStringAsIterable(byteString: com.google.protobuf.ByteString): Iterable[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(byteString.toByteArray()) // fresh array, safe to wrap

  def byteStringFactory: scala.collection.Factory[Byte, com.google.protobuf.ByteString] =
    new scala.collection.Factory[Byte, com.google.protobuf.ByteString] {
      def fromSpecific(it: IterableOnce[Byte]): com.google.protobuf.ByteString =
        com.google.protobuf.ByteString.copyFrom(it.iterator.toArray)
      def newBuilder: scala.collection.mutable.Builder[Byte, com.google.protobuf.ByteString] =
        scala.Array.newBuilder[Byte].mapResult(array => com.google.protobuf.ByteString.copyFrom(array))
    }

  // com.google.protobuf.wrappers.*Value (value types wrapping primitives/String/ByteString)

  def wrapBoolValue(value: Boolean): com.google.protobuf.wrappers.BoolValue =
    com.google.protobuf.wrappers.BoolValue.of(value)

  def wrapBytesValue(value: com.google.protobuf.ByteString): com.google.protobuf.wrappers.BytesValue =
    com.google.protobuf.wrappers.BytesValue.of(value)

  def wrapDoubleValue(value: Double): com.google.protobuf.wrappers.DoubleValue =
    com.google.protobuf.wrappers.DoubleValue.of(value)

  def wrapFloatValue(value: Float): com.google.protobuf.wrappers.FloatValue =
    com.google.protobuf.wrappers.FloatValue.of(value)

  def wrapInt32Value(value: Int): com.google.protobuf.wrappers.Int32Value =
    com.google.protobuf.wrappers.Int32Value.of(value)

  def wrapInt64Value(value: Long): com.google.protobuf.wrappers.Int64Value =
    com.google.protobuf.wrappers.Int64Value.of(value)

  def wrapUInt32Value(value: Int): com.google.protobuf.wrappers.UInt32Value =
    com.google.protobuf.wrappers.UInt32Value.of(value)

  def wrapUInt64Value(value: Long): com.google.protobuf.wrappers.UInt64Value =
    com.google.protobuf.wrappers.UInt64Value.of(value)

  def wrapStringValue(value: String): com.google.protobuf.wrappers.StringValue =
    com.google.protobuf.wrappers.StringValue.of(value)

  // Unwrapping goes through helpers too, so that the quotes stay trivial method calls on this object.

  def unwrapBoolValue(wrapper: com.google.protobuf.wrappers.BoolValue): Boolean = wrapper.value
  def unwrapBytesValue(wrapper: com.google.protobuf.wrappers.BytesValue): com.google.protobuf.ByteString =
    wrapper.value
  def unwrapDoubleValue(wrapper: com.google.protobuf.wrappers.DoubleValue): Double = wrapper.value
  def unwrapFloatValue(wrapper: com.google.protobuf.wrappers.FloatValue): Float = wrapper.value
  def unwrapInt32Value(wrapper: com.google.protobuf.wrappers.Int32Value): Int = wrapper.value
  def unwrapInt64Value(wrapper: com.google.protobuf.wrappers.Int64Value): Long = wrapper.value
  def unwrapUInt32Value(wrapper: com.google.protobuf.wrappers.UInt32Value): Int = wrapper.value
  def unwrapUInt64Value(wrapper: com.google.protobuf.wrappers.UInt64Value): Long = wrapper.value
  def unwrapStringValue(wrapper: com.google.protobuf.wrappers.StringValue): String = wrapper.value

  // com.google.protobuf.timestamp.Timestamp (value type whose "inner type" is the COMPUTED java.time.Instant
  // conversion - Hearth's IsValueType contract only requires unwrap to be an Expr function, not a field access)

  def timestampToInstant(timestamp: com.google.protobuf.timestamp.Timestamp): java.time.Instant =
    java.time.Instant.ofEpochSecond(timestamp.seconds, timestamp.nanos.toLong)

  def instantToTimestamp(instant: java.time.Instant): com.google.protobuf.timestamp.Timestamp =
    com.google.protobuf.timestamp.Timestamp.of(instant.getEpochSecond, instant.getNano)
}
