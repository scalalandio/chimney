package io.scalaland.chimney.protobufs

/** Since 2.0.0 this trait contains NO implicits: everything it used to provide is now covered WITHOUT any import by two
  * ServiceLoader-registered extensions shipped in this jar:
  *
  *   - Hearth `StandardMacroExtension`
  *     ([[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsMacroExtension]]) for the shape-expressible
  *     cases: `com.google.protobuf.ByteString` <-> collections of `Byte` (`IsCollection`),
  *     `com.google.protobuf.wrappers.*Value` <-> their unwrapped values and `com.google.protobuf.timestamp.Timestamp`
  *     <-> `java.time.Instant` (`IsValueType`),
  *   - Chimney `ChimneyMacroExtension`
  *     ([[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsChimneyMacroExtension]]) for the engine-aware,
  *     pair-specific cases that `IsValueType` (one inner type per outer type) could not express:
  *     `com.google.protobuf.duration.Duration` <-> `java.time.Duration` / `scala.concurrent.duration.FiniteDuration` /
  *     `scala.concurrent.duration.Duration` (with the total/partial asymmetry on the last one) and
  *     `com.google.protobuf.empty.Empty` <-> `Unit` / any type / case objects.
  *
  * What still needs implicits lives in [[ProtobufsPartialTransformerImplicits]] (the empty-oneof/`UnrecognizedEnum`
  * partial instances, which match a BOUNDED `From` type family for ANY `To`).
  */
trait ProtobufsTransformerImplicits {}
