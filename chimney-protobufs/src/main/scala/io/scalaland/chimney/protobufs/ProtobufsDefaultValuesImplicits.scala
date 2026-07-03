package io.scalaland.chimney.protobufs

import io.scalaland.chimney.integrations.DefaultValue

/** Kept as an implicit: `DefaultValue` is a Chimney `integrations` typeclass consumed by the ProductToProduct rule for
  * absent constructor arguments - Hearth std extensions have no equivalent hook, so this cannot be a provider (see
  * [[io.scalaland.chimney.protobufs.internal.compiletime.ProtobufsMacroExtension]]).
  *
  * @since 1.2.0
  */
trait ProtobufsDefaultValuesImplicits {

  implicit val defaultValueForUnknownFieldSet: DefaultValue[scalapb.UnknownFieldSet] =
    () => scalapb.UnknownFieldSet.empty
}
