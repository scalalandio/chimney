package io.scalaland.chimney.protobufs

import io.scalaland.chimney.integrations.DefaultValue

/** @since 1.2.0 */
trait ProtobufsDefaultValuesImplicits {

  implicit val defaultValueForUnknownFieldSet: DefaultValue[scalapb.UnknownFieldSet] =
    () => scalapb.UnknownFieldSet.empty
}
