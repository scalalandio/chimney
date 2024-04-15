package io.scalaland.chimney.integrations

trait PartiallyBuildMap[Mapp, Key, Value] extends PartiallyBuildIterable[Mapp, (Key, Value)]
