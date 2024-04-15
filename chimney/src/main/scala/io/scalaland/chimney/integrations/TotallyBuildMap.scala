package io.scalaland.chimney.integrations

trait TotallyBuildMap[Mapp, Key, Value] extends TotallyBuildIterable[Mapp, (Key, Value)]
