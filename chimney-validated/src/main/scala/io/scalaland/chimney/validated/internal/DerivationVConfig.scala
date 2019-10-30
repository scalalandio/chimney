package io.scalaland.chimney.validated.internal

import io.scalaland.chimney.internal.DerivationConfig

trait DerivationVConfig { this: DerivationConfig =>
  case class VConfig(underlying: Config = Config(), overridenVFields: Set[String] = Set.empty) {
    def rec: VConfig =
      VConfig(underlying.rec)
  }
}
