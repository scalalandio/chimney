package io.scalaland.chimney

import io.scalaland.chimney.dsl.IsoDefinition
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

final case class Iso[From, To](from: Transformer[From, To], to: Transformer[To, From]) extends Iso.AutoDerived[From, To]
object Iso extends IsoCompanionPlatform {

  def define[From, To]
      : IsoDefinition[From, To, TransformerOverrides.Empty, TransformerOverrides.Empty, TransformerFlags.Default] =
    new IsoDefinition(Transformer.define, Transformer.define)

  trait AutoDerived[From, To] {
    val from: Transformer[From, To]
    val to: Transformer[To, From]
  }
  object AutoDerived extends IsoAutoDerivedCompanionPlatform
}
