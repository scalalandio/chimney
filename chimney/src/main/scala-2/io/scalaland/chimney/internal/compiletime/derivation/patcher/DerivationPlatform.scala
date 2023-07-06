package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.internal.compiletime.derivation.transformer

trait DerivationPlatform extends Derivation with transformer.DerivationPlatform
