package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.reflect.macros.blackbox

final class Macro(val c: blackbox.Context) extends DefinitionsPlatform with DerivationPlatform with GatewayPlatform
