package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ResultDefinitionsPlatform extends ResultDefinitions { this: DefinitionsPlatform =>

  final override protected def reportOrReturn[A](context: Context, value: DerivationResult.Value[A]): A = ???
}
