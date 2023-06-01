package io.scalaland.chimney.internal.compiletime.derivation.transformer

private[derivation] trait ImplicitSummoningPlatform { this: DerivationPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final protected def isAutoderivedFromTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.Transformer[From, To]]
  ): Boolean = expr.tree match {
    case TypeApply(Select(qualifier, name), _) =>
      qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.Transformer.type] && name.toString == "derive"
    case _ =>
      false
  }

  final protected def isAutoderivedFromPartialTransformerDerive[From: Type, To: Type](
      expr: Expr[io.scalaland.chimney.PartialTransformer[From, To]]
  ): Boolean = expr.tree match {
    case TypeApply(Select(qualifier, name), _) =>
      qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.PartialTransformer.type] && name.toString == "derive"
    case _ =>
      false
  }
}
