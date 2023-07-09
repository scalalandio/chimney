package io.scalaland.chimney.internal.compiletime.dsl

private[chimney] trait TypeEncodingsPlatform extends TypeEncodings { this: DslDefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import Type.platformSpecific.fromUntyped

  protected def extractSelectorFieldName[A: Type, Field: Type](
      selector: Expr[A => Field]
  ): Option[ExistentialType.UpperBounded[String]] = selector.tree match {
    case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: TermName}" if vd.name == idt.name =>
      Some(fromUntyped[String](c.internal.constantType(Constant(fieldName.decodedName.toString))).as_?<[String])
    case _ => None
  }
}
