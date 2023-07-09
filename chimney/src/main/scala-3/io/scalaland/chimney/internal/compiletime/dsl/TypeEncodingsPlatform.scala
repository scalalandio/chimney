package io.scalaland.chimney.internal.compiletime.dsl

private[chimney] trait TypeEncodingsPlatform extends TypeEncodings { this: DslDefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  @scala.annotation.nowarn(
    "msg=the type test for TypeEncodingsPlatform.this.quotes.reflect.ValDef cannot be checked at runtime because it refers to an abstract type member or type parameter"
  )
  protected def extractSelectorFieldName[A: Type, Field: Type](
      selector: Expr[A => Field]
  ): Option[ExistentialType.UpperBounded[String]] = {
    object SelectLike {
      def unapply(term: Term): Option[(String, String)] =
        term match {
          case Select(Ident(out), va)              => Some(out, va)
          case Block(_, SelectLike(ident, member)) => Some(ident, member)
          case _                                   => None
        }
    }

    def helper(selectorTerm: Term): Option[String] = selectorTerm match
      case Inlined(
            _,
            _,
            Block(
              List(DefDef(_, List(List(ValDef(in, _, _))), _, Some(SelectLike(out, va)))),
              _ // closure (necessary?)
            )
          ) if in == out =>
        Some(va)
      case Inlined(_, _, block) => helper(block)
      case _                    => None

    helper(selector.asTerm).map { name =>
      ConstantType(StringConstant(name)).asType.asInstanceOf[Type[String]].as_?<[String]
    }
  }
}
