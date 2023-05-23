package io.scalaland.chimney.internal.compiletime.derivation.transformer

import io.scalaland.chimney.internal.compiletime.{DefinitionsPlatform, DerivationResult}

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[derivation] trait DerivationPlatform
    extends Derivation
    with DefinitionsPlatform
    with ImplicitSummoningPlatform
    with rules.TransformImplicitRuleModule
    with rules.TransformSubtypesRuleModule
    with rules.TransformOptionToOptionRuleModule
    with rules.LegacyMacrosFallbackRuleModule {

  import c.universe.{internal as _, Transformer as _, *}

  protected def deriveFold[NewFrom: Type, NewTo: Type, To: Type](
      deriveFromSrc: Expr[NewFrom] => DerivationResult[Rule.ExpansionResult[NewTo]]
  )(
      provideSrcForTotal: (Expr[NewFrom] => Expr[NewTo]) => DerivedExpr[To]
  )(
      provideSrcForPartial: (Expr[NewFrom] => Expr[io.scalaland.chimney.partial.Result[NewTo]]) => DerivedExpr[To]
  ): DerivationResult[Rule.ExpansionResult[To]] = {
    val newFromTermName = freshTermName(Type[NewFrom])
    val newFromExpr = c.Expr[NewFrom](q"$newFromTermName")

    deriveFromSrc(newFromExpr).map {
      case Rule.ExpansionResult.Expanded(DerivedExpr.TotalExpr(total)) =>
        Rule.ExpansionResult.Expanded(
          provideSrcForTotal { newFromValue =>
            c.Expr[NewTo](q"""val $newFromExpr: ${Type[NewFrom]} = $newFromValue; $total""")
          }
        )
      case Rule.ExpansionResult.Expanded(DerivedExpr.PartialExpr(partial)) =>
        Rule.ExpansionResult.Expanded(
          provideSrcForPartial { newFromValue =>
            c.Expr[io.scalaland.chimney.partial.Result[NewTo]](
              q"""val $newFromExpr: ${Type[NewFrom]} = $newFromValue; $partial"""
            )
          }
        )
      case Rule.ExpansionResult.Continue =>
        Rule.ExpansionResult.Continue
    }
  }

  protected object DeferredExprInit extends DeferredExprInitModule {

    // TODO: freshName with extra steps in Scala 2, random in Scala 3 since freshName is experimental
    protected def provideFreshName(): String = ???

    protected def refToTypedName[T](typedName: TypedName[T]): Expr[T] = ???

    // TODO:
    // val $freshName1: $tpe = $expr1
    // ...
    // $value
    protected def initializeVals[To](init: InitializedAsVal[DerivedExpr[To]]): DerivedExpr[To] = ???

    // TODO:
    // { $freshName: $tpe => $value }
    protected def useLambda[From, To, B](param: InitializedAsParam[From, Expr[To]], usage: Expr[From => To] => B): B =
      ???
  }

  final override protected val rulesAvailableForPlatform: List[Rule] =
    List(TransformImplicitRule, TransformSubtypesRule, TransformOptionToOptionRule, LegacyMacrosFallbackRule)

  // TODO: copy pasted from GatewayPlatform, find a better solution

  private def freshTermName(srcPrefixTree: Tree): c.universe.TermName = {
    freshTermName(toFieldName(srcPrefixTree))
  }

  private def freshTermName(tpe: c.Type): c.universe.TermName = {
    freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
  }

  private def freshTermName(prefix: String): c.universe.TermName = {
    c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$")
  }

  private def toFieldName(srcPrefixTree: Tree): String = {
    // undo the encoding of freshTermName
    srcPrefixTree
      .toString()
      .replaceAll("\\$\\d+", "")
      .replace("$u002E", ".")
  }
}
