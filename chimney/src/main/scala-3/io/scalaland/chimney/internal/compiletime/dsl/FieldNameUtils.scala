package io.scalaland.chimney.internal.compiletime.dsl

import scala.annotation.nowarn
import scala.quoted.*

object FieldNameUtils {

  // This is workaround for limitations in quoted pattern matching that doesn't yet allow to express
  // type bounds for type variables. It should not be required any more after SIP-53.
  type StringBounded[A <: String] = A

  def strLiteralType(s: String)(using Quotes) = {
    import quotes.reflect.*
    ConstantType(StringConstant(s))
  }

  def extractSelectorFieldNameOrAbort[T: Type, F: Type](selectorExpr: Expr[T => F])(using quotes: Quotes): String = {
    import quotes.reflect.*
    extractSelectorFieldNameOpt(selectorExpr.asTerm).getOrElse {
      report.errorAndAbort(invalidSelectorErrorMessage(selectorExpr), Position.ofMacroExpansion)
    }
  }

  def extractSelectorFieldNamesOrAbort[T1: Type, T2: Type, F1: Type, F2: Type](
      selector1Expr: Expr[T1 => F1],
      selector2Expr: Expr[T2 => F2]
  )(using quotes: Quotes): (String, String) = {
    import quotes.reflect.*
    val term1 = selector1Expr.asTerm
    val term2 = selector2Expr.asTerm

    (extractSelectorFieldNameOpt(term1), extractSelectorFieldNameOpt(term2)) match {
      case (Some(fieldName1), Some(fieldName2)) =>
        (fieldName1, fieldName2)
      case (None, Some(_)) =>
        report.errorAndAbort(invalidSelectorErrorMessage(selector1Expr), Position.ofMacroExpansion)
      case (Some(_), None) =>
        report.errorAndAbort(invalidSelectorErrorMessage(selector2Expr), Position.ofMacroExpansion)
      case (None, None) =>
        val err1 = invalidSelectorErrorMessage(selector1Expr)
        val err2 = invalidSelectorErrorMessage(selector2Expr)
        report.errorAndAbort(s"Invalid selectors:\n$err1\n$err2", Position.ofMacroExpansion)
    }
  }

  @nowarn(
    "msg=the type test for quotes.reflect.ValDef cannot be checked at runtime because it refers to an abstract type member or type parameter"
  )
  def extractSelectorFieldNameOpt(using quotes: Quotes)(selectorTerm: quotes.reflect.Term): Option[String] = {
    import quotes.reflect.*

    object SelectLike {
      def unapply(term: Term): Option[(String, String)] =
        term match {
          case Select(Ident(out), va)              => Some(out, va)
          case Block(_, SelectLike(ident, member)) => Some(ident, member)
          case _                                   => None
        }
    }

    selectorTerm match
      case Inlined(
            _,
            _,
            Block(
              List(DefDef(_, List(List(ValDef(in, _, _))), _, Some(SelectLike(out, va)))),
              _ // closure (necessary?)
            )
          ) if in == out =>
        Some(va)
      case Inlined(_, _, block) => extractSelectorFieldNameOpt(block)
      case _                    => None
  }

  private def invalidSelectorErrorMessage[T](selectorExpr: Expr[T])(using Quotes): String =
    s"Invalid selector expression: ${selectorExpr.show}"
}
