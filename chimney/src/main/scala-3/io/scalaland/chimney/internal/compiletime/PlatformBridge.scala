package io.scalaland.chimney.internal.compiletime

import scala.quoted.Quotes

/** Scala 3 entrypoint of the macro cake: concrete macro classes extend this class. It also hosts the Scala 3 overrides
  * of the compat workarounds (see [[MacroCommonsCompat]]).
  */
abstract private[compiletime] class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions {

  import quotes.reflect.*

  // Workaround to contain @experimental Symbol.freshName from polluting the whole codebase.
  private lazy val freshName = quotes.reflect.Symbol.getClass.getMethod("freshName", classOf[String])
  private def freshTerm(prefix: String): String =
    freshName.invoke(quotes.reflect.Symbol, prefix).asInstanceOf[String]

  private def annotatedValExpr[A: Type](annotation: Term, namePrefix: String)(expr: Expr[A]): Expr[A] = {
    val name = Symbol.newVal(
      Symbol.spliceOwner,
      freshTerm(namePrefix),
      AnnotatedType(TypeRepr.of[A], annotation),
      Flags.EmptyFlags,
      Symbol.noSymbol
    )

    Block(
      List(ValDef(name, Some(expr.asTerm.changeOwner(name)))),
      Ref(name)
    ).asExprOf[A]
  }

  /** Scala 3 override of [[MacroCommonsCompat.cacheScopeToken]]: the ACTIVE Cross-Quotes `Quotes`. Each `Expr.splice`
    * evaluates its thunks under a fresh nested `Quotes`, so values a `TypeCache` materializes during one splice are
    * never handed out during another (the cross-quotes usage contract; Iso/Codec derive two instances - two sibling
    * splices - per expansion).
    */
  override protected def cacheScopeToken: AnyRef = CrossQuotes.ctx[scala.quoted.Quotes]

  /** Hearth has no annotation-attaching API - the `AnnotatedType`-based implementation lives here (see
    * [[MacroCommonsCompat.nowarnExpr]]).
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[scala.annotation.nowarn].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "nowarnResult")(expr)
  }

  /** See [[nowarnExpr]]. */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[java.lang.SuppressWarnings].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "suppressWarningsResult")(expr)
  }
}
