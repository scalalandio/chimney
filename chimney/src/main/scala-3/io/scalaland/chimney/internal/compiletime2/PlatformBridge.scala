package io.scalaland.chimney.internal.compiletime2

import scala.quoted.Quotes

/** Scala 3 entrypoint of the Hearth-based macro cake.
  *
  * Mirrors the old `DefinitionsPlatform`/`DerivationPlatform` split: concrete macro classes will extend this class the
  * same way they extended `DefinitionsPlatform(using q)` before. Its main purpose right now is to prove that the whole
  * `compiletime2` cake composes and compiles on Scala 3.
  */
abstract private[compiletime2] class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions {

  import quotes.reflect.*

  // Workaround to contain @experimental Symbol.freshName from polluting the whole codebase (same trick as
  // macro-commons' ExprPromise.platformSpecific.freshTerm).
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

  /** macro-commons `Expr.nowarn` (Scala 3) - Hearth has no annotation-attaching API, so the old `AnnotatedType`-based
    * implementation lives here (see [[MacroCommonsCompat.nowarnExpr]]).
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[scala.annotation.nowarn].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "nowarnResult")(expr)
  }

  /** macro-commons `Expr.SuppressWarnings` (Scala 3) - see [[nowarnExpr]]. */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[java.lang.SuppressWarnings].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "suppressWarningsResult")(expr)
  }
}
