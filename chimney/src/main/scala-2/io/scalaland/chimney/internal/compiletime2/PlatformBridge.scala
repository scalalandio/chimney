package io.scalaland.chimney.internal.compiletime2

/** Scala 2 entrypoint of the Hearth-based macro cake.
  *
  * Mirrors the old `DefinitionsPlatform`/`DerivationPlatform` split: concrete macro bundles will extend this class the
  * same way they extended `DefinitionsPlatform(c)` before. Its main purpose right now is to prove that the whole
  * `compiletime2` cake composes and compiles on Scala 2.
  */
abstract private[compiletime2] class PlatformBridge(val c: scala.reflect.macros.blackbox.Context)
    extends hearth.MacroCommonsScala2
    with ChimneyDefinitions {

  /** On Scala 2 the shared implementation would leave the wildcard example's existentially-quantified symbols unbound
    * after re-application - re-quantify them here (see [[MacroCommonsCompat.reapplyLeadingTypeArgsCompat]]).
    */
  override protected def reapplyLeadingTypeArgsCompat(
      wildcardExample: UntypedType,
      leading: List[UntypedType]
  ): UntypedType = {
    import c.universe.*
    val dealiased = wildcardExample.dealias
    val (quantified, underlying) = dealiased match {
      case ExistentialType(qs, u) => (qs, u)
      case other                  => (Nil, other)
    }
    val applied = appliedType(underlying.typeConstructor, leading ++ underlying.typeArgs.drop(leading.size))
    internal.existentialAbstraction(quantified, applied)
  }

  /** macro-commons `Expr.nowarn` (Scala 2) - Hearth has no annotation-attaching API, so the old quasiquote-based
    * implementation lives here (see [[MacroCommonsCompat.nowarnExpr]]).
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    import c.universe.*
    val name = c.internal.reificationSupport.freshTermName("nowarnresult$macro$")
    c.Expr[A](
      warnings.fold(
        q"""
        @ _root_.scala.annotation.nowarn
        val $name = $expr
        $name
        """
      ) { msg =>
        q"""
        @ _root_.scala.annotation.nowarn($msg)
        val $name = $expr
        $name
        """
      }
    )
  }

  /** macro-commons `Expr.SuppressWarnings` (Scala 2) - see [[nowarnExpr]]. */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    import c.universe.*
    val name = c.internal.reificationSupport.freshTermName("suppresswarningsresult$macro$")
    c.Expr[A](
      q"""
      @ _root_.java.lang.SuppressWarnings(_root_.scala.Array(..$warnings))
      val $name = $expr
      $name
      """
    )
  }
}
