package io.scalaland.chimney.internal.compiletime

/** Foundation of the derivation cake: `hearth.MacroCommons` (mixed in by the platform bridges) plus the Chimney modules
  * below. The self-type includes `hearth.std.StdExtensions` so that [[datatypes.StdExtensionsLoading]] and
  * `IsOption`/`IsEither`/... call sites type-check.
  */
private[compiletime] trait ChimneyDefinitions
    extends MacroCommonsCompat
    with ResultSyntax
    with ChimneyTypes
    with ChimneyExprs
    with CtorLikeExprs
    with datatypes.ProductTypes
    with datatypes.SealedHierarchies
    with datatypes.ValueClasses
    with datatypes.SingletonTypes
    with datatypes.StdExtensionsLoading {
  this: hearth.MacroCommons & hearth.std.StdExtensions =>

  // $COVERAGE-OFF$It's testable in (Scala-CLI) snippets and not really in normal tests with coverage
  implicit protected class FlagOps(sc: StringContext) {

    /** Usage
      * {{{
      * "chimney.SuppressWarnings=none" match {
      *   case chimneyFlag"SuppressWarnings=$value => value // "none"
      * }
      * }}}
      */
    object chimneyFlag {
      def unapplySeq(s: String): Option[Seq[String]] =
        if (s.startsWith(chimneyPrefix)) StringContext.glob(sc.parts, s.drop(chimneyPrefix.length))
        else None
    }

    /** Usage
      * {{{
      * "chimney.transformer.MacrosLogging=false" match {
      *   case transformerFlag"MacrosLogging=$value => value // "false"
      * }
      * }}}
      */
    object transformerFlag {
      def unapplySeq(s: String): Option[Seq[String]] =
        if (s.startsWith(transformerPrefix)) StringContext.glob(sc.parts, s.drop(transformerPrefix.length))
        else None
    }

    /** Usage
      * {{{
      * "chimney.patcher.MacrosLogging=false" match {
      *   case patcherFlag"MacrosLogging=$value => value // "false"
      * }
      * }}}
      */
    object patcherFlag {
      def unapplySeq(s: String): Option[Seq[String]] =
        if (s.startsWith(patcherPrefix)) StringContext.glob(sc.parts, s.drop(patcherPrefix.length))
        else None
    }
  }
  private val chimneyPrefix = "chimney."
  private val transformerPrefix = chimneyPrefix + "transformer."
  private val patcherPrefix = chimneyPrefix + "patcher."
}
