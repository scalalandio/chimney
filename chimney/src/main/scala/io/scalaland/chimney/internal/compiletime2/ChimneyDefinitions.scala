package io.scalaland.chimney.internal.compiletime2

/** Hearth-based port of `io.scalaland.chimney.internal.compiletime.ChimneyDefinitions`.
  *
  * Instead of macro-commons' `Definitions` the cake builds on `hearth.MacroCommons` (mixed in by the platform bridges)
  * plus [[MacroCommonsCompat]] for the shims that have no direct Hearth counterpart.
  *
  * The self-type includes `hearth.std.StdExtensions` (mixed into `MacroCommonsScala2`/`MacroCommonsScala3` by Hearth
  * itself) so that [[datatypes.StdExtensionsLoading]] and future `IsOption`/`IsEither`/... call sites type-check.
  */
private[compiletime2] trait ChimneyDefinitions
    extends MacroCommonsCompat
    with ResultSyntax
    with ChimneyTypes
    with ChimneyExprs
    with datatypes.ProductTypes
    with datatypes.SealedHierarchies
    with datatypes.ValueClasses
    with datatypes.IterableOrArrays
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
