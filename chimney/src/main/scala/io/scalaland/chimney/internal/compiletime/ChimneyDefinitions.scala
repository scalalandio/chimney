package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ChimneyDefinitions extends Definitions with ChimneyTypes with ChimneyExprs {

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
