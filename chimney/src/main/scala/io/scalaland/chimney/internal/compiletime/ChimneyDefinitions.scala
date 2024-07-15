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
        if (s.startsWith(chimneyPrefix)) glob(sc.parts, s.drop(chimneyPrefix.length))
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
        if (s.startsWith(transformerPrefix)) glob(sc.parts, s.drop(transformerPrefix.length))
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
        if (s.startsWith(patcherPrefix)) glob(sc.parts, s.drop(patcherPrefix.length))
        else None
    }
  }
  private val chimneyPrefix = "chimney."
  private val transformerPrefix = chimneyPrefix + "transformer."
  private val patcherPrefix = chimneyPrefix + "patcher."

  // Copy-pasted from scala.StringContext.glob, because 2.12 has no pattern matching on s"...".
  private def glob(patternChunks: Seq[String], input: String): Option[Seq[String]] = {
    var patternIndex = 0
    var inputIndex = 0
    var nextPatternIndex = 0
    var nextInputIndex = 0

    val numWildcards = patternChunks.length - 1
    val matchStarts = Array.fill(numWildcards)(-1)
    val matchEnds = Array.fill(numWildcards)(-1)

    val nameLength = input.length
    // The final pattern is as long as all the chunks, separated by 1-character
    // glob-wildcard placeholders
    val patternLength = patternChunks.iterator.map(_.length).sum + numWildcards

    // Convert the input pattern chunks into a single sequence of shorts; each
    // non-negative short represents a character, while -1 represents a glob wildcard
    val pattern = {
      val b = new scala.collection.mutable.ArrayBuilder.ofShort; b.sizeHint(patternLength)
      patternChunks.head.foreach(c => b.+=(c.toShort))
      patternChunks.tail.foreach { s => b.+=(-1); s.foreach(c => b.+=(c.toShort)) }
      b.result()
    }

    // Lookup table for each character in the pattern to check whether or not
    // it refers to a glob wildcard; a non-negative integer indicates which
    // glob wildcard it represents, while -1 means it doesn't represent any
    val matchIndices = {
      val arr = Array.fill(patternLength + 1)(-1)
      val _ = patternChunks.init.zipWithIndex.foldLeft(0) { case (ttl, (chunk, i)) =>
        val sum = ttl + chunk.length
        arr(sum) = i
        sum + 1
      }
      arr
    }

    while (patternIndex < patternLength || inputIndex < nameLength) {
      matchIndices(patternIndex) match {
        case -1 => // do nothing
        case n =>
          matchStarts(n) = matchStarts(n) match {
            case -1 => inputIndex
            case s  => math.min(s, inputIndex)
          }
          matchEnds(n) = matchEnds(n) match {
            case -1 => inputIndex
            case s  => math.max(s, inputIndex)
          }
      }

      val continue = if (patternIndex < patternLength) {
        val c = pattern(patternIndex)
        c match {
          case -1 => // zero-or-more-character wildcard
            // Try to match at nx. If that doesn't work out, restart at nx+1 next.
            nextPatternIndex = patternIndex
            nextInputIndex = inputIndex + 1
            patternIndex += 1
            true
          case _ => // ordinary character
            if (inputIndex < nameLength && input(inputIndex) == c) {
              patternIndex += 1
              inputIndex += 1
              true
            } else {
              false
            }
        }
      } else false

      // Mismatch. Maybe restart.
      if (!continue) {
        if (0 < nextInputIndex && nextInputIndex <= nameLength) {
          patternIndex = nextPatternIndex
          inputIndex = nextInputIndex
        } else {
          return None
        }
      }
    }

    // Matched all of pattern to all of name. Success.
    Some(
      Array.tabulate(patternChunks.length - 1)(n => input.slice(matchStarts(n), matchEnds(n))).toSeq
    )
  }
  // $COVERAGE-ON$
}
