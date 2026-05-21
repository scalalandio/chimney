package io.scalaland.chimney.dsl

/** Provides a way of customizing how fields/subtypes should get matched between source value and target value.
  *
  * @see
  *   [[https://chimney.readthedocs.io/supported-transformations/#defining-custom-name-matching-predicate]] for more
  *   details
  *
  * @since 1.0.0
  */
abstract class TransformedNamesComparison { this: Singleton =>

  /** Return true if `fromName` should be considered a match for `toName`.
    *
    * @param fromName
    *   name of a field/subtype in the source type
    * @param toName
    *   name of a field/subtype in the target type
    * @return
    *   whether fromName should be used as a source for value in toName
    */
  def namesMatch(fromName: String, toName: String): Boolean
}

/** @since 1.0.0 */
object TransformedNamesComparison {

  /** Matches names, dropping is/get/set prefixes and then lowercasing the first letter if it was a Bean name. */
  case object BeanAware extends TransformedNamesComparison {

    // While it's bad to refer to compiletime package this code should only be used by this compiletime package.
    // Additionally, current module has to rely on chimney-macro-commons, not the other way round.
    import io.scalaland.chimney.internal.compiletime.datatypes.ProductTypes
    private val normalize = ProductTypes.BeanAware.dropGetIs andThen ProductTypes.BeanAware.dropSet

    def namesMatch(fromName: String, toName: String): Boolean =
      fromName == toName || normalize(fromName) == normalize(toName)
  }

  /** Matches only the same Strings. */
  case object StrictEquality extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean = fromName == toName
  }

  /** Matches Strings ignoring upper/lower case distinction. */
  case object CaseInsensitiveEquality extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
  }

  type FieldDefault = BeanAware.type
  val FieldDefault: FieldDefault = BeanAware

  type SubtypeDefault = StrictEquality.type
  val SubtypeDefault: SubtypeDefault = StrictEquality
}

case object CamelSnakeCaseEquality extends TransformedNamesComparison {

  private def snakeToCamel(snake: String): (String, Boolean) =
    snake.split('_') match {
      case Array(part) => (part, false)
      case parts       =>
        val camel = parts
          .filter(_.nonEmpty) // Remove empty parts from consecutive underscores
          .zipWithIndex
          .map { case (part, idx) =>
            if (idx == 0) part.toLowerCase
            else part.toLowerCase.capitalize
          }
          .mkString
        (camel, true)
    }

  def namesMatch(fromName: String, toName: String): Boolean = {
    val (from, fromWasCamelCase) = snakeToCamel(fromName)
    val (to, toWasCamelCase) = snakeToCamel(toName)
    if (fromWasCamelCase == toWasCamelCase)
      fromName == toName
    else
      from == to
  }
}
