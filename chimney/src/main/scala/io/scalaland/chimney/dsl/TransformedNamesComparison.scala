package io.scalaland.chimney.dsl

/** Provides a way of customizing how fields/subtypes shoud get matched betwen source value and target value.
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

  /** Matches names, assuming VALUE_NAME convention for Java enums should match ValueName. */
  case object EnumAware extends TransformedNamesComparison {

    private def normalize(name: String): String =
      if (name.forall(c => !c.isLetter || c.isUpper))
        name.split("_").map(s => s.head.toString + s.tail.toLowerCase).mkString
      else name

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

  type SubtypeDefault = EnumAware.type
  val SubtypeDefault: SubtypeDefault = EnumAware
}
