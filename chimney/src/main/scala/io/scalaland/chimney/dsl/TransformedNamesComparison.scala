package io.scalaland.chimney.dsl

// TODO: documentation

abstract class TransformedNamesComparison { this: Singleton =>

  def namesMatch(fromName: String, toName: String): Boolean
}
object TransformedNamesComparison {

  object BeanAware extends TransformedNamesComparison {

    // While it's bad to refer to compiletime package this code should only be used by this compiletime package.
    // Additionally, current module has to rely on chimney-macro-commons, not the other way round.
    import io.scalaland.chimney.internal.compiletime.datatypes.ProductTypes
    private val normalize = ProductTypes.BeanAware.dropGetIs andThen ProductTypes.BeanAware.dropSet

    def namesMatch(fromName: String, toName: String): Boolean =
      fromName == toName || normalize(fromName) == normalize(toName)
  }

  object StrictEquality extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean = fromName == toName
  }

  object CaseInsensitiveEquality extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
  }

  type FieldDefault = BeanAware.type
  val FieldDefault: FieldDefault = BeanAware

  type SubtypeDefault = StrictEquality.type
  val SubtypeDefault: SubtypeDefault = StrictEquality
}
