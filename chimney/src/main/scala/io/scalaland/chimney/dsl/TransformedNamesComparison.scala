package io.scalaland.chimney.dsl

// TODO: documentation

abstract class TransformedNamesComparison { this: Singleton =>

  def namesMatch(fromName: String, toName: String): Boolean
}
object TransformedNamesComparison {

  object BeanAware extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean =
      // TODO: move logic here, so that dsl would not have a dependency on internal.compiletime
      io.scalaland.chimney.internal.compiletime.datatypes.ProductTypes.areNamesMatching(fromName, toName)
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
