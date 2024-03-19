package io.scalaland.chimney.dsl

// TODO: documentation

abstract class TransformedNamesComparison { this: Singleton =>

  def namesMatch(fromName: String, toName: String): Boolean
  def dropSet(name: String): String
}
object TransformedNamesComparison {

  object BeanAware extends TransformedNamesComparison {

    private val getAccessor = raw"(?i)get(.)(.*)".r
    private val isAccessor = raw"(?i)is(.)(.*)".r
    private val setAccessor = raw"(?i)set(.)(.*)".r

    override def dropSet(name: String): String = name match {
      case setAccessor(head, tail) => head.toLowerCase + tail
      case other                   => other
    }

    private val dropGetIs: String => String = {
      case getAccessor(head, tail) => head.toLowerCase + tail
      case isAccessor(head, tail)  => head.toLowerCase + tail
      case other                   => other
    }
    private val normalize: String => String = dropGetIs.andThen(dropSet)

    def namesMatch(fromName: String, toName: String): Boolean =
      fromName == toName || normalize(fromName) == normalize(toName)
  }

  object StrictEquality extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean = fromName == toName
    override def dropSet(name: String): String = name
  }

  object CaseInsensitiveEquality extends TransformedNamesComparison {

    def namesMatch(fromName: String, toName: String): Boolean = fromName.equalsIgnoreCase(toName)
    override def dropSet(name: String): String = name
  }

  type FieldDefault = BeanAware.type
  val FieldDefault: FieldDefault = BeanAware

  type SubtypeDefault = StrictEquality.type
  val SubtypeDefault: SubtypeDefault = StrictEquality
}
