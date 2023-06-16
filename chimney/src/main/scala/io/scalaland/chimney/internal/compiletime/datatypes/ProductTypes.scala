package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait ProductTypes { this: Definitions =>

  final protected case class Product[A](extraction: Product.Getters[A], construction: Product.Constructor[A])
  protected object Product {

    final case class Getter[From, A](sourceType: Getter.SourceType, get: Expr[From] => Expr[A])
    object Getter {
      sealed trait SourceType extends scala.Product with Serializable
      object SourceType {
        case object ConstructorVal extends SourceType
        case object AccessorMethod extends SourceType
        case object JavaBeanGetter extends SourceType
      }
    }
    final type Getters[From] = Map[String, Existential[Getter[From, *]]]

    final case class Parameter[A](targetType: Parameter.TargetType, defaultValue: Option[Expr[A]])
    object Parameter {
      sealed trait TargetType extends scala.Product with Serializable
      object TargetType {
        case object ConstructorParameter extends TargetType
        case object SetterParameter extends TargetType
      }
    }
    final type Parameters = Map[String, Existential[Parameter]]

    final type Arguments = Map[String, ExistentialExpr]

    final case class Constructor[To](parameters: Parameters, constructor: Arguments => Expr[To])
  }

  protected val ProductType: ProductTypesModule
  protected trait ProductTypesModule { this: ProductType.type =>

    def isCaseClass[A](A: Type[A]): Boolean
    def isCaseObject[A](A: Type[A]): Boolean
    def isJavaBean[A](A: Type[A]): Boolean

    def parse[A: Type]: Option[Product[A]]
    final def unapply[A](tpe: Type[A]): Option[Product[A]] = parse(tpe)

    implicit class RegexpOps(regexp: scala.util.matching.Regex) {

      def isMatching(value: String): Boolean = regexp.pattern.matcher(value).matches() // 2.12 doesn't have .matches
    }

    private val getAccessor = raw"(?i)get(.)(.*)".r
    private val isAccessor = raw"(?i)is(.)(.*)".r
    val dropGetIs: String => String = {
      case getAccessor(head, tail) => head.toLowerCase + tail
      case isAccessor(head, tail)  => head.toLowerCase + tail
      case other                   => other
    }
    val isGetterName: String => Boolean = name => getAccessor.isMatching(name) || isAccessor.isMatching(name)

    private val setAccessor = raw"(?i)set(.)(.*)".r
    val dropSet: String => String = {
      case setAccessor(head, tail) => head.toLowerCase + tail
      case other                   => other
    }
    val isSetterName: String => Boolean = name => setAccessor.isMatching(name)
  }

  implicit class ProductTypeOps[A](private val tpe: Type[A]) {

    def isCaseClass: Boolean = ProductType.isCaseClass(tpe)
    def isCaseObject: Boolean = ProductType.isCaseObject(tpe)
    def isJavaBean: Boolean = ProductType.isJavaBean(tpe)
  }
}
