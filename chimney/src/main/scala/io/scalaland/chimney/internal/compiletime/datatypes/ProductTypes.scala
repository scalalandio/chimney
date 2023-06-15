package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait ProductTypes { this: Definitions =>

  final protected case class Product[A](extraction: Product.Getters[A], construction: Product.Constructor[A])
  protected object Product {

    final case class Getter[From, A](name: String, sourceType: Getter.SourceType, get: Expr[From] => Expr[A])
    object Getter {
      sealed trait SourceType extends scala.Product with Serializable
      object SourceType {
        case object ConstructorVal extends SourceType
        case object AccessorMethod extends SourceType
        case object JavaBeanGetter extends SourceType
      }
    }
    final type Getters[From] = List[Existential[Getter[From, *]]]

    final case class Setter[To, A](name: String, set: (Expr[To], Expr[A]) => Expr[Unit])
    final type Setters[To] = List[Existential[Setter[To, *]]]

    final case class Parameter[A](name: String, defaultValue: Option[Parameter.DefaultValue[A]])
    object Parameter {
      final case class DefaultValue[A](default: Expr[A])
    }
    final type Parameters = List[List[Existential[Parameter]]]

    final case class Argument[A](name: String, value: Expr[A])
    final type Arguments = List[Existential[Argument]]

    sealed trait Constructor[To] extends scala.Product with Serializable
    object Constructor {

      final case class JavaBean[To](defaultConstructor: Expr[To], setters: Setters[To]) extends Constructor[To]
      final case class CaseClass[To](constructor: Arguments => Expr[To], parameters: Parameters) extends Constructor[To]
    }
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
    private val dropGetIs: String => String = {
      case getAccessor(head, tail) => head.toLowerCase + tail
      case isAccessor(head, tail)  => head.toLowerCase + tail
      case other                   => other
    }
    val isGetterName: String => Boolean = name => getAccessor.isMatching(name) || isAccessor.isMatching(name)

    private val setAccessor = raw"(?i)set(.)(.*)".r
    private val dropSet: String => String = {
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
