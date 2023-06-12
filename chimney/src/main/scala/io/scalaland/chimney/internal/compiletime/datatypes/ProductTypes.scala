package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

private[compiletime] trait ProductTypes { this: Definitions =>

  final protected case class ProductType[A](
      extraction: ProductType.Getters[A],
      construction: ProductType.Construction[A]
  )
  protected object ProductType {

    final def unapply[A](implicit tpe: Type[A]): Option[ProductType[A]] = parseAsProductType(tpe)

    final case class Getter[From, A](name: String, sourceType: Getter.SourceType, get: Expr[From] => Expr[A])
    object Getter {
      sealed trait SourceType extends Product with Serializable
      object SourceType {
        case object ConstructorVal extends SourceType
        case object AccessorMethod extends SourceType
        case object JavaBeanGetter extends SourceType
      }
    }
    final type Getters[From] = List[Existential[Getter[From, *]]]

    final case class Setter[To, A](name: String, set: (Expr[To], Expr[A]) => Expr[Unit])
    final type Setters[To] = List[Existential[Setter[To, *]]]

    final case class Param[A](name: String, fallbacks: Vector[Param.Fallback[A]])
    object Param {
      sealed trait Fallback[A] extends Product with Serializable
      object Fallback {
        final case class DefaultValue[A](default: Expr[A]) extends Fallback[A]
      }
    }
    final type Params = List[List[Existential[Param]]]

    sealed trait Construction[To] extends Product with Serializable
    object Construction {

      final case class JavaBean[To](defaultConstructor: Expr[To], setters: Setters[To]) extends Construction[To]
      final case class CaseClass[To](parameters: Params, create: Params => Expr[To]) extends Construction[To]
    }
  }

  protected def parseAsProductType[A: Type]: Option[ProductType[A]]
}
