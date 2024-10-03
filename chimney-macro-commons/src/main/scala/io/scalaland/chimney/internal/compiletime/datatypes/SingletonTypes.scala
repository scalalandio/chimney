package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

trait SingletonTypes { this: (Definitions & ProductTypes) =>

  /** Describes all types which are singletons (singleton literal types, Unit, Null, case objects, vals, ...).
    *
    * Should have the same behavior as `ValueOf` without relying on it (it's unavailable in Scala 2.12).
    */
  final protected case class Singleton[A](value: Expr[A])
  protected object Singleton {

    def unapply[A](A: Type[A]): Option[Expr[A]] = A match {
      case SingletonType(singleton) => Some(singleton.value)
      case _                        => None
    }
  }

  protected object SingletonType {

    import Type.Implicits.*

    final def parse[A: Type]: Option[Singleton[A]] = {
      def found[B](b: Expr[B]): Option[Singleton[A]] = Some(Singleton(b.asInstanceOf[Expr[A]]))
      Type[A] match {
        case _ if Type[A] <:< Type[Unit] => found(Expr.Unit.asInstanceOfExpr[A])
        case _ if Type[A] <:< Type[Null] => found(Expr.Null.asInstanceOfExpr[A])
        case Type.BooleanLiteral(b)      => found(Expr.Boolean(b.value))
        case Type.IntLiteral(i)          => found(Expr.Int(i.value))
        case Type.LongLiteral(l)         => found(Expr.Long(l.value))
        case Type.FloatLiteral(f)        => found(Expr.Float(f.value))
        case Type.DoubleLiteral(d)       => found(Expr.Double(d.value))
        case Type.CharLiteral(c)         => found(Expr.Char(c.value))
        case Type.StringLiteral(s)       => found(Expr.String(s.value))
        // This supports ONLY case object or parameterless enum's cases (on Scala 3). We might consider expanding it.
        case _ if ProductType.isCaseObject[A] || ProductType.isCaseVal[A] || ProductType.isJavaEnumValue[A] =>
          Type[A] match {
            case Product.Constructor(params, ctor) if params.isEmpty => found(ctor(Map.empty))
            // $COVERAGE-OFF$should never happen unless we messed up
            case _ =>
              assertionFailed(
                s"Expected case object/case with no params/Java enum of ${Type.prettyPrint[A]} to have a nullary constructor"
              )
            // $COVERAGE-ON$
          }
        case _ => None
      }
    }
    final def unapply[A](tpe: Type[A]): Option[Singleton[A]] = parse(using tpe)
  }
}
