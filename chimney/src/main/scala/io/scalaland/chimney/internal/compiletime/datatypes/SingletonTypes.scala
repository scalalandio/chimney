package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

/** Hearth's `SingletonValue`/`Expr.singletonOf` covers objects/enum values/stable refs but NOT literal types, `Unit`
  * and `Null` - those are bridged with Hearth's `TypeCodec` (literal type -> value) + `ExprCodec` (value -> literal
  * expr). The case-object/case-val/Java-enum-value branch goes through `Product.Constructor` so that it matches `case
  * object`s, but not plain `object`s, even though Hearth's `SingletonValue` would.
  */
private[compiletime] trait SingletonTypes { this: ChimneyDefinitions & hearth.MacroCommons =>

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

    // hearth#316: implicit Type vals with cross-quoted initializers deadlock lazy-val init at macro runtime on
    // Scala 3 - keep these NON-implicit (object-level lazies with no implicit-Type-val siblings are safe).
    private lazy val UnitType: Type[Unit] = Type.of[Unit]
    private lazy val NullType: Type[Null] = Type.of[Null]
    private lazy val unitExpr: Expr[Unit] = Expr.UnitExprCodec.toExpr(())
    private lazy val nullExpr: Expr[Null] = Expr.NullExprCodec.toExpr(null)

    final def parse[A: Type]: Option[Singleton[A]] = {
      def found[B](b: Expr[B]): Option[Singleton[A]] = Some(Singleton(b.asInstanceOf[Expr[A]]))
      Type[A] match {
        case _ if Type[A] <:< UnitType =>
          implicit val Unit: Type[Unit] = UnitType
          found(castToExpr[Unit, A](unitExpr))
        case _ if Type[A] <:< NullType =>
          implicit val Null: Type[Null] = NullType
          found(castToExpr[Null, A](nullExpr))
        case _ =>
          literalOf[A]
            .map(expr => Singleton(expr))
            .orElse {
              // This supports ONLY case object or parameterless enum's cases (on Scala 3). We might consider expanding it.
              if (ProductType.isCaseObject[A] || ProductType.isCaseVal[A] || ProductType.isJavaEnumValue[A])
                Type[A] match {
                  case Product.Constructor(params, ctor) if params.isEmpty => found(ctor(Map.empty))
                  // $COVERAGE-OFF$should never happen unless we messed up
                  case _ =>
                    assertionFailed(
                      s"Expected case object/case with no params/Java enum of ${Type.prettyPrint[A]} to have a nullary constructor"
                    )
                  // $COVERAGE-ON$
                }
              else None
            }
      }
    }
    final def unapply[A](tpe: Type[A]): Option[Singleton[A]] = parse(using tpe)

    /** Matches literal singleton types (Boolean/Int/.../String) via Hearth's `TypeCodec`+`ExprCodec`.
      *
      * The `Expr`-level cast is safe: the emitted tree is the literal itself, which inhabits the literal type `A`.
      */
    private def literalOf[A](implicit A: Type[A]): Option[Expr[A]] = {
      def lit[U](codec: TypeCodec[U], exprCodec: ExprCodec[U]): Option[Expr[A]] =
        codec.fromType(A).map { extracted =>
          val value: U = extracted.value
          exprCodec.toExpr(value).asInstanceOf[Expr[A]]
        }
      lit(Type.BooleanCodec, Expr.BooleanExprCodec)
        .orElse(lit(Type.IntCodec, Expr.IntExprCodec))
        .orElse(lit(Type.LongCodec, Expr.LongExprCodec))
        .orElse(lit(Type.FloatCodec, Expr.FloatExprCodec))
        .orElse(lit(Type.DoubleCodec, Expr.DoubleExprCodec))
        .orElse(lit(Type.CharCodec, Expr.CharExprCodec))
        .orElse(lit(Type.StringCodec, Expr.StringExprCodec))
    }
  }
}
