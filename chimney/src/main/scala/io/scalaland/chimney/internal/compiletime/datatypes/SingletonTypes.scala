package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

/** Thin view over Hearth's `SingletonValue`, extended with literal types, `Unit` and `Null` (bridged with Hearth's
  * `TypeCodec` + `ExprCodec` - `Expr.singletonOf` does not cover constant types).
  *
  * Hearth semantics: any publicly referencable singleton value matches - case objects, PLAIN objects, parameterless
  * Scala 3 enum cases, Java enum values, Scala `Enumeration` values and stable term refs (the availability gate lives
  * in `ProductType.parseSingleton`).
  */
private[compiletime] trait SingletonTypes { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Describes all types which are singletons (singleton literal types, Unit, Null, objects, enum values, ...).
    *
    * Should have the same behavior as `ValueOf` without relying on it.
    */
  final protected case class Singleton[A](value: Expr[A])
  protected object Singleton {

    def unapply[A](A: Type[A]): Option[Expr[A]] = SingletonType.parse(using A).map(_.value)
  }

  protected object SingletonType {

    // Not implicit (the hearth#316 sibling-implicit-lazy-Type deadlock this guarded against is fixed since
    // 0.4.1 - kept explicit to avoid ambient-implicit ambiguity).
    private lazy val UnitType: Type[Unit] = Type.of[Unit]
    private lazy val NullType: Type[Null] = Type.of[Null]
    // defs, NOT lazy vals: a trait-level materialized Expr would be created under whichever splice touches it first
    // and leak into later splices (cross-quotes usage contract violation, ScopeException under -Xcheck-macros).
    private def unitExpr: Expr[Unit] = Expr.UnitExprCodec.toExpr(())
    private def nullExpr: Expr[Null] = Expr.NullExprCodec.toExpr(null)

    final def parse[A: Type]: Option[Singleton[A]] =
      Type[A] match {
        case _ if Type[A] <:< UnitType =>
          implicit val Unit: Type[Unit] = UnitType
          Some(Singleton(castToExpr[Unit, A](unitExpr)))
        case _ if Type[A] <:< NullType =>
          implicit val Null: Type[Null] = NullType
          Some(Singleton(castToExpr[Null, A](nullExpr)))
        case _ =>
          literalOf[A].orElse(ProductType.parseSingleton[A].map(_.singletonExpr)).map(Singleton(_))
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
