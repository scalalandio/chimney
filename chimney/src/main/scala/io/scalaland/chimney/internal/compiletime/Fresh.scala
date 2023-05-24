package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Fresh { this: Definitions =>

  abstract class FreshIdent[T](ident: String) {
    def asIdentExpr: Expr[T] // using "ident" as expr
    def asValDef[U <: T](initExpr: Expr[U]): ValDef[T] // "val ident = initExpr"
    def asFunctionExpr[U](funBodyExpr: Expr[U]): Expr[T] => Expr[U] // "{ ident => funBodyExpr }"
  }

  def newFreshIdent[T: Type](namePrefix: String): FreshIdent[T]
  // ident = namePrefix + separator + random suffix; impl might be platform-dependent

}
