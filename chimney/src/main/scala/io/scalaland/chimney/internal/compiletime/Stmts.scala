package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Stmts { this: Definitions =>

  type Stmt[T]
  type ValDef[T] <: Stmt[T]
  type VarDef[T] <: Stmt[T]
  type LazyValDef[T] <: Stmt[T]

  def blockExpr(stats: Seq[Stmt[_]], expr: Expr[T]): Expr[T]
}
