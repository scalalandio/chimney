package io.scalaland.chimney.internal.compiletime.dsl.utils

import io.scalaland.chimney.internal.runtime

import scala.annotation.nowarn
import scala.quoted.*

private[chimney] class DslMacroUtils()(using quotes: Quotes) {
  import quotes.*
  import quotes.reflect.*

  private object SelectLike {
    def unapply(term: Term): Option[(Term, String)] = term match {
      case Select(instance, name)               => Some((instance, name))
      case Apply(Select(instance, name), Nil)   => Some((instance, name))
      case Block(_, SelectLike(instance, name)) => Some((instance, name))
      case _                                    => None
    }
  }

  private trait ExistentialString {
    type Underlying <: String
    implicit val Underlying: Type[Underlying]
  }
  private object ExistentialString {

    def apply(fieldName: String): ExistentialString = new ExistentialString {
      type Underlying = String
      val Underlying: Type[String] = ConstantType(StringConstant(fieldName)).asType.asInstanceOf[Type[String]]
    }
  }

  private trait ExistentialPath {
    type Underlying <: runtime.Path
    implicit val Underlying: Type[Underlying]
  }
  private object ExistentialPath {

    @nowarn(
      "msg=the type test for DslMacroUtils.this.quotes.reflect.ValDef cannot be checked at runtime because it refers to an abstract type member or type parameter"
    )
    def parse(t: Tree): Either[String, ExistentialPath] = t match {
      case Block(List(DefDef(_, List(List(ValDef(in, _, _))), _, Some(selects))), _) =>
        def unpackSelects(selects: Tree): Either[String, ExistentialPath] = selects match {
          case Ident(out) if in == out =>
            Right(new ExistentialPath {
              type Underlying = runtime.Path.Root
              val Underlying: Type[runtime.Path.Root] = Type.of[runtime.Path.Root]
            })
          case _: Ident => Left(ignoringInputNotAllowed(t))
          case SelectLike(t2, fieldName) =>
            unpackSelects(t2).map { instance =>
              val name = ExistentialString(fieldName)
              import instance.Underlying as Instance
              import name.Underlying as FieldName
              new ExistentialPath {
                type Underlying = runtime.Path.Select[FieldName, Instance]
                val Underlying: Type[runtime.Path.Select[FieldName, Instance]] =
                  Type.of[runtime.Path.Select[FieldName, Instance]]
              }
            }
          case Apply(_, _) => Left(arbitraryFunctionNotAllowed(t))
          case _           => Left(invalidSelectorErrorMessage(t))
        }
        unpackSelects(selects)
      case Inlined(_, _, block) => parse(block)
      case _                    => Left(invalidSelectorErrorMessage(t))
    }
    List(1 -> 2).fol

    private def invalidSelectorErrorMessage(t: Tree): String =
      s"Invalid selector expression: ${t.show(using Printer.TreeAnsiCode)}"

    private def arbitraryFunctionNotAllowed(t: Tree): String =
      s"Invalid selector expression - only vals, and nullary defs allowed: ${t.show(using Printer.TreeAnsiCode)}"

    private def ignoringInputNotAllowed(t: Tree): String =
      s"Invalid selector expression - only input value can be extracted from: ${t.show(using Printer.TreeAnsiCode)}"
  }

  def applyFieldNameType[Out](f: [A <: runtime.Path] => Type[A] ?=> Out)(selector: Expr[?]): Out =
    ExistentialPath
      .parse(selector.asTerm)
      .fold[Out](error => report.errorAndAbort(error, Position.ofMacroExpansion), path => f(using path.Underlying))

  def applyFieldNameTypes[Out](
      f: [A <: runtime.Path, B <: runtime.Path] => Type[A] ?=> Type[B] ?=> Out
  )(selector1: Expr[?], selector2: Expr[?]): Out =
    (ExistentialPath.parse(selector1.asTerm), ExistentialPath.parse(selector2.asTerm)) match {
      case (Right(path1), Right(path2)) => f(using path1.Underlying)(using path2.Underlying)
      case (Left(error1), Left(error2)) =>
        report.errorAndAbort(s"Invalid selectors:\n$error1\n$error2", Position.ofMacroExpansion)
      case (Left(error), _) => report.errorAndAbort(error, Position.ofMacroExpansion)
      case (_, Left(error)) => report.errorAndAbort(error, Position.ofMacroExpansion)
    }

  def applyConstructorType[Out](
      f: [Ctor <: runtime.ArgumentLists] => Type[Ctor] ?=> Out
  )(ctor: Expr[?]): Out = {
    // TODO: analyze f and implement the type
    val _ = ctor
    f[runtime.ArgumentLists.Empty]
  }
}
