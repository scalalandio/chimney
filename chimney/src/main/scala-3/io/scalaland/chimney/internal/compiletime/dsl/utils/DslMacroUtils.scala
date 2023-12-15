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

    private def invalidSelectorErrorMessage(t: Tree): String =
      s"Invalid selector expression: ${t.show(using Printer.TreeAnsiCode)}"

    private def arbitraryFunctionNotAllowed(t: Tree): String =
      s"Invalid selector expression - only vals, and nullary defs allowed: ${t.show(using Printer.TreeAnsiCode)}"

    private def ignoringInputNotAllowed(t: Tree): String =
      s"Invalid selector expression - only input value can be extracted from: ${t.show(using Printer.TreeAnsiCode)}"
  }

  private trait ExistentialCtor {
    type Underlying <: runtime.ArgumentLists
    implicit val Underlying: Type[Underlying]
  }
  private object ExistentialCtor {

    def parse(t: Tree): Either[String, ExistentialCtor] = {
      def extractParams(t: Tree): Either[String, List[List[ValDef]]] = t match {
        case DefDef(_, params, _, bodyOpt) =>
          val head = params.map(_.params.asInstanceOf[List[ValDef]])

          // TODO: sprawdzić czy to faktycznie działa

          bodyOpt match {
            case Some(body) =>
              extractParams(body) match {
                case Left(_) =>
                  println(s"Skipped: $body")
                  Right(head)
                case Right(tail) =>
                  println(s"Prepended: $head to $tail")
                  Right(head ++ tail)
              }
            case None =>
              println(s"No body")
              Right(head)
          }
        case Block(List(defdef), Closure(_, _)) =>
          println("Unwrapping single element block")
          extractParams(defdef)
        case Block(Nil, singleTerm) =>
          println("Unwrapping single element block")
          extractParams(singleTerm)
        case Inlined(_, _, block) =>
          println("Unwrapping inlined")
          extractParams(block)
        case _ =>
          println(s"""Expression:
                     |${t.show(using Printer.TreeAnsiCode)}
                     |defined as:
                     |${t.show(using Printer.TreeStructure)}
                     |of type:
                     |${t.asInstanceOf[Term].tpe.show(using Printer.TypeReprAnsiCode)}
                     |of type:
                     |${t.asInstanceOf[Term].tpe.show(using Printer.TypeReprStructure)}
                     |""".stripMargin)
          Left(invalidConstructor(t))
      }

      extractParams(t).map(params =>
        new ExistentialCtor {
          type Underlying = runtime.ArgumentLists
          implicit val Underlying: Type[runtime.ArgumentLists] = paramsToType(params)
        }
      )
    }

    private def paramsToType(paramsLists: List[List[ValDef]]): Type[runtime.ArgumentLists] =
      paramsLists
        .map { paramList =>
          paramList.foldRight[Type[? <: runtime.ArgumentList]](Type.of[runtime.ArgumentList.Empty])(
            constructArgumentListType
          )
        }
        .foldRight[Type[? <: runtime.ArgumentLists]](Type.of[runtime.ArgumentLists.Empty])(
          constructArgumentListsType
        )
        .asInstanceOf[Type[runtime.ArgumentLists]]

    private def constructArgumentListsType(
        head: Type[? <: runtime.ArgumentList],
        tail: Type[? <: runtime.ArgumentLists]
    ): Type[? <: runtime.ArgumentLists] = {
      object ApplyParams {
        def apply[Head <: runtime.ArgumentList: Type, Tail <: runtime.ArgumentLists: Type]
            : Type[runtime.ArgumentLists.List[Head, Tail]] =
          Type.of[runtime.ArgumentLists.List[Head, Tail]]
      }

      ApplyParams(head, tail)
    }

    private def constructArgumentListType(
        t: ValDef,
        args: Type[? <: runtime.ArgumentList]
    ): Type[? <: runtime.ArgumentList] = {
      val ValDef(name, tpe, _) = t

      object ApplyParam {
        def apply[ParamName <: String: Type, ParamType: Type, Args <: runtime.ArgumentList: Type]
            : Type[runtime.ArgumentList.Argument[ParamName, ParamType, Args]] =
          Type.of[runtime.ArgumentList.Argument[ParamName, ParamType, Args]]
      }

      ApplyParam(
        ConstantType(StringConstant(name)).asType.asInstanceOf[Type[String]],
        tpe.tpe.asType.asInstanceOf[Type[Any]],
        args
      )
    }

    private def invalidConstructor(t: Tree): String =
      s"Expected function, instead got: ${t.show(using Printer.TreeAnsiCode)}: ${t.asInstanceOf[Term].tpe.show(using Printer.TypeReprAnsiCode)}"
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
  )(ctor: Expr[?]): Out =
    ExistentialCtor.parse(ctor.asTerm) match {
      case Right(ctorType) =>
        println(s"""Expression:
                   |${ctor.asTerm.show(using Printer.TreeAnsiCode)}
                   |defined as:
                   |${ctor.asTerm.show(using Printer.TreeStructure)}
                   |of type:
                   |${ctor.asTerm.tpe.show(using Printer.TypeReprAnsiCode)}
                   |of type:
                   |${ctor.asTerm.tpe.show(using Printer.TypeReprStructure)}
                   |resolved as:
                   |${TypeRepr.of(using ctorType.Underlying).show(using Printer.TypeReprAnsiCode)}
                   |""".stripMargin)
        f(using ctorType.Underlying)
      case Left(error) => report.errorAndAbort(error, Position.ofMacroExpansion)
    }
}
