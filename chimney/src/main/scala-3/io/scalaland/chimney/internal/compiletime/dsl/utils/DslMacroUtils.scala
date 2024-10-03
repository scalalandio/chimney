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

  private object IsOptionOf {
    def unapply(term: Term): Option[(ExistentialType, ExistentialType, ExistentialType)] = term.tpe.asType match {
      case '[runtime.IsOption.Of[o, sv, s]] => Some((ExistentialType[o], ExistentialType[sv], ExistentialType[s]))
      case _                                => None
    }
  }

  private object IsEitherOf {
    def unapply(
        term: Term
    ): Option[(ExistentialType, ExistentialType, ExistentialType, ExistentialType, ExistentialType)] =
      term.tpe.asType match {
        case '[runtime.IsEither.Of[e, lv, rv, l, r]] =>
          Some((ExistentialType[e], ExistentialType[lv], ExistentialType[rv], ExistentialType[l], ExistentialType[r]))
        case _ => None
      }
  }

  private object IsCollectionOf {
    def unapply(term: Term): Option[(ExistentialType, ExistentialType)] =
      term.tpe.asType match {
        case '[runtime.IsCollection.Of[c, a]] => Some((ExistentialType[c], ExistentialType[a]))
        case _                                => None
      }
  }

  private object IsMapOf {
    def unapply(term: Term): Option[(ExistentialType, ExistentialType, ExistentialType)] =
      term.tpe.asType match {
        case '[runtime.IsMap.Of[m, k, v]] => Some((ExistentialType[m], ExistentialType[k], ExistentialType[v]))
        case _                            => None
      }
  }

  private trait ExistentialType {
    type Underlying
    implicit val Underlying: Type[Underlying]
  }
  private object ExistentialType {

    def apply[A](implicit tpe: Type[A]): ExistentialType = new ExistentialType {
      type Underlying = A
      val Underlying: Type[A] = tpe
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
          // matches `_` part in `_.foo.bar.baz...`
          case Ident(out) if in == out =>
            Right(new ExistentialPath {
              type Underlying = runtime.Path.Root
              val Underlying: Type[runtime.Path.Root] = Type.of[runtime.Path.Root]
            })
          // matches `_ => something unrelated` - not allowed
          case i: Ident => Left(ignoringInputNotAllowed(i, t))
          // matches `.fieldName` AND `.fieldName()`
          case SelectLike(t2, fieldName) =>
            unpackSelects(t2).map { init =>
              val name = ExistentialString(fieldName)
              import init.Underlying as Init, name.Underlying as FieldName
              new ExistentialPath {
                type Underlying = runtime.Path.Select[Init, FieldName]
                val Underlying: Type[runtime.Path.Select[Init, FieldName]] =
                  Type.of[runtime.Path.Select[Init, FieldName]]
              }
            }
          // matches `.matching[Subtype]`
          case TypeApply(Apply(TypeApply(Ident("matching"), _), List(t2)), List(subtypeA)) =>
            unpackSelects(t2).map { init =>
              val subtype = ExistentialType(subtypeA.tpe.asType.asInstanceOf[Type[Any]])
              import init.Underlying as Init, subtype.Underlying as Subtype
              new ExistentialPath {
                type Underlying = runtime.Path.Matching[Init, Subtype]
                val Underlying: Type[runtime.Path.Matching[Init, Subtype]] =
                  Type.of[runtime.Path.Matching[Init, Subtype]]
              }
            }
          // matches `.matchingSome`
          case Apply(
                TypeApply(Apply(TypeApply(Ident("matchingSome"), _), List(t2)), _),
                List(IsOptionOf(_, _, someA))
              ) =>
            unpackSelects(t2).map { init =>
              import init.Underlying as Init, someA.Underlying as SomeA
              new ExistentialPath {
                type Underlying = runtime.Path.Matching[Init, SomeA]
                val Underlying: Type[runtime.Path.Matching[Init, SomeA]] = Type.of[runtime.Path.Matching[Init, SomeA]]
              }
            }
          // matches `.matchingLeft`
          case Apply(
                TypeApply(Apply(TypeApply(Ident("matchingLeft"), _), List(t2)), _),
                List(IsEitherOf(_, _, _, left, _))
              ) =>
            unpackSelects(t2).map { init =>
              import init.Underlying as Init, left.Underlying as Left
              new ExistentialPath {
                type Underlying = runtime.Path.Matching[Init, Left]
                val Underlying: Type[runtime.Path.Matching[Init, Left]] = Type.of[runtime.Path.Matching[Init, Left]]
              }
            }
          // matches `.matchingRight`
          case Apply(
                TypeApply(Apply(TypeApply(Ident("matchingRight"), _), List(t2)), _),
                List(IsEitherOf(_, _, _, _, right))
              ) =>
            unpackSelects(t2).map { init =>
              import init.Underlying as Init, right.Underlying as Right
              new ExistentialPath {
                type Underlying = runtime.Path.Matching[Init, Right]
                val Underlying: Type[runtime.Path.Matching[Init, Right]] = Type.of[runtime.Path.Matching[Init, Right]]
              }
            }
          // matches `.everyItem`
          case Apply(Apply(TypeApply(Ident("everyItem"), _), List(t2)), List(IsCollectionOf(_, _))) =>
            unpackSelects(t2).map { init =>
              import init.Underlying as Init
              new ExistentialPath {
                type Underlying = runtime.Path.EveryItem[Init]
                val Underlying: Type[runtime.Path.EveryItem[Init]] = Type.of[runtime.Path.EveryItem[Init]]
              }
            }
          // matches `.everyMapKey`
          case Apply(Apply(TypeApply(Ident("everyMapKey"), _), List(t2)), List(IsMapOf(_, _, _))) =>
            unpackSelects(t2).map { init =>
              import init.Underlying as Init
              new ExistentialPath {
                type Underlying = runtime.Path.EveryMapKey[Init]
                val Underlying: Type[runtime.Path.EveryMapKey[Init]] = Type.of[runtime.Path.EveryMapKey[Init]]
              }
            }
          // matches `.everyMapValue`
          case Apply(Apply(TypeApply(Ident("everyMapValue"), _), List(t2)), List(IsMapOf(_, _, _))) =>
            unpackSelects(t2).map { init =>
              import init.Underlying as Init
              new ExistentialPath {
                type Underlying = runtime.Path.EveryMapValue[Init]
                val Underlying: Type[runtime.Path.EveryMapValue[Init]] = Type.of[runtime.Path.EveryMapValue[Init]]
              }
            }
          // matches `someFunctionName` - not allowed
          case f @ Apply(_, _) => Left(arbitraryFunctionNotAllowed(f, t))
          case _               => Left(invalidSelectorErrorMessage(t))
        }
        unpackSelects(selects)
      case Inlined(_, _, block) => parse(block)
      case _                    => Left(invalidSelectorErrorMessage(t))
    }

    private def invalidSelectorErrorMessage(t: Tree): String =
      s"The path expression has to be a single chain of calls on the original input, got: ${t.show(using Printer.TreeAnsiCode)}"

    private def arbitraryFunctionNotAllowed(f: Tree, t: Tree): String =
      s"The path expression has to be a single chain of calls on the original input, got operation other than value extraction: ${f
          .show(using Printer.TreeAnsiCode)} in: ${t
          .show(using Printer.TreeAnsiCode)}"

    private def ignoringInputNotAllowed(i: Tree, t: Tree): String =
      s"The path expression has to be a single chain of calls on the original input, got external identifier: ${i.show(
          using Printer.TreeAnsiCode
        )} in: ${t.show(using Printer.TreeAnsiCode)}"
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
          bodyOpt match {
            case Some(body) =>
              extractParams(body) match {
                case Left(_)     => Right(head)
                case Right(tail) => Right(head ++ tail)
              }
            case None => Right(head)
          }
        case Block(List(defdef), Closure(_, _)) => extractParams(defdef)
        case Block(Nil, singleTerm)             => extractParams(singleTerm)
        case Inlined(_, _, block)               => extractParams(block)
        case _                                  => Left(invalidConstructor(t))
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

      ApplyParams(using head, tail)
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

      ApplyParam(using
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
      case Right(ctorType) => f(using ctorType.Underlying)
      case Left(error)     => report.errorAndAbort(error, Position.ofMacroExpansion)
    }
}
