package io.scalaland.chimney.internal.compiletime.dsl.utils

import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

private[chimney] trait DslMacroUtils {

  val c: blackbox.Context

  import c.universe.*

  implicit final protected class TreeOps(tree: Tree) {

    def addOverride(data: Tree): Tree =
      q"_root_.io.scalaland.chimney.internal.runtime.WithRuntimeDataStore.update($tree, $data)"

    def asInstanceOfExpr[A: WeakTypeTag]: Tree =
      q"$tree.asInstanceOf[${weakTypeOf[A]}]"
  }

  private trait ExistentialString {
    type Underlying <: String
    val Underlying: c.WeakTypeTag[Underlying]
  }
  private object ExistentialString {

    def apply(fieldName: TermName): ExistentialString = new ExistentialString {
      type Underlying = String
      val Underlying: WeakTypeTag[String] =
        c.WeakTypeTag(c.internal.constantType(Constant(fieldName.decodedName.toString)))
    }
  }

  private trait ExistentialPath {
    type Underlying <: runtime.Path
    val Underlying: c.WeakTypeTag[Underlying]
  }
  private object ExistentialPath {

    def parse(t: Tree): Either[String, ExistentialPath] = t match {
      case q"(${vd: ValDef}) => $selects" =>
        def unpackSelects(selects: Tree): Either[String, ExistentialPath] = selects match {
          case idt: Ident if vd.name == idt.name =>
            Right(new ExistentialPath {
              type Underlying = runtime.Path.Root
              val Underlying: WeakTypeTag[runtime.Path.Root] = weakTypeTag[runtime.Path.Root]
            })
          case _: Ident                          => Left(ignoringInputNotAllowed(t))
          case Apply(select @ Select(_, _), Nil) => unpackSelects(select)
          case Apply(_, _)                       => Left(arbitraryFunctionNotAllowed(t))
          case Select(t2, fieldName: TermName) =>
            unpackSelects(t2).map { init =>
              val name = ExistentialString(fieldName)

              def applyTypes[Init <: runtime.Path: c.WeakTypeTag, FieldName <: String: c.WeakTypeTag] =
                new ExistentialPath {
                  type Underlying = runtime.Path.Select[Init, FieldName]
                  val Underlying: WeakTypeTag[runtime.Path.Select[Init, FieldName]] =
                    weakTypeTag[runtime.Path.Select[Init, FieldName]]
                }
              applyTypes(init.Underlying, name.Underlying)
            }
          case _ => Left(invalidSelectorErrorMessage(t))
        }

        unpackSelects(selects)
      case _ => Left(invalidSelectorErrorMessage(t))
    }

    private def invalidSelectorErrorMessage(selectorTree: Tree): String =
      s"Invalid selector expression: $selectorTree"

    private def arbitraryFunctionNotAllowed(selectorTree: Tree): String =
      s"Invalid selector expression - only vals, and nullary defs allowed: $selectorTree"

    private def ignoringInputNotAllowed(selectorTree: Tree): String =
      s"Invalid selector expression - only input value can be extracted from: $selectorTree"
  }

  private trait ExistentialCtor {
    type Underlying <: runtime.ArgumentLists
    implicit val Underlying: WeakTypeTag[Underlying]
  }
  private object ExistentialCtor {

    def parse(t: Tree): Either[String, ExistentialCtor] = {
      def extractParams(t: Tree): Either[String, List[List[ValDef]]] = t match {
        case Function(params, tail) =>
          extractParams(tail) match {
            case Left(_)     => Right(List(params))
            case Right(tail) => Right(params :: tail)
          }
        // Eta-expansion wrapper in Scala 2.12
        case Block(Nil, term) => extractParams(term)
        case _                => Left(invalidConstructor(t))
      }

      extractParams(t).map { params =>
        new ExistentialCtor {
          type Underlying = runtime.ArgumentLists
          implicit val Underlying: WeakTypeTag[runtime.ArgumentLists] = paramsToType(params)
        }
      }
    }

    private def paramsToType(paramsLists: List[List[ValDef]]): WeakTypeTag[runtime.ArgumentLists] =
      paramsLists
        .map { paramList =>
          paramList.foldRight[WeakTypeTag[? <: runtime.ArgumentList]](weakTypeTag[runtime.ArgumentList.Empty])(
            constructArgumentListType
          )
        }
        .foldRight[WeakTypeTag[? <: runtime.ArgumentLists]](weakTypeTag[runtime.ArgumentLists.Empty])(
          constructArgumentListsType
        )
        .asInstanceOf[WeakTypeTag[runtime.ArgumentLists]]

    private def constructArgumentListsType(
        head: WeakTypeTag[? <: runtime.ArgumentList],
        tail: WeakTypeTag[? <: runtime.ArgumentLists]
    ): WeakTypeTag[? <: runtime.ArgumentLists] = {
      object ApplyParams {
        def apply[Head <: runtime.ArgumentList: WeakTypeTag, Tail <: runtime.ArgumentLists: WeakTypeTag]
            : WeakTypeTag[runtime.ArgumentLists.List[Head, Tail]] =
          weakTypeTag[runtime.ArgumentLists.List[Head, Tail]]
      }

      ApplyParams(head, tail)
    }

    private def constructArgumentListType(
        t: ValDef,
        args: WeakTypeTag[? <: runtime.ArgumentList]
    ): WeakTypeTag[? <: runtime.ArgumentList] = {
      object ApplyParam {
        def apply[ParamName <: String: WeakTypeTag, ParamType: WeakTypeTag, Args <: runtime.ArgumentList: WeakTypeTag]
            : WeakTypeTag[runtime.ArgumentList.Argument[ParamName, ParamType, Args]] =
          weakTypeTag[runtime.ArgumentList.Argument[ParamName, ParamType, Args]]
      }

      ApplyParam(
        c.WeakTypeTag(c.internal.constantType(Constant(t.name.decodedName.toString))),
        c.WeakTypeTag(t.tpt.tpe),
        args
      )
    }

    private def invalidConstructor(t: Tree): String =
      s"Expected function, instead got: ${Console.MAGENTA}$t${Console.RESET}: ${Console.MAGENTA}${t.tpe}${Console.RESET}"
  }

  // If we try to do:
  //
  //   implicit val toPath = fieldName.Underlying
  //   someMethod[SomeType[toPath.Underlying]
  //
  // compiler will generate type containing... fieldName.Underlying instead of using implicit, and it'll start printing:
  //
  //   Macro expansion contains free term variable fieldName defined by withFieldConstImpl in ....
  //   Have you forgotten to use splice when splicing this variable into a reifee?
  //   If you have troubles tracking free term variables, consider using -Xlog-free-terms
  //
  // Using type parameters instead of path-dependent types make compiler use the implicit as intended.
  protected trait ApplyFieldNameType {
    def apply[A <: runtime.Path: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelector(t: Tree): WeakTypeTag[?] =
      apply(extractSelectorAsType(t).Underlying)

    private def extractSelectorAsType(t: Tree): ExistentialPath =
      ExistentialPath.parse(t).fold(error => c.abort(c.enclosingPosition, error), path => path)
  }
  protected trait ApplyFieldNameTypes {
    def apply[A <: runtime.Path: WeakTypeTag, B <: runtime.Path: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelectors(t1: Tree, t2: Tree): WeakTypeTag[?] = {
      val (e1, e2) = extractSelectorsAsTypes(t1, t2)
      apply(e1.Underlying, e2.Underlying)
    }

    private def extractSelectorsAsTypes(t1: Tree, t2: Tree): (ExistentialPath, ExistentialPath) =
      (ExistentialPath.parse(t1), ExistentialPath.parse(t2)) match {
        case (Right(path1), Right(path2)) => (path1, path2)
        case (Left(error1), Left(error2)) => c.abort(c.enclosingPosition, s"Invalid selectors:\n$error1\n$error2")
        case (Left(error), _)             => c.abort(c.enclosingPosition, error)
        case (_, Left(error))             => c.abort(c.enclosingPosition, error)
      }
  }

  /** Workaround for Java Enums, see [[io.scalaland.chimney.internal.runtime.RefinedJavaEnum]]. */
  protected trait ApplyFixedCoproductType {

    def apply[FixedSubtype: WeakTypeTag]: Tree

    final def applyJavaEnumFixFromClosureSignature[Subtype: WeakTypeTag](f: Tree): Tree =
      if (weakTypeOf[Subtype].typeSymbol.isJavaEnum) {
        val Subtype = weakTypeOf[Subtype]
        val Function(List(ValDef(_, _, lhs: TypeTree, _)), _) = f
        lhs.original match {
          // Java enum value in Scala 2.13
          case SingletonTypeTree(Literal(Constant(t: TermSymbol))) => apply(refineJavaEnum[Subtype](t))
          // Java enum value in Scala 2.12
          case SingletonTypeTree(Select(t, n)) if t.isTerm =>
            val t = Subtype.companion.decls
              .find(_.name == n)
              .getOrElse(
                c.abort(
                  c.enclosingPosition,
                  s"Can't find symbol `$n` among the declarations of `${Subtype.typeSymbol.fullName}`"
                )
              )
            apply(refineJavaEnum[Subtype](t))
          case _ => apply(weakTypeTag[Subtype])
        }
      } else apply(weakTypeTag[Subtype])

    private def refineJavaEnum[Subtype: WeakTypeTag](t: Symbol): WeakTypeTag[?] = {
      object ApplyInstanceName {
        def apply[InstanceName <: String: WeakTypeTag]
            : WeakTypeTag[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Subtype, InstanceName]] =
          weakTypeTag[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Subtype, InstanceName]]
      }

      ApplyInstanceName(c.WeakTypeTag(c.internal.constantType(Constant(t.name.decodedName.toString))))
    }
  }

  protected trait ApplyConstructorType {

    def apply[Ctor <: runtime.ArgumentLists: WeakTypeTag]: Tree

    final def applyFromBody(f: Tree): Tree = {
      val ctorType = extractCtorType(f)
      apply(ctorType.Underlying)
    }

    private def extractCtorType(f: Tree): ExistentialCtor = ExistentialCtor.parse(f) match {
      case Right(ctor) => ctor
      case Left(error) => c.abort(c.enclosingPosition, error)
    }
  }
}
