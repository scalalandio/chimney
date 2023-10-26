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
          case _: Ident =>
            Left(invalidSelectorErrorMessage(t)) // TODO: error for foo => bar.fieldName
          case Select(t2, fieldName: TermName) =>
            unpackSelects(t2).map { instance =>
              val name = ExistentialString(fieldName)

              def applyTypes[FieldName <: String: c.WeakTypeTag, Instance <: runtime.Path: c.WeakTypeTag] =
                new ExistentialPath {
                  type Underlying = runtime.Path.Select[FieldName, Instance]
                  val Underlying: WeakTypeTag[runtime.Path.Select[FieldName, Instance]] =
                    weakTypeTag[runtime.Path.Select[FieldName, Instance]]
                }
              applyTypes(name.Underlying, instance.Underlying)
            }
          case _ => Left(invalidSelectorErrorMessage(t))
        }

        unpackSelects(selects)
      case _ => Left(invalidSelectorErrorMessage(t))
    }

    private def invalidSelectorErrorMessage(selectorTree: Tree): String = s"Invalid selector expression: $selectorTree"
  }

  // If we try to do:
  //
  //   implicit val fieldNameT = fieldName.Underlying
  //   someMethod[SomeType[fieldNameT.Underlying]
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

    final def applyFromSelector(t: c.Tree): WeakTypeTag[?] =
      apply(extractSelectorAsType(t).Underlying)

    private def extractSelectorAsType(t: Tree): ExistentialPath =
      ExistentialPath.parse(t).fold(error => c.abort(c.enclosingPosition, error), path => path)
  }
  protected trait ApplyFieldNameTypes {
    def apply[A <: runtime.Path: WeakTypeTag, B <: runtime.Path: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelectors(t1: c.Tree, t2: c.Tree): WeakTypeTag[?] = {
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

    def apply[FixedInstance: WeakTypeTag]: Tree

    def applyJavaEnumFixFromClosureSignature[Inst: WeakTypeTag](f: Tree): Tree =
      if (weakTypeOf[Inst].typeSymbol.isJavaEnum) {
        val Inst = weakTypeOf[Inst]
        val Function(List(ValDef(_, _, lhs: TypeTree, _)), _) = f
        lhs.original match {
          // java enum value in Scala 2.13
          case SingletonTypeTree(Literal(Constant(t: TermSymbol))) => apply(refineJavaEnum[Inst](t))
          // java enum value in Scala 2.12
          case SingletonTypeTree(Select(t, n)) if t.isTerm =>
            val t = Inst.companion.decls
              .find(_.name == n)
              .getOrElse(
                c.abort(
                  c.enclosingPosition,
                  s"Can't find symbol `$n` among the declarations of `${Inst.typeSymbol.fullName}`"
                )
              )
            apply(refineJavaEnum[Inst](t))
          case _ => apply(weakTypeTag[Inst])
        }
      } else apply(weakTypeTag[Inst])

    private def refineJavaEnum[Inst: WeakTypeTag](t: Symbol): WeakTypeTag[?] = {
      object ApplyInstanceName {
        def apply[InstanceName <: String: WeakTypeTag]
            : WeakTypeTag[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Inst, InstanceName]] =
          weakTypeTag[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Inst, InstanceName]]
      }

      ApplyInstanceName(c.WeakTypeTag(c.internal.constantType(Constant(t.name.decodedName.toString))))
    }
  }
}
