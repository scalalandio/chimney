package io.scalaland.chimney.internal.compiletime.dsl.utils

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

    def unapply(t: Tree): Option[ExistentialString] = t match {
      case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: TermName}" if vd.name == idt.name =>
        Some(new ExistentialString {
          type Underlying = String
          val Underlying: WeakTypeTag[String] =
            c.WeakTypeTag(c.internal.constantType(Constant(fieldName.decodedName.toString)))
        })
      case _ =>
        None
    }

    def invalidSelectorErrorMessage(selectorTree: Tree): String = s"Invalid selector expression: $selectorTree"
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
    def apply[A <: String: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelector(t: c.Tree): WeakTypeTag[?] = apply(extractSelectorAsType(t).Underlying)

    private def extractSelectorAsType(t: Tree): ExistentialString = ExistentialString.unapply(t).getOrElse {
      c.abort(c.enclosingPosition, ExistentialString.invalidSelectorErrorMessage(t))
    }
  }
  protected trait ApplyFieldNameTypes {
    def apply[A <: String: WeakTypeTag, B <: String: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelectors(t1: c.Tree, t2: c.Tree): WeakTypeTag[?] = {
      val (e1, e2) = extractSelectorsAsTypes(t1, t2)
      apply(e1.Underlying, e2.Underlying)
    }

    private def extractSelectorsAsTypes(t1: Tree, t2: Tree): (ExistentialString, ExistentialString) =
      (t1, t2) match {
        case (ExistentialString(fieldName1), ExistentialString(fieldName2)) =>
          (fieldName1, fieldName2)
        case (_, ExistentialString(_)) =>
          c.abort(c.enclosingPosition, ExistentialString.invalidSelectorErrorMessage(t1))
        case (ExistentialString(_), _) =>
          c.abort(c.enclosingPosition, ExistentialString.invalidSelectorErrorMessage(t2))
        case (_, _) =>
          val err1 = ExistentialString.invalidSelectorErrorMessage(t1)
          val err2 = ExistentialString.invalidSelectorErrorMessage(t2)
          c.abort(c.enclosingPosition, s"Invalid selectors:\n$err1\n$err2")
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
