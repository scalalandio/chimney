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
  trait ApplyFieldNameType {
    def apply[A <: String: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelector(t: c.Tree): WeakTypeTag[?] = apply(extractSelectorAsType(t).Underlying)
  }
  trait ApplyFieldNameTypes {
    def apply[A <: String: WeakTypeTag, B <: String: WeakTypeTag]: WeakTypeTag[?]

    final def applyFromSelectors(t1: c.Tree, t2: c.Tree): WeakTypeTag[?] = {
      val (e1, e2) = extractSelectorsAsTypes(t1, t2)
      apply(e1.Underlying, e2.Underlying)
    }
  }

  private trait ExistentialString {
    type Underlying <: String
    val Underlying: c.WeakTypeTag[Underlying]
  }

  private def extractSelectorAsType(t: Tree): ExistentialString = extractSelectorAsTypeOpt(t).getOrElse {
    c.abort(c.enclosingPosition, invalidSelectorErrorMessage(t))
  }

  private def extractSelectorsAsTypes(t1: Tree, t2: Tree): (ExistentialString, ExistentialString) =
    (extractSelectorAsTypeOpt(t1), extractSelectorAsTypeOpt(t2)) match {
      case (Some(fieldName1), Some(fieldName2)) =>
        (fieldName1, fieldName2)
      case (None, Some(_)) =>
        c.abort(c.enclosingPosition, invalidSelectorErrorMessage(t1))
      case (Some(_), None) =>
        c.abort(c.enclosingPosition, invalidSelectorErrorMessage(t2))
      case (None, None) =>
        val err1 = invalidSelectorErrorMessage(t1)
        val err2 = invalidSelectorErrorMessage(t2)
        c.abort(c.enclosingPosition, s"Invalid selectors:\n$err1\n$err2")
    }

  private def extractSelectorAsTypeOpt(t: Tree): Option[ExistentialString] = t match {
    case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: TermName}" if vd.name == idt.name =>
      Some(new ExistentialString {
        type Underlying = String
        val Underlying: WeakTypeTag[String] =
          c.WeakTypeTag(c.internal.constantType(Constant(fieldName.decodedName.toString)))
      })
    case _ =>
      None
  }

  private def invalidSelectorErrorMessage(selectorTree: Tree): String =
    s"Invalid selector expression: $selectorTree"

  /** Workaround for Java Enums, see [[io.scalaland.chimney.internal.runtime.RefinedJavaEnum]]. */
  def fixJavaEnumType[Inst: WeakTypeTag](f: Tree)(fixedCoproductType: ApplyFixedCoproductType): Tree =
    if (weakTypeOf[Inst].typeSymbol.isJavaEnum) {
      val Inst = weakTypeOf[Inst]
      val Function(List(ValDef(_, _, lhs: TypeTree, _)), _) = f
      lhs.original match {
        // java enum value in Scala 2.13
        case SingletonTypeTree(Literal(Constant(t: TermSymbol))) => fixedCoproductType(refineJavaEnum[Inst](t))
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
          fixedCoproductType(refineJavaEnum[Inst](t))
        case _ => fixedCoproductType(weakTypeTag[Inst])
      }
    } else fixedCoproductType(weakTypeTag[Inst])

  private def refineJavaEnum[Inst: WeakTypeTag](t: Symbol): WeakTypeTag[?] = {
    object ApplyInstanceName {
      def apply[InstanceName <: String: WeakTypeTag]
          : WeakTypeTag[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Inst, InstanceName]] =
        weakTypeTag[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[Inst, InstanceName]]
    }

    ApplyInstanceName(c.WeakTypeTag(c.internal.constantType(Constant(t.name.decodedName.toString))))
  }

  trait ApplyFixedCoproductType {

    def apply[FixedInstance: WeakTypeTag]: Tree
  }
}
