package io.scalaland.chimney.internal.compiletime.dsl.utils

import scala.reflect.macros.blackbox

trait MacroUtils {

  val c: blackbox.Context

  import c.universe.*

  def freshTermName(srcPrefixTree: Tree): c.universe.TermName = {
    freshTermName(toFieldName(srcPrefixTree))
  }

  def freshTermName(tpe: Type): c.universe.TermName = {
    freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
  }

  def freshTermName(prefix: String): c.universe.TermName = {
    c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$")
  }

  def toFieldName(srcPrefixTree: Tree): String = {
    // undo the encoding of freshTermName
    srcPrefixTree
      .toString()
      .replaceAll("\\$\\d+", "")
      .replace("$u002E", ".")
  }

  implicit class NameOps(n: Name) {
    def toNameConstant: Constant = Constant(n.decodedName.toString)
    def toSingletonTpe: ConstantType = c.internal.constantType(toNameConstant)
  }

  implicit class TypeOps(t: Type) {

    def applyTypeArgs(args: Type*): Type = {
      val ee = t.etaExpand
      // $COVERAGE-OFF$
      if (ee.typeParams.size != args.size) {
        val een = ee.typeParams.size
        val argsn = args.size
        c.abort(c.enclosingPosition, s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
      }
      // $COVERAGE-ON$
      ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
    }
  }

  implicit class ClassSymbolOps(cs: ClassSymbol) {

    def subclasses: List[Symbol] =
      cs.knownDirectSubclasses.toList.flatMap { subclass =>
        val asClass = subclass.asClass
        if (asClass.isTrait && asClass.isSealed) {
          asClass.subclasses
        } else {
          List(subclass)
        }
      }
  }

  // $COVERAGE-OFF$
  implicit class TreeOps(t: Tree) {

//    def debug: Tree = {
//      println("TREE: " + t)
//      println("RAW:  " + showRaw(t))
//      t
//    }

    def extractSelectorFieldName: TermName = {
      extractSelectorFieldNameOpt.getOrElse {
        c.abort(c.enclosingPosition, invalidSelectorErrorMessage(t))
      }
    }

    def extractSelectorFieldNameOpt: Option[TermName] = {
      t match {
        case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: TermName}" if vd.name == idt.name =>
          Some(fieldName)
        case _ =>
          None
      }
    }
  }

  implicit class PairTreeOps(pair: (Tree, Tree)) {
    def extractSelectorsOrAbort: (TermName, TermName) = {
      val (selectorTree1, selectorTree2) = pair

      (selectorTree1.extractSelectorFieldNameOpt, selectorTree2.extractSelectorFieldNameOpt) match {
        case (Some(fieldName1), Some(fieldName2)) =>
          (fieldName1, fieldName2)
        case (None, Some(_)) =>
          c.abort(c.enclosingPosition, invalidSelectorErrorMessage(selectorTree1))
        case (Some(_), None) =>
          c.abort(c.enclosingPosition, invalidSelectorErrorMessage(selectorTree2))
        case (None, None) =>
          val err1 = invalidSelectorErrorMessage(selectorTree1)
          val err2 = invalidSelectorErrorMessage(selectorTree2)
          c.abort(c.enclosingPosition, s"Invalid selectors:\n$err1\n$err2")
      }
    }
  }

  private def invalidSelectorErrorMessage(selectorTree: Tree): String = {
    s"Invalid selector expression: $selectorTree"
  }
}
