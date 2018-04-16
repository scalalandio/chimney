package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait MacroUtils {

  val c: blackbox.Context

  import c.universe._

  implicit class TypeOps(t: Type) {

    def isValueClass: Boolean =
      t <:< typeOf[AnyVal] && !primitives.exists(_ =:= t)

    def isCaseClass: Boolean =
      t.typeSymbol.classSymbolOpt.exists(_.isCaseClass)

    def caseClassParams: Iterable[MethodSymbol] =
      t.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
          m.asMethod
      }
  }

  implicit class SymbolOps(s: Symbol) {

    def classSymbolOpt: Option[ClassSymbol] =
      if(s.isClass) Some(s.asClass) else None
  }

  implicit class TreeOps(t: Tree) {

    def debug: Tree = {
      println("TREE: " + t)
      println("RAW:  " + showRaw(t))
      t
    }

    def insertToBlock(tree: Tree): Tree = t match {
      case Typed(tt, _) =>
        tt.insertToBlock(tree)
      case Block(stats, expr) =>
        Block(stats :+ tree, expr)
      case other =>
        Block(List(tree), other)
    }
  }

  private val primitives = Set(
    typeOf[Double],
    typeOf[Float],
    typeOf[Short],
    typeOf[Byte],
    typeOf[Int],
    typeOf[Long],
    typeOf[Char],
    typeOf[Boolean],
    typeOf[Unit]
  )
}
