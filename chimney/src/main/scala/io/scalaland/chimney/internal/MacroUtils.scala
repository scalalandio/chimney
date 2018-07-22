package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait MacroUtils extends CompanionUtils {

  val c: blackbox.Context

  import c.universe._

  implicit class TypeOps(t: Type) {

    def isValueClass: Boolean =
      t <:< typeOf[AnyVal] && !primitives.exists(_ =:= t)

    def isCaseClass: Boolean =
      t.typeSymbol.isCaseClass

    def isSealedClass: Boolean =
      t.typeSymbol.classSymbolOpt.exists(_.isSealed)

    def caseClassParams: Iterable[MethodSymbol] = {
      t.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
          m.asMethod
      }
    }

    def getterMethods: Iterable[MethodSymbol] = {
      t.decls.collect {
        case m: MethodSymbol if m.isGetter =>
          m.asMethod
      }
    }

    def valueClassMember: Option[MethodSymbol] = {
      t.decls.collectFirst {
        case m: MethodSymbol if m.isParamAccessor =>
          m.asMethod
      }
    }

    def singletonString: String = {
      t.asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
        .value
        .value
        .asInstanceOf[String]
    }
  }

  implicit class SymbolOps(s: Symbol) {

    def classSymbolOpt: Option[ClassSymbol] =
      if (s.isClass) Some(s.asClass) else None

    def isCaseClass: Boolean =
      classSymbolOpt.exists(_.isCaseClass)

    lazy val caseClassDefaults: Map[String, c.Tree] = {
      classSymbolOpt
        .flatMap { classSymbol =>
          val classType = classSymbol.toType
          val companionRef = patchedCompanionRef(c)(classType)
          val companionSym = companionRef.symbol.asModule.info
          val primaryFactoryMethod = companionSym.decl(TermName("apply")).alternatives.lastOption
          primaryFactoryMethod.foreach(_.asMethod.typeSignature)
          val primaryConstructor = classSymbol.primaryConstructor
          val headParamListOpt = primaryConstructor.asMethod.typeSignature.paramLists.headOption.map(_.map(_.asTerm))

          headParamListOpt.map { headParamList =>
            headParamList.zipWithIndex.flatMap {
              case (param, idx) =>
                if (param.isParamWithDefault) {
                  val method = TermName("apply$default$" + (idx + 1))
                  Some(param.name.toString -> q"$companionRef.$method")
                } else {
                  None
                }
            }.toMap
          }
        }
        .getOrElse(Map.empty)
    }
  }

  // $COVERAGE-OFF$
  implicit class TreeOps(t: Tree) {

    def debug: Tree = {
      println("TREE: " + t)
      println("RAW:  " + showRaw(t))
      t
    }

    def extractBlock: (List[Tree], Tree) = t match {
      case Typed(tt, _) =>
        tt.extractBlock
      case Block(stats, expr) =>
        (stats, expr)
      case other =>
        (Nil, other)
    }

    def extractStats: List[Tree] = t match {
      case Typed(tt, _) =>
        tt.extractStats
      case Block(stats, _) =>
        stats
      case _ =>
        Nil
    }

    def insertToBlock(tree: Tree): Tree = {
      val (stats, expr) = t.extractBlock
      Block(stats :+ tree, expr)
    }

    def extractSelectorFieldName: Name = {
      extractSelectorFieldNameOpt.getOrElse {
        c.abort(c.enclosingPosition, "Invalid selector!")
      }
    }

    def extractSelectorFieldNameOpt: Option[Name] = {
      t match {
        case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: Name}" if vd.name == idt.name =>
          Some(fieldName)
        case _ =>
          None
      }
    }
  }
  // $COVERAGE-ON$

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
