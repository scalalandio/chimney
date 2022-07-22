package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox

trait MacroUtils extends CompanionUtils {

  val c: blackbox.Context

  import c.universe._

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
    def toNameLiteral: Literal = Literal(toNameConstant)
    def toSingletonTpe: ConstantType = c.internal.constantType(toNameConstant)
  }

  type TypeConstructorTag[F[_]] = WeakTypeTag[F[Unit]]

  object TypeConstructorTag {
    def apply[F[_]: TypeConstructorTag]: Type = {
      weakTypeOf[F[Unit]].typeConstructor
    }
  }

  implicit class TypeOps(t: Type) {

    def applyTypeArg(arg: Type): Type = {
      val ee = t.etaExpand
      if (ee.typeParams.size != 1) {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, s"Type $ee must have single type parameter!")
        // $COVERAGE-ON$
      }
      ee.finalResultType.substituteTypes(ee.typeParams, List(arg))
    }

    def applyTypeArgs(args: Type*): Type = {
      val ee = t.etaExpand
      if (ee.typeParams.size != args.size) {
        // $COVERAGE-OFF$
        val een = ee.typeParams.size
        val argsn = args.size
        c.abort(c.enclosingPosition, s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
        // $COVERAGE-ON$
      }
      ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
    }

    def isValueClass: Boolean =
      t <:< typeOf[AnyVal] && !primitives.exists(_ =:= t)

    def isCaseClass: Boolean =
      t.typeSymbol.isCaseClass

    def isSealedClass: Boolean =
      t.typeSymbol.classSymbolOpt.exists(_.isSealed)

    def caseClassParams: Seq[MethodSymbol] = {
      t.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
          m.asMethod
      }.toSeq
    }

    def getterMethods: Seq[MethodSymbol] = {
      t.decls.collect {
        case m: MethodSymbol if m.isPublic && (m.isGetter || m.isParameterless) =>
          m
      }.toSeq
    }

    def beanSetterMethods: Seq[MethodSymbol] = {
      t.members.collect { case m: MethodSymbol if m.isBeanSetter => m }.toSeq
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

    def collectionInnerTpe: Type = {
      t.typeArgs match {
        case List(unaryInnerT) => unaryInnerT
        case List(innerT1, innerT2) =>
          c.typecheck(tq"($innerT1, $innerT2)", c.TYPEmode).tpe
        // $COVERAGE-OFF$
        case Nil =>
          c.abort(c.enclosingPosition, "Collection type must have type parameters!")
        case _ =>
          c.abort(c.enclosingPosition, "Collection types with more than 2 type arguments are not supported!")
        // $COVERAGE-ON$
      }
    }
  }

  implicit class SymbolOps(s: Symbol) {

    def classSymbolOpt: Option[ClassSymbol] =
      if (s.isClass) Some(s.asClass) else None

    def isCaseClass: Boolean =
      classSymbolOpt.exists(_.isCaseClass)

    lazy val caseClassDefaults: Map[String, Tree] = {
      s.typeSignature
      classSymbolOpt
        .flatMap { classSymbol =>
          val classType = classSymbol.toType
          val companionSym = companionSymbol(classType)
          val primaryFactoryMethod = companionSym.asModule.info.decl(TermName("apply")).alternatives.lastOption
          primaryFactoryMethod.foreach(_.asMethod.typeSignature)
          val primaryConstructor = classSymbol.primaryConstructor
          val headParamListOpt = primaryConstructor.asMethod.typeSignature.paramLists.headOption.map(_.map(_.asTerm))

          headParamListOpt.map { headParamList =>
            headParamList.zipWithIndex.flatMap {
              case (param, idx) =>
                if (param.isParamWithDefault) {
                  val method = TermName("apply$default$" + (idx + 1))
                  Some(param.name.toString -> q"$companionSym.$method")
                } else {
                  None
                }
            }.toMap
          }
        }
        .getOrElse {
          // $COVERAGE-OFF$
          Map.empty
          // $COVERAGE-ON$
        }
    }

    def typeInSealedParent(parentTpe: Type): Type = {
      s.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

      val sEta = s.asType.toType.etaExpand
      sEta.finalResultType.substituteTypes(
        sEta.baseType(parentTpe.typeSymbol).typeArgs.map(_.typeSymbol),
        parentTpe.typeArgs
      )
    }
  }

  implicit class MethodSymbolOps(ms: MethodSymbol) {

    def canonicalName: String = {
      val name = ms.name.decodedName.toString
      if (isBeanSetter) {
        val stripedPrefix = name.drop(3)
        val lowerizedName = stripedPrefix.toCharArray
        lowerizedName(0) = lowerizedName(0).toLower
        new String(lowerizedName)
      } else {
        name
      }
    }

    def isBeanSetter: Boolean = {
      ms.isPublic &&
      ms.name.decodedName.toString.startsWith("set") &&
      ms.name.decodedName.toString.lengthCompare(3) > 0 &&
      ms.paramLists.lengthCompare(1) == 0 &&
      ms.paramLists.head.lengthCompare(1) == 0 &&
      ms.returnType == typeOf[Unit]
    }

    def resultTypeIn(site: Type): Type = {
      ms.typeSignatureIn(site).finalResultType
    }

    def beanSetterParamTypeIn(site: Type): Type = {
      ms.paramLists.head.head.typeSignatureIn(site)
    }

    def isParameterless: Boolean = {
      ms.paramLists.isEmpty || ms.paramLists == List(List())
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

    def extractSelectorFieldName: TermName = {
      extractSelectorFieldNameOpt.getOrElse {
        c.abort(c.enclosingPosition, "Invalid selector!")
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

    def convertCollection(TargetTpe: Type, InnerTpe: Type): Tree = {
      if (TargetTpe <:< typeOf[scala.collection.Map[_, _]] && scala.util.Properties.versionNumberString < "2.13") {
        q"$t.toMap"
      } else {
        q"$t.to(_root_.scala.Predef.implicitly[_root_.scala.collection.compat.Factory[$InnerTpe, $TargetTpe]])"
      }
    }

    def callTransform(input: Tree): Tree = {
      q"$t.transform($input)"
    }

    def callUnaryApply(argTree: Tree): Tree = {
      q"$t.apply($argTree)"
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
