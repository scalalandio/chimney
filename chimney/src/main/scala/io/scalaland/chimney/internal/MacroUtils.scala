package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait MacroUtils {

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
        .map { classSymbol =>
          println(classSymbol)
          val companion = companionRef(classSymbol.toType)
          println(companion)
          val constructor = classSymbol.primaryConstructor.asMethod

          constructor.paramLists.head
            .map(_.asTerm)
            .zipWithIndex
            .flatMap {
              case (p, i) =>
                if (!p.isParamWithDefault) None
                else {
                  val getterName = TermName("apply$default$" + (i + 1))
                  Some(p.name.toString -> q"$companion.$getterName".debug)
                }
            }
            .toMap
        }
        .getOrElse(Map.empty)
    }
  }

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

  def companionRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val cSym = patchedCompanionSymbolOf(tpe.typeSymbol).asInstanceOf[global.Symbol]
    if (cSym != NoSymbol)
      global.gen.mkAttributedRef(pre, cSym).asInstanceOf[Tree]
    else
      Ident(tpe.typeSymbol.name.toTermName) // Attempt to refer to local companion
  }

  // Cut-n-pasted (with most original comments) and slightly adapted from
  // https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
  def patchedCompanionSymbolOf(original: Symbol): Symbol = {
    // see https://github.com/scalamacros/paradise/issues/7
    // also see https://github.com/scalamacros/paradise/issues/64

    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val typer = c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val ctx = typer.context
    val owner = original.owner

    import global.analyzer.Context

    original.companion.orElse {
      import global.{abort => aabort, _}
      implicit class PatchedContext(ctx: Context) {
        trait PatchedLookupResult { def suchThat(criterion: Symbol => Boolean): Symbol }
        def patchedLookup(name: Name, expectedOwner: Symbol) = new PatchedLookupResult {
          override def suchThat(criterion: Symbol => Boolean): Symbol = {
            var res: Symbol = NoSymbol
            var ctx = PatchedContext.this.ctx
            while (res == NoSymbol && ctx.outer != ctx) {
              // NOTE: original implementation says `val s = ctx.scope lookup name`
              // but we can't use it, because Scope.lookup returns wrong results when the lookup is ambiguous
              // and that triggers https://github.com/scalamacros/paradise/issues/64
              val s = {
                val lookupResult = ctx.scope.lookupAll(name).filter(criterion).toList
                lookupResult match {
                  case Nil          => NoSymbol
                  case List(unique) => unique
                  case _ =>
                    aabort(s"unexpected multiple results for a companion symbol lookup for $original#{$original.id}")
                }
              }
              if (s != NoSymbol && s.owner == expectedOwner)
                res = s
              else
                ctx = ctx.outer
            }
            res
          }
        }
      }
      ctx
        .patchedLookup(original.asInstanceOf[global.Symbol].name.companionName, owner.asInstanceOf[global.Symbol])
        .suchThat(
          sym =>
            (original.isTerm || sym.hasModuleFlag) &&
              (sym isCoDefinedWith original.asInstanceOf[global.Symbol])
        )
        .asInstanceOf[c.universe.Symbol]
    }
  }
}
