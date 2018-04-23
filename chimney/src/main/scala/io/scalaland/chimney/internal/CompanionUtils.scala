package io.scalaland.chimney.internal

import scala.reflect.macros.{runtime, blackbox}
import scala.tools.nsc.Global

trait CompanionUtils {

  val c: blackbox.Context

  // Copied from Magnolia: https://github.com/propensive/magnolia/blob/master/core/shared/src/main/scala/globalutil.scala

  // From Shapeless: https://github.com/milessabin/shapeless/blob/master/core/src/main/scala/shapeless/generic.scala#L698
  // Cut-n-pasted (with most original comments) and slightly adapted from
  // https://github.com/scalamacros/paradise/blob/c14c634923313dd03f4f483be3d7782a9b56de0e/plugin/src/main/scala/org/scalamacros/paradise/typechecker/Namers.scala#L568-L613
  def patchedCompanionRef(c: blackbox.Context)(tpe: c.Type): c.Tree = {
    // see https://github.com/scalamacros/paradise/issues/7
    // also see https://github.com/scalamacros/paradise/issues/64

    val global = c.universe.asInstanceOf[Global]
    val typer = c.asInstanceOf[runtime.Context].callsiteTyper.asInstanceOf[global.analyzer.Typer]
    val ctx = typer.context
    val globalType = tpe.asInstanceOf[global.Type]
    val original = globalType.typeSymbol
    val owner = original.owner
    val companion = original.companion.orElse {
      import global.{abort => aabort, _}
      implicit class PatchedContext(ctx: global.analyzer.Context) {
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

      ctx.patchedLookup(original.name.companionName, owner) suchThat { sym =>
        (original.isTerm || sym.hasModuleFlag) && (sym isCoDefinedWith original)
      }
    }

    global.gen.mkAttributedRef(globalType.prefix, companion).asInstanceOf[c.Tree]
  }
}
