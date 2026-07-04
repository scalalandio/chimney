package io.scalaland.chimney.internal.compiletime

import hearth.fp.effect.MIO
import scala.quoted.Quotes

/** Scala 3 entrypoint of the macro cake: concrete macro classes extend this class. It also hosts the Scala 3 overrides
  * of the compat workarounds (see [[MacroCommonsCompat]]).
  */
abstract private[compiletime] class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions {

  import quotes.reflect.*

  // Workaround to contain @experimental Symbol.freshName from polluting the whole codebase.
  private lazy val freshName = quotes.reflect.Symbol.getClass.getMethod("freshName", classOf[String])
  private def freshTerm(prefix: String): String =
    freshName.invoke(quotes.reflect.Symbol, prefix).asInstanceOf[String]

  private def annotatedValExpr[A: Type](annotation: Term, namePrefix: String)(expr: Expr[A]): Expr[A] = {
    val name = Symbol.newVal(
      Symbol.spliceOwner,
      freshTerm(namePrefix),
      AnnotatedType(TypeRepr.of[A], annotation),
      Flags.EmptyFlags,
      Symbol.noSymbol
    )

    Block(
      List(ValDef(name, Some(expr.asTerm.changeOwner(name)))),
      Ref(name)
    ).asExprOf[A]
  }

  /** Fresh `FromType`-named val symbol + its `Ref`, minted against the macro-entry `Quotes` (used by the
    * instance-builder overrides below - "promise a name, derive against it, bind it in the quote").
    */
  private def freshValSymbolOf[A](prefix0: Option[String])(using scala.quoted.Type[A]): (Symbol, Expr[A]) = {
    val prefix = prefix0.getOrElse {
      (TypeRepr.of[A] match {
        case AppliedType(repr, _) => repr
        case repr                 => repr
      }).show(using Printer.TypeReprShortCode).toLowerCase
    }
    val sym = Symbol.newVal(Symbol.spliceOwner, freshTerm(prefix), TypeRepr.of[A], Flags.EmptyFlags, Symbol.noSymbol)
    (sym, Ref(sym).asExprOf[A])
  }

  /** `{ val <sym> = <bound>; <body> }` - binds the promised val symbols to the instance-method parameters. */
  private def bindPromisedVals[B](bindings: List[(Symbol, Term)], body: Expr[B])(using
      scala.quoted.Type[B]
  ): Expr[B] =
    Block(
      bindings.map { case (sym, bound) => ValDef(sym, Some(bound.changeOwner(sym))) },
      body.asTerm
    ).asExprOf[B]

  /** Scala 3 override of `ChimneyExprs.transformerInstanceCompat` (hearth#318): derives the body FIRST (plain MIO - no
    * direct-style `await`, whose executor-thread hop makes splice-scoped exprs escape their evaluation and trips
    * `-Xcheck-macros`' ScopeException), then constructs the instance quote binding the promised val to the method
    * parameter.
    */
  override protected def transformerInstanceCompat[From: Type, To: Type](
      deriveBody: Expr[From] => MIO[Expr[To]]
  ): MIO[Expr[io.scalaland.chimney.Transformer[From, To]]] = {
    given tFrom: scala.quoted.Type[From] = Type[From].asInstanceOf[scala.quoted.Type[From]]
    given tTo: scala.quoted.Type[To] = Type[To].asInstanceOf[scala.quoted.Type[To]]
    val (srcSym, srcRef) = freshValSymbolOf[From](None)
    deriveBody(srcRef).map { body =>
      '{
        new io.scalaland.chimney.Transformer[From, To] {
          def transform(src: From): To = ${
            bindPromisedVals[To](List(srcSym -> ('src).asTerm), body)
          }
        }
      }
    }
  }

  /** Scala 3 override of `ChimneyExprs.partialTransformerInstanceCompat` - see [[transformerInstanceCompat]]. */
  override protected def partialTransformerInstanceCompat[From: Type, To: Type](
      deriveBody: (Expr[From], Expr[Boolean]) => MIO[Expr[io.scalaland.chimney.partial.Result[To]]]
  ): MIO[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] = {
    given tFrom: scala.quoted.Type[From] = Type[From].asInstanceOf[scala.quoted.Type[From]]
    given tTo: scala.quoted.Type[To] = Type[To].asInstanceOf[scala.quoted.Type[To]]
    val (srcSym, srcRef) = freshValSymbolOf[From](None)
    val (failFastSym, failFastRef) = freshValSymbolOf[Boolean](Some("failFast"))
    deriveBody(srcRef, failFastRef).map { body =>
      '{
        new io.scalaland.chimney.PartialTransformer[From, To] {
          def transform(src: From, failFast: Boolean): io.scalaland.chimney.partial.Result[To] = ${
            bindPromisedVals[io.scalaland.chimney.partial.Result[To]](
              List(srcSym -> ('src).asTerm, failFastSym -> ('failFast).asTerm),
              body
            )
          }
        }
      }
    }
  }

  /** Scala 3 override of `ChimneyExprs.patcherInstanceCompat` - see [[transformerInstanceCompat]]. */
  override protected def patcherInstanceCompat[A: Type, Patch: Type](
      deriveBody: (Expr[A], Expr[Patch]) => MIO[Expr[A]]
  ): MIO[Expr[io.scalaland.chimney.Patcher[A, Patch]]] = {
    given tA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
    given tPatch: scala.quoted.Type[Patch] = Type[Patch].asInstanceOf[scala.quoted.Type[Patch]]
    val (objSym, objRef) = freshValSymbolOf[A](None)
    val (patchSym, patchRef) = freshValSymbolOf[Patch](None)
    deriveBody(objRef, patchRef).map { body =>
      '{
        new io.scalaland.chimney.Patcher[A, Patch] {
          def patch(obj: A, patch: Patch): A = ${
            bindPromisedVals[A](List(objSym -> ('obj).asTerm, patchSym -> ('patch).asTerm), body)
          }
        }
      }
    }
  }

  /** Scala 3 override of [[MacroCommonsCompat.withMacroEntryCtxCompat]]: delegates to Hearth's `withMacroEntryCtx`
    * (added in 0.4.1 as part of the hearth#317/#318 fix), which pins the macro-ENTRY `Quotes` as Cross-Quotes' active
    * context for the duration of the thunk.
    */
  override protected def withMacroEntryCtxCompat[T](thunk: => T): T = withMacroEntryCtx(thunk)

  /** Scala 3 override of [[MacroCommonsCompat.prependFreshValCompat]]: builds the `val` under the CURRENT
    * (Cross-Quotes-managed) `Quotes` so its owner matches the definitions produced by cross-quoted helpers inside the
    * same `Expr.splice`, and heals the body with `changeOwner(Symbol.spliceOwner)` (the same healing Hearth applies
    * internally, "Required by -Xcheck-macros to pass"). Hearth's own `ValDefs` is bound to the macro-entry `Quotes` and
    * would create a definition with a stale owner here.
    */
  override protected def prependFreshValCompat[A: Type, B: Type](value: Expr[A])(use: Expr[A] => Expr[B]): Expr[B] = {
    val q = CrossQuotes.ctx[scala.quoted.Quotes]
    given scala.quoted.Quotes = q
    import q.reflect.*
    given tA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
    given tB: scala.quoted.Type[B] = Type[B].asInstanceOf[scala.quoted.Type[B]]

    // FreshName.FromType-style prefix (like Hearth: type-constructor short name, lowercased).
    val prefix = (TypeRepr.of[A] match {
      case AppliedType(repr, _) => repr
      case repr                 => repr
    }).show(using Printer.TypeReprShortCode).toLowerCase

    val sym = Symbol.newVal(Symbol.spliceOwner, freshTerm(prefix), TypeRepr.of[A], Flags.EmptyFlags, Symbol.noSymbol)
    val ref: Expr[A] = Ref(sym).asExprOf[A].asInstanceOf[Expr[A]]
    val body = use(ref).asInstanceOf[scala.quoted.Expr[B]]
    Block(
      List(ValDef(sym, Some(value.asInstanceOf[scala.quoted.Expr[A]].asTerm.changeOwner(sym)))),
      body.asTerm.changeOwner(Symbol.spliceOwner)
    ).asExprOf[B].asInstanceOf[Expr[B]]
  }

  /** Hearth has no annotation-attaching API - the `AnnotatedType`-based implementation lives here (see
    * [[MacroCommonsCompat.nowarnExpr]]).
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[scala.annotation.nowarn].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "nowarnResult")(expr)
  }

  /** See [[nowarnExpr]]. */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[java.lang.SuppressWarnings].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "suppressWarningsResult")(expr)
  }
}
