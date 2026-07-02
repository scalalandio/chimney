package io.scalaland.chimney.internal.compiletime

import scala.quoted.Quotes

/** Scala 3 entrypoint of the Hearth-based macro cake.
  *
  * Mirrors the old `DefinitionsPlatform`/`DerivationPlatform` split: concrete macro classes will extend this class the
  * same way they extended `DefinitionsPlatform(using q)` before. Its main purpose right now is to prove that the whole
  * `compiletime` cake composes and compiles on Scala 3.
  */
abstract private[compiletime] class PlatformBridge(q: Quotes)
    extends hearth.MacroCommonsScala3(using q)
    with ChimneyDefinitions {

  import quotes.reflect.*

  // Workaround to contain @experimental Symbol.freshName from polluting the whole codebase (same trick as
  // macro-commons' ExprPromise.platformSpecific.freshTerm).
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
    * instance-builder overrides below - the old macro-commons "promise a name, derive against it, bind it in the quote"
    * pattern).
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

  /** Scala 3 override of `ChimneyExprs.transformerInstanceCompat`: derives the body FIRST (plain MIO - no direct-style
    * `await`, whose executor-thread hop makes splice-scoped exprs escape their evaluation and trips `-Xcheck-macros`'
    * ScopeException), then constructs the instance quote binding the promised val to the method parameter - the old
    * macro-commons `ExprPromise` pattern.
    */
  override protected def transformerInstanceCompat[From: Type, To: Type](
      deriveBody: Expr[From] => DerivationResult[Expr[To]]
  ): DerivationResult[Expr[io.scalaland.chimney.Transformer[From, To]]] = {
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
      deriveBody: (Expr[From], Expr[Boolean]) => DerivationResult[Expr[io.scalaland.chimney.partial.Result[To]]]
  ): DerivationResult[Expr[io.scalaland.chimney.PartialTransformer[From, To]]] = {
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
      deriveBody: (Expr[A], Expr[Patch]) => DerivationResult[Expr[A]]
  ): DerivationResult[Expr[io.scalaland.chimney.Patcher[A, Patch]]] = {
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

  /** Scala 3 override of `ProductTypes.emptyNamedTupleConstructorCompat`: Hearth 0.4.0's NamedTuple view does not
    * recognize `NamedTuple.Empty` - construct it as `EmptyTuple` like macro-commons did.
    */
  override protected def emptyNamedTupleConstructorCompat[A: Type]: Option[Product.Constructor[A]] = {
    given tA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
    if TypeRepr.of[A].dealias =:= TypeRepr.of[scala.NamedTuple.Empty].dealias then Some(
      Product.Constructor[A](
        scala.collection.immutable.ListMap.empty,
        _ => '{ EmptyTuple }.asExprOf[A].asInstanceOf[Expr[A]]
      )
    )
    else None
  }

  /** Scala 3 override of `ProductTypes.namedTupleGetterCompat`: macro-commons' original getters -
    * `in.asInstanceOf[(V1, ..., Vn)]._N` for arity < 23, `productElement(idx)` + cast for TupleXXL.
    */
  override protected def namedTupleGetterCompat[A: Type, Elem: Type](
      in: Expr[A],
      idx: Int,
      valueTypes: List[??]
  ): Expr[Elem] = {
    given tA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
    given tElem: scala.quoted.Type[Elem] = Type[Elem].asInstanceOf[scala.quoted.Type[Elem]]
    val inQ = in.asInstanceOf[scala.quoted.Expr[A]]
    val arity = valueTypes.size
    (if arity < 23 then {
       // tuple._1, tuple._2, ...
       val tupleSym = if arity == 1 then TypeRepr.of[Tuple1].classSymbol.get else defn.TupleClass(arity)
       val tupleType =
         tupleSym.typeRef.appliedTo(
           valueTypes.map(vt => TypeRepr.of(using vt.Underlying.asInstanceOf[scala.quoted.Type[Any]]))
         )
       tupleType.asType match {
         case '[tpe] => Select.unique('{ $inQ.asInstanceOf[tpe] }.asTerm, s"_${idx + 1}").asExprOf[Elem]
       }
     } else {
       // tupleXXL.productElement(n)
       '{ $inQ.asInstanceOf[scala.Product].productElement(${ scala.quoted.Expr(idx) }).asInstanceOf[Elem] }
     }).asInstanceOf[Expr[Elem]]
  }

  /** Scala 3 override of `ProductTypes.tupleXXLConstructorCompat`: Hearth 0.4.0's synthetic named-tuple constructor
    * emits an invalid application for TupleXXL arities - build `Tuple.fromIArray(IArray(...)).asInstanceOf[A]` like
    * macro-commons did.
    */
  override protected def tupleXXLConstructorCompat[A: Type](args: List[ExistentialExpr]): Expr[A] = {
    given tA: scala.quoted.Type[A] = Type[A].asInstanceOf[scala.quoted.Type[A]]
    val argExprs = args.map(_.value.asInstanceOf[scala.quoted.Expr[Any]])
    '{ Tuple.fromIArray(IArray(${ scala.quoted.Varargs(argExprs) }*)).asInstanceOf[A] }.asInstanceOf[Expr[A]]
  }

  /** Scala 3 override of [[MacroCommonsCompat.isEnumCaseValCompat]]: the old macro-commons `isCaseVal` formula - checks
    * `Case|Enum` (+ static/stable) on the type symbol OR the TERM symbol (parameterless enum cases carry their flags on
    * the term symbol; the type symbol is the enum class itself).
    */
  override protected def isEnumCaseValCompat[A: Type]: Boolean = {
    def attempt(sym: Symbol): Boolean =
      !sym.isNoSymbol && sym.flags.is(Flags.Case | Flags.Enum) &&
        (sym.flags.is(Flags.JavaStatic) || sym.flags.is(Flags.StableRealizable))
    val repr = TypeRepr.of[A](using Type[A].asInstanceOf[scala.quoted.Type[A]])
    attempt(repr.typeSymbol) || attempt(repr.termSymbol)
  }

  /** Scala 3 override of [[MacroCommonsCompat.isJavaEnumValueTermCompat]]: a Java enum VALUE type is a `TermRef` (or a
    * `JavaStatic`-flagged symbol); the enum CLASS type has neither.
    */
  override protected def isJavaEnumValueTermCompat[A: Type]: Boolean = {
    val repr = TypeRepr.of[A](using Type[A].asInstanceOf[scala.quoted.Type[A]])
    (!repr.termSymbol.isNoSymbol) || repr.typeSymbol.flags.is(Flags.JavaStatic)
  }

  /** Scala 3 override of [[MacroCommonsCompat.withMacroEntryCtxCompat]]: restores the macro-entry `Quotes` as
    * Cross-Quotes' active context for the duration of the thunk (no-op when it is already active).
    */
  override protected def withMacroEntryCtxCompat[T](thunk: => T): T =
    if CrossQuotes.ctx[Quotes] eq quotes then thunk
    else CrossQuotes.nestedCtx(using quotes)(thunk)

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

  /** macro-commons `Expr.nowarn` (Scala 3) - Hearth has no annotation-attaching API, so the old `AnnotatedType`-based
    * implementation lives here (see [[MacroCommonsCompat.nowarnExpr]]).
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[scala.annotation.nowarn].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "nowarnResult")(expr)
  }

  /** macro-commons `Expr.SuppressWarnings` (Scala 3) - see [[nowarnExpr]]. */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    val annotationSymbol: Symbol = TypeRepr.of[java.lang.SuppressWarnings].typeSymbol
    val annotation = Apply(
      Select(New(TypeIdent(annotationSymbol)), annotationSymbol.primaryConstructor),
      List(scala.quoted.Expr(warnings.toArray).asTerm)
    )
    annotatedValExpr[A](annotation, "suppressWarningsResult")(expr)
  }
}
