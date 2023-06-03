package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  type ExprPromiseName = Symbol

  protected object ExprPromise extends ExprPromiseModule {

    protected def provideFreshName[From: Type](nameGenerationStrategy: NameGenerationStrategy): ExprPromiseName =
      nameGenerationStrategy match {
        case NameGenerationStrategy.FromPrefix(src) => freshTermName[From](src)
        case NameGenerationStrategy.FromType        => freshTermName[From]
        case NameGenerationStrategy.FromExpr(expr)  => freshTermName[From](expr)
      }

    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From] =
      Ref(name).asExpr.asExprOf[From]

    def createAndUseLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To],
        use: Expr[From => To] => B
    ): B = use('{ (param: From) =>
      ${ PrependValsTo.initializeVals[To](vals = Vector(fromName -> ComputedExpr('{ param })), expr = to) }
    })

    def createAndUseLambda2[From: Type, From2: Type, To: Type, B](
        fromName: ExprPromiseName,
        from2Name: ExprPromiseName,
        to: Expr[To],
        use: Expr[(From, From2) => To] => B
    ): B = use('{ (param: From, param2: From2) =>
      ${
        PrependValsTo.initializeVals[To](
          vals = Vector(fromName -> ComputedExpr('{ param }), from2Name -> ComputedExpr('{ param2 })),
          expr = to
        )
      }
    })

    private val freshTerm: FreshTerm = new FreshTerm
    private def freshTermName[A: Type](prefix: String): ExprPromiseName =
      Symbol.newVal(Symbol.spliceOwner, freshTerm.generate(prefix), TypeRepr.of[A], Flags.EmptyFlags, Symbol.noSymbol)
    private def freshTermName[A: Type]: ExprPromiseName =
      freshTermName(TypeRepr.of[A].show(using Printer.TypeReprShortCode).toLowerCase)
    private def freshTermName[A: Type](srcPrefixTree: Expr[?]): ExprPromiseName =
      freshTermName[A](toFieldName(srcPrefixTree))

    // TODO: check if that is still a thing
    // undo the encoding of freshTermName
    private def toFieldName[A](srcPrefixTree: Expr[A]): String =
      srcPrefixTree.asTerm.toString.replaceAll("\\$\\d+", "").replace("$u002E", ".")
  }

  protected object PrependValsTo extends PrependValsToModule {

    def initializeVals[To: Type](vals: Vector[(ExprPromiseName, ComputedExpr)], expr: Expr[To]): Expr[To] = {
      val statements = vals.map { case (name, cexpr) =>
        ComputedExpr.use(cexpr) { (_, expr) => ValDef(name, Some(expr.asTerm)) }
      }.toList
      Block(statements, expr.asTerm).asExprOf[To]
    }
  }

  // TODO: consult with Janek Chyb if this is necessary/safe
  // workaround to contain @experimental from polluting the whole codebase
  private class FreshTerm(using q: quoted.Quotes) {
    private val impl = q.reflect.Symbol.getClass.getMethod("freshName", classOf[String])

    def generate(prefix: String): String = impl.invoke(q.reflect.Symbol, prefix).asInstanceOf[String]
  }
}
