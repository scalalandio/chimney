package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  type ExprPromiseName = Symbol

  protected object ExprPromise extends ExprPromiseModule {

    protected def provideFreshName[From: Type](nameGenerationStrategy: NameGenerationStrategy): ExprPromiseName =
      nameGenerationStrategy match {
        case NameGenerationStrategy.FromPrefix(src) => freshTermName(src)
        case NameGenerationStrategy.FromType        => freshTermName(Type[From])
        case NameGenerationStrategy.FromExpr(expr)  => freshTermName(expr)
      }

    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From] =
      Ref(name).asExpr.asInstanceOf[Expr[From]]

    def createAndUseLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To],
        usage: Expr[From => To] => B
    ): B =
      // Block(
      //  List(
      //    DefDef(
      //      "$anonfun",
      //      List(TermParamClause(List(ValDef("a", TypeIdent("Int"), None)))),
      //      Inferred(),
      //      Some(Block(Nil, Apply(Select(Ident("a"), "toString"), Nil)))
      //    )
      //  ),
      //  Closure(Ident("$anonfun"), None)
      // )

      usage(
        Lambda(
          owner = Symbol.spliceOwner,
          tpe = MethodType(
            paramNames = List(fromName.name)
          )(
            paramInfosExp = _ => List(TypeRepr.of[From]),
            resultTypeExp = _ => TypeRepr.of[To]
          ),
          rhsFn = (_, _) => to.asTerm
        ).asExprOf[From => To]
      )

    private def freshTermName(srcPrefixTree: Expr[?]): ExprPromiseName =
      // TODO: check if that is still a thing
      freshTermName(
        srcPrefixTree.asTerm.toString
          .replaceAll("\\$\\d+", "")
          .replace("$u002E", ".")
      )
    private def freshTermName(tpe: Type[?]): ExprPromiseName =
      freshTermName(TypeRepr.of(using tpe).toString.toLowerCase)
    private def freshTermName[T: Type](prefix: String): ExprPromiseName = {
      val freshName = freshTermImpl.generate(prefix)
      Symbol.newVal(Symbol.spliceOwner, freshName, TypeRepr.of[T], Flags.EmptyFlags, Symbol.noSymbol)
    }
    private lazy val freshTermImpl: FreshTerm = new FreshTerm
  }

  protected object PrependValsTo extends PrependValsToModule {

    def initializeVals[To](vals: Vector[(ExprPromiseName, ComputedExpr)], expr: Expr[To]): Expr[To] = {
      val statements = vals.map { case (name, cexpr) =>
        ComputedExpr.use(cexpr) { (_, expr) => ValDef(name, Some(expr.asTerm)) }
      }.toList
      Block(statements, expr.asTerm).asExprOf(using Expr.typeOf(expr))
    }
  }

  // workaround to contain @experimental from polluting the whole codebase
  private class FreshTerm(using q: quoted.Quotes) {
    val freshTerm = q.reflect.Symbol.getClass.getMethod("freshName", classOf[String])

    def generate(prefix: String): String = freshTerm.invoke(q.reflect.Symbol, prefix).asInstanceOf[String]
  }
}
