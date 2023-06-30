package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override protected type ExprPromiseName = Symbol

  protected object ExprPromise extends ExprPromiseModule {

    // made public for ProductType.parse
    def provideFreshName[From: Type](
        nameGenerationStrategy: NameGenerationStrategy,
        usageHint: UsageHint
    ): ExprPromiseName =
      nameGenerationStrategy match {
        case NameGenerationStrategy.FromPrefix(src) => freshTermName[From](src, usageHint)
        case NameGenerationStrategy.FromType        => freshTermName[From](usageHint)
        case NameGenerationStrategy.FromExpr(expr)  => freshTermName[From](expr, usageHint)
      }

    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From] = Ref(name).asExprOf[From]

    def createLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To]
    ): Expr[From => To] = '{ (param: From) =>
      ${
        PrependValsTo.initializeDefns[To](
          vals = Vector((fromName, ExistentialExpr('{ param }), PrependValsTo.DefnType.Val)),
          expr = to
        )
      }
    }

    def createLambda2[From: Type, From2: Type, To: Type, B](
        fromName: ExprPromiseName,
        from2Name: ExprPromiseName,
        to: Expr[To]
    ): Expr[(From, From2) => To] = '{ (param: From, param2: From2) =>
      ${
        PrependValsTo.initializeDefns[To](
          vals = Vector(
            (fromName, ExistentialExpr('{ param }), PrependValsTo.DefnType.Val),
            (from2Name, ExistentialExpr('{ param2 }), PrependValsTo.DefnType.Val)
          ),
          expr = to
        )
      }
    }

    private def freshTermName[A: Type](prefix: String, usageHint: UsageHint): ExprPromiseName = Symbol.newVal(
      Symbol.spliceOwner,
      FreshTerm.generate(prefix),
      TypeRepr.of[A],
      usageHint match
        case UsageHint.None => Flags.EmptyFlags
        case UsageHint.Lazy => Flags.Lazy
        case UsageHint.Var  => Flags.Mutable
      ,
      Symbol.noSymbol
    )
    private def freshTermName[A: Type](usageHint: UsageHint): ExprPromiseName =
      freshTermName(TypeRepr.of[A].show(using Printer.TypeReprShortCode).toLowerCase, usageHint)
    private def freshTermName[A: Type](expr: Expr[?], usageHint: UsageHint): ExprPromiseName =
      freshTermName[A](toFieldName(expr), usageHint)

    // TODO: check if that is still a thing
    // undo the encoding of freshTermName
    private def toFieldName[A](expr: Expr[A]): String =
      expr.asTerm.toString.replaceAll("\\$\\d+", "").replace("$u002E", ".")
  }

  protected object PrependValsTo extends PrependValsToModule {

    def initializeDefns[To: Type](
        vals: Vector[(ExprPromiseName, ExistentialExpr, DefnType)],
        expr: Expr[To]
    ): Expr[To] = {
      val statements = vals.map {
        case (name, eexpr, DefnType.Def) =>
          ExistentialExpr.use(eexpr) { _ => expr => DefDef(name, _ => Some(expr.asTerm)) }
        case (name, eexpr, _) =>
          // val/lazy val/var is handled by Symbol by flag provided by UsageHint
          ExistentialExpr.use(eexpr) { _ => expr => ValDef(name, Some(expr.asTerm)) }
      }.toList
      Block(statements, expr.asTerm).asExprOf[To]
    }

    def setVal[To: Type](name: ExprPromiseName): Expr[To] => Expr[Unit] = expr =>
      Assign(Ref(name), expr.asTerm).asExprOf[Unit]
  }

  protected object PatternMatchCase extends PatternMatchCaseModule {

    def matchOn[From: Type, To: Type](src: Expr[From], cases: List[PatternMatchCase[To]]): Expr[To] = Match(
      src.asTerm,
      cases.map { case PatternMatchCase(someFrom, usage, fromName, isCaseObject) =>
        ExistentialType.use(someFrom) { implicit SomeFrom: Type[someFrom.Underlying] =>
          // Unfortunatelly, we cannot do
          //   case $fromName: $SomeFrom => $using
          // because bind and val have different flags in Symbol. We need to do something like
          //   case $bindName: $SomeFrom => val $fromName = $bindName; $using
          val bindName = Symbol.newBind(
            Symbol.spliceOwner,
            FreshTerm.generate(TypeRepr.of(using SomeFrom).show(using Printer.TypeReprShortCode).toLowerCase),
            Flags.EmptyFlags,
            TypeRepr.of(using SomeFrom)
          )
          // We're constructing:
          // '{ val fromName = bindName; val _ = fromName; ${ usage } }
          val body = Block(
            List(
              ValDef(fromName, Some(Ref(bindName))), // not a Term, so we cannot use Expr.block
              Expr.suppressUnused(Ref(fromName).asExprOf[someFrom.Underlying]).asTerm
            ),
            usage.asTerm
          )

          // Scala 3's enums' parameterless cases are vals with type erased, so w have to match them by value
          if isCaseObject then
            // case arg @ Enum.Value => ...
            CaseDef(Bind(bindName, Ident(TypeRepr.of[someFrom.Underlying].typeSymbol.termRef)), None, body)
          else
            // case arg : Enum.Value => ...
            CaseDef(Bind(bindName, Typed(Wildcard(), TypeTree.of[someFrom.Underlying])), None, body)
        }
      }
    ).asExprOf[To]
  }

  // TODO: consult with Janek Chyb if this is necessary/safe
  // workaround to contain @experimental from polluting the whole codebase
  private object FreshTerm {
    private val impl = quotes.reflect.Symbol.getClass.getMethod("freshName", classOf[String])

    def generate(prefix: String): String = impl.invoke(quotes.reflect.Symbol, prefix).asInstanceOf[String]
  }
}
