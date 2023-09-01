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
        PrependDefinitionsTo.initializeDefns[To](
          vals = Vector((fromName, ExistentialExpr('{ param }), PrependDefinitionsTo.DefnType.Val)),
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
        PrependDefinitionsTo.initializeDefns[To](
          vals = Vector(
            (fromName, ExistentialExpr('{ param }), PrependDefinitionsTo.DefnType.Val),
            (from2Name, ExistentialExpr('{ param2 }), PrependDefinitionsTo.DefnType.Val)
          ),
          expr = to
        )
      }
    }

    import scala.util.chaining.*
    private def freshTermName[A: Type](
        prefix: String,
        usageHint: UsageHint,
        dropSuffix: Boolean = false
    ): ExprPromiseName = Symbol
      .newVal(
        Symbol.spliceOwner,
        FreshTerm.generate(prefix).pipe(name => if dropSuffix then dropMacroSuffix(name) else name),
        TypeRepr.of[A],
        usageHint match
          case UsageHint.None => Flags.EmptyFlags
          case UsageHint.Lazy => Flags.Lazy
          case UsageHint.Var  => Flags.Mutable
        ,
        Symbol.noSymbol
      )
    private def freshTermName[A: Type](usageHint: UsageHint): ExprPromiseName = {
      // To keep things consistent with Scala 2 for e.g. "Some[String]" we should generate "some" rather than
      // "some[string], so we need to remove types applied to type constructor.
      val repr = TypeRepr.of[A] match
        case AppliedType(repr, _) => repr
        case otherwise            => otherwise
      freshTermName(repr.show(using Printer.TypeReprShortCode).toLowerCase, usageHint, dropSuffix = true)
    }
    private def freshTermName[A: Type](expr: Expr[?], usageHint: UsageHint): ExprPromiseName =
      freshTermName[A](expr.asTerm.toString, usageHint, dropSuffix = true)

    // Undoes the encoding of freshTermName so that generated value would not contain $1, $2, ...
    // - this makes generated fresh names more readable as it prevents e.g. typename$macro$1, typename$macro$2
    private def dropMacroSuffix[A](freshName: String): String =
      freshName.replaceAll("\\$macro\\$\\d+", "")
  }

  protected object PrependDefinitionsTo extends PrependDefinitionsToModule {

    def initializeDefns[To: Type](
        vals: Vector[(ExprPromiseName, ExistentialExpr, DefnType)],
        expr: Expr[To]
    ): Expr[To] = {
      val statements = vals.map {
        case (name, expr, DefnType.Def) =>
          DefDef(name, _ => Some(expr.value.asTerm.changeOwner(name)))
        case (name, expr, _) =>
          // val/lazy val/var is handled by Symbol by flag provided by UsageHint
          ValDef(name, Some(expr.value.asTerm.changeOwner(name)))
      }.toList
      Block(statements, expr.asTerm).asExprOf[To]
    }

    def setVal[To: Type](name: ExprPromiseName): Expr[To] => Expr[Unit] = expr =>
      Assign(Ref(name), expr.asTerm).asExprOf[Unit]
  }

  protected object PatternMatchCase extends PatternMatchCaseModule {

    def matchOn[From: Type, To: Type](src: Expr[From], cases: List[PatternMatchCase[To]]): Expr[To] = Match(
      '{ ${ src.asTerm.changeOwner(Symbol.spliceOwner).asExprOf[From] }: @scala.unchecked }.asTerm,
      cases.map { case PatternMatchCase(someFrom, usage, fromName, isCaseObject) =>
        import someFrom.Underlying as SomeFrom
        // Unfortunately, we cannot do
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

        if TypeRepr.of[someFrom.Underlying].typeSymbol.flags.is(Flags.Enum | Flags.JavaStatic) then
          // Scala 3's enums' parameterless cases are vals with type erased, so w have to match them by value
          // case arg @ Enum.Value => ...
          CaseDef(Bind(bindName, Ident(TypeRepr.of[someFrom.Underlying].typeSymbol.termRef)), None, body)
        else if isCaseObject then
          // case objects are also matched by value but the tree for them is generated in a slightly different way
          // case arg @ Enum.Value => ...
          CaseDef(
            Bind(bindName, Ident(TypeRepr.of[someFrom.Underlying].typeSymbol.companionModule.termRef)),
            None,
            body
          )
        else
          // case arg : Enum.Value => ...
          CaseDef(Bind(bindName, Typed(Wildcard(), TypeTree.of[someFrom.Underlying])), None, body)
      }
    ).asExprOf[To]
  }

  // workaround to contain @experimental from polluting the whole codebase
  private object FreshTerm {
    private val impl = quotes.reflect.Symbol.getClass.getMethod("freshName", classOf[String])

    def generate(prefix: String): String = impl.invoke(quotes.reflect.Symbol, prefix).asInstanceOf[String]
  }
}
