package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override protected type ExprPromiseName = Symbol

  protected object ExprPromise extends ExprPromiseModule {

    object platformSpecific {

      object freshTerm {
        // workaround to contain @experimental from polluting the whole codebase
        private val impl = quotes.reflect.Symbol.getClass.getMethod("freshName", classOf[String])

        def apply(prefix: String): String = impl.invoke(quotes.reflect.Symbol, prefix).asInstanceOf[String]
      }
    }

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

    private def freshTermName[A: Type](
        prefix: String,
        usageHint: UsageHint
    ): ExprPromiseName = Symbol
      .newVal(
        Symbol.spliceOwner,
        platformSpecific.freshTerm(prefix),
        TypeRepr.of[A],
        usageHint match {
          case UsageHint.None => Flags.EmptyFlags
          case UsageHint.Lazy => Flags.Lazy
          case UsageHint.Var  => Flags.Mutable
        },
        Symbol.noSymbol
      )
    private def freshTermName[A: Type](usageHint: UsageHint): ExprPromiseName = {
      // To keep things consistent with Scala 2 for e.g. "Some[String]" we should generate "some" rather than
      // "some[string], so we need to remove types applied to type constructor.
      val repr = TypeRepr.of[A] match {
        case AppliedType(repr, _) => repr
        case otherwise            => otherwise
      }
      freshTermName(repr.show(using Printer.TypeReprShortCode).toLowerCase, usageHint)
    }
    private def freshTermName[A: Type](expr: Expr[?], usageHint: UsageHint): ExprPromiseName =
      freshTermName[A](expr.asTerm.show(using Printer.TreeCode), usageHint)
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
      cases.map { case PatternMatchCase(someFrom, usage, fromName) =>
        import someFrom.Underlying as SomeFrom
        // Unfortunately, we cannot do
        //   case $fromName: $SomeFrom => $using
        // because bind and val have different flags in Symbol. We need to do something like
        //   case $bindName: $SomeFrom => val $fromName = $bindName; $using
        val bindName = Symbol.newBind(
          Symbol.spliceOwner,
          ExprPromise.platformSpecific.freshTerm(
            TypeRepr.of[SomeFrom].show(using Printer.TypeReprShortCode).toLowerCase
          ),
          Flags.EmptyFlags,
          TypeRepr.of(using SomeFrom)
        )
        // We're constructing:
        // '{ val fromName = bindName; val _ = fromName; ${ usage } }
        val body = Block(
          List(
            ValDef(fromName, Some(Ref(bindName))), // not a Term, so we cannot use Expr.block
            Expr.suppressUnused(Ref(fromName).asExprOf[SomeFrom]).asTerm
          ),
          usage.asTerm
        )

        val sym = TypeRepr.of[SomeFrom].typeSymbol
        if sym.flags.is(Flags.Enum) && (sym.flags.is(Flags.JavaStatic) || sym.flags.is(Flags.StableRealizable)) then
        // Scala 3's enums' parameterless cases are vals with type erased, so w have to match them by value
        // case arg @ Enum.Value => ...
        CaseDef(Bind(bindName, Ident(sym.termRef)), None, body)
        else
          // case arg : Enum.Value => ...
          CaseDef(Bind(bindName, Typed(Wildcard(), TypeTree.of[SomeFrom])), None, body)
      }
    ).asExprOf[To]
  }

  protected object DefCache extends DefCacheModule {

    def apply[F[_]: fp.DirectStyle]: DefCache[F] = new DefCache[F] {
      import ExprPromise.platformSpecific.freshTerm

      protected def define1[In1: Type, Out: Type](name: String): Define[Expr[In1], Expr[Out]] =
        new Define[Expr[In1], Expr[Out]] {
          private val symbol =
            Symbol.newMethod(
              Symbol.spliceOwner,
              freshTerm(name),
              MethodType(List(freshTerm("in1")))(_ => List(TypeRepr.of[In1]), _ => TypeRepr.of[Out])
            )
          private val call: Expr[In1] => Expr[Out] = in1 =>
            Ref(symbol).appliedToArgss(List(List(in1.asTerm))).asExprOf[Out]

          def apply(body: Expr[In1] => Expr[Out]): Def = new Def {
            private val defdef = fp.DirectStyle[F].asyncUnsafe {
              DefDef(
                symbol,
                {
                  case List(List(in1: Term)) =>
                    Some(body(in1.asExprOf[In1]).asTerm.changeOwner(symbol))
                  case _ => None
                }
              )
            }
            def prependDef[A: Type](expr: Expr[A]): Expr[A] = Block(
              List(fp.DirectStyle[F].awaitUnsafe(defdef)),
              expr.asTerm
            ).changeOwner(Symbol.spliceOwner).asExprOf[A]
            def cast[A]: A = { (in1: Expr[In1]) =>
              val _ = fp.DirectStyle[F].awaitUnsafe(defdef) // re-fail
              call(in1)
            }.asInstanceOf[A]
          }
          val pending: PendingDef = new PendingDef {
            var isRecursive: Boolean = false
            def cast[A]: A = call.asInstanceOf[A]
          }
        }
      protected def define2[In1: Type, In2: Type, Out: Type](name: String): Define[(Expr[In1], Expr[In2]), Expr[Out]] =
        new Define[(Expr[In1], Expr[In2]), Expr[Out]] {
          private val symbol = Symbol.newMethod(
            Symbol.spliceOwner,
            freshTerm(name),
            MethodType(List(freshTerm("in1"), freshTerm("in2")))(
              _ => List(TypeRepr.of[In1], TypeRepr.of[In2]),
              _ => TypeRepr.of[Out]
            )
          )
          private val call: (Expr[In1], Expr[In2]) => Expr[Out] = (in1, in2) =>
            Ref(symbol).appliedToArgss(List(List(in1.asTerm, in2.asTerm))).asExprOf[Out]

          def apply(body: ((Expr[In1], Expr[In2])) => Expr[Out]): Def = new Def {
            private val defdef = fp.DirectStyle[F].asyncUnsafe {
              DefDef(
                symbol,
                {
                  case List(List(in1: Term, in2: Term)) =>
                    Some(body((in1.asExprOf[In1], in2.asExprOf[In2])).asTerm.changeOwner(symbol))
                  case _ => None
                }
              )
            }
            def prependDef[A: Type](expr: Expr[A]): Expr[A] = Block(
              List(fp.DirectStyle[F].awaitUnsafe(defdef)),
              expr.asTerm
            ).changeOwner(Symbol.spliceOwner).asExprOf[A]
            def cast[A]: A = { (in1: Expr[In1], in2: Expr[In2]) =>
              val _ = fp.DirectStyle[F].awaitUnsafe(defdef) // re-fail
              call(in1, in2)
            }.asInstanceOf[A]
          }
          val pending: PendingDef = new PendingDef {
            def cast[A]: A = call.asInstanceOf[A]
          }
        }
    }
  }
}
