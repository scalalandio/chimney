package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final override protected type ExprPromiseName = TermName

  protected object ExprPromise extends ExprPromiseModule {

    object platformSpecific {

      def freshTermName(prefix: String): TermName =
        // Scala 3 generate prefix$macro$[n] while Scala 2 prefix[n] and we want to align the behavior
        c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$macro$")
    }

    // made public for ChimneyExprsPlatform: Transformer.lift and PartialTransformer.lift
    def provideFreshName[From: Type](
        nameGenerationStrategy: NameGenerationStrategy,
        usageHint: UsageHint
    ): ExprPromiseName =
      nameGenerationStrategy match {
        case NameGenerationStrategy.FromPrefix(src) => freshTermName(src)
        case NameGenerationStrategy.FromType        => freshTermName(Type[From].tpe)
        case NameGenerationStrategy.FromExpr(expr)  => freshTermName(expr)
      }

    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From] =
      c.Expr[From](q"$name")

    def createLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To]
    ): Expr[From => To] =
      c.Expr[From => To](q"($fromName: ${Type[From]}) => $to")

    def createLambda2[From: Type, From2: Type, To: Type, B](
        fromName: ExprPromiseName,
        from2Name: ExprPromiseName,
        to: Expr[To]
    ): Expr[(From, From2) => To] =
      c.Expr[(From, From2) => To](q"($fromName: ${Type[From]}, $from2Name: ${Type[From2]}) => $to")

    private def freshTermName(prefix: String): ExprPromiseName =
      platformSpecific.freshTermName(prefix)
    private def freshTermName(tpe: c.Type): ExprPromiseName =
      freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
    private def freshTermName(srcPrefixTree: Expr[?]): ExprPromiseName =
      freshTermName(srcPrefixTree.tree.toString)
  }

  protected object PatternMatchCase extends PatternMatchCaseModule {

    def matchOn[From: Type, To: Type](src: Expr[From], cases: List[PatternMatchCase[To]]): Expr[To] = {
      val casesTrees = cases.map { case PatternMatchCase(someFrom, usage, fromName) =>
        import someFrom.Underlying as SomeFrom
        val markUsed = Expr.suppressUnused(c.Expr[SomeFrom](q"$fromName"))
        cq"""$fromName : $SomeFrom => { $markUsed; $usage }"""
      }
      c.Expr[To](q"$src match { case ..$casesTrees }")
    }
  }

  protected object PrependDefinitionsTo extends PrependDefinitionsToModule {

    def initializeDefns[To: Type](
        vals: Vector[(ExprPromiseName, ExistentialExpr, DefnType)],
        expr: Expr[To]
    ): Expr[To] = {
      val statements = vals.map {
        case (name, initialValue, DefnType.Def) =>
          q"def $name: ${initialValue.Underlying} = ${initialValue.value}"
        case (name, initialValue, DefnType.Lazy) =>
          q"lazy val $name: ${initialValue.Underlying} = ${initialValue.value}"
        case (name, initialValue, DefnType.Val) =>
          q"val $name: ${initialValue.Underlying} = ${initialValue.value}"
        case (name, initialValue, DefnType.Var) =>
          q"var $name: ${initialValue.Underlying} = ${initialValue.value}"
      }.toList
      c.Expr[To](q"..$statements; $expr")
    }

    def setVal[To: Type](name: ExprPromiseName): Expr[To] => Expr[Unit] = value => c.Expr[Unit](q"$name = $value")
  }

  protected object DefCache extends DefCacheModule {

    import ExprPromise.{provideFreshName, NameGenerationStrategy, UsageHint}

    def apply[F[_]: fp.DirectStyle]: DefCache[F] = new DefCache[F] {

      protected def define1[In1: Type, Out: Type](name: String): Define[Expr[In1], Expr[Out]] =
        new Define[Expr[In1], Expr[Out]] {
          private val termName = provideFreshName[In1](NameGenerationStrategy.FromPrefix(name), UsageHint.None)
          private val call: Expr[In1] => Expr[Out] = in1 => c.Expr[Out](q"$termName($in1)")

          def apply(body: Expr[In1] => Expr[Out]): Def = new Def {
            private val defdef = fp.DirectStyle[F].asyncUnsafe {
              val in1 = provideFreshName[In1](NameGenerationStrategy.FromType, UsageHint.None)
              q"""
              def $termName($in1: ${Type[In1]}): ${Type[Out]} = ${body(c.Expr[In1](q"$in1"))}
              """
            }
            def prependDef[A: Type](expr: Expr[A]): Expr[A] = c.Expr[A](
              q"""
              ${fp.DirectStyle[F].awaitUnsafe(defdef)}
              $expr
              """
            )
            def cast[A]: A = { (in1: Expr[In1]) =>
              fp.DirectStyle[F].awaitUnsafe(defdef) // re-fail
              call(in1)
            }.asInstanceOf[A]
          }
          val pending: PendingDef = new PendingDef {
            def cast[A]: A = call.asInstanceOf[A]
          }
        }
      protected def define2[In1: Type, In2: Type, Out: Type](
          name: String
      ): Define[(Expr[In1], Expr[In2]), Expr[Out]] = new Define[(Expr[In1], Expr[In2]), Expr[Out]] {
        private val termName = provideFreshName[In1](NameGenerationStrategy.FromPrefix(name), UsageHint.None)
        private val call: (Expr[In1], Expr[In2]) => Expr[Out] = (in1, in2) => c.Expr[Out](q"$termName($in1, $in2)")

        def apply(body: ((Expr[In1], Expr[In2])) => Expr[Out]): Def = new Def {
          private val defdef = fp.DirectStyle[F].asyncUnsafe {

            val in1 = provideFreshName[In1](NameGenerationStrategy.FromType, UsageHint.None)
            val in2 = provideFreshName[In2](NameGenerationStrategy.FromType, UsageHint.None)
            q"""
            def $termName($in1: ${Type[In1]}, $in2: ${Type[In2]}): ${Type[Out]} = ${body(
                (c.Expr[In1](q"$in1"), c.Expr[In2](q"$in2"))
              )}
            """
          }
          def prependDef[A: Type](expr: Expr[A]): Expr[A] = c.Expr[A](
            q"""
            ${fp.DirectStyle[F].awaitUnsafe(defdef)}
            $expr
            """
          )
          def cast[A]: A = { (in1: Expr[In1], in2: Expr[In2]) =>
            fp.DirectStyle[F].awaitUnsafe(defdef) // re-fail
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
