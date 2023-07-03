package io.scalaland.chimney.internal.compiletime

private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final override protected type ExprPromiseName = TermName

  protected object ExprPromise extends ExprPromiseModule {

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
      c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$")
    private def freshTermName(tpe: c.Type): ExprPromiseName =
      freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
    private def freshTermName(srcPrefixTree: Expr[?]): ExprPromiseName =
      freshTermName(toFieldName(srcPrefixTree))

    // TODO: document why it that a thing
    // undo the encoding of freshTermName
    private def toFieldName[A](srcPrefixTree: Expr[A]): String =
      srcPrefixTree.tree.toString.replaceAll("\\$\\d+", "").replace("$u002E", ".")
  }

  protected object PatternMatchCase extends PatternMatchCaseModule {

    def matchOn[From: Type, To: Type](src: Expr[From], cases: List[PatternMatchCase[To]]): Expr[To] = {
      val casesTrees = cases.map { case PatternMatchCase(someFrom, usage, fromName, _) =>
        ExistentialType.use(someFrom) { implicit SomeFrom: Type[someFrom.Underlying] =>
          val markUsed = Expr.suppressUnused(c.Expr[someFrom.Underlying](q"$fromName"))
          cq"""$fromName : $SomeFrom => { $markUsed; $usage }"""
        }
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
          ExistentialExpr.use(initialValue) { tpe => expr => q"def $name: $tpe = $expr" }
        case (name, initialValue, DefnType.Lazy) =>
          ExistentialExpr.use(initialValue) { tpe => expr => q"lazy val $name: $tpe = $expr" }
        case (name, initialValue, DefnType.Val) =>
          ExistentialExpr.use(initialValue) { tpe => expr => q"val $name: $tpe = $expr" }
        case (name, initialValue, DefnType.Var) =>
          ExistentialExpr.use(initialValue) { tpe => expr => q"var $name: $tpe = $expr" }
      }.toList
      c.Expr[To](q"..$statements; $expr")
    }

    def setVal[To: Type](name: ExprPromiseName): Expr[To] => Expr[Unit] = value => c.Expr[Unit](q"$name = $value")
  }
}
