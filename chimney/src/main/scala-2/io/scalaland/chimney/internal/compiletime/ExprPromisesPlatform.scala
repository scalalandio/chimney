package io.scalaland.chimney.internal.compiletime

import scala.annotation.nowarn

@nowarn("msg=The outer reference in this type test cannot be checked at run time.")
private[compiletime] trait ExprPromisesPlatform extends ExprPromises { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  type ExprPromiseName = TermName

  protected object ExprPromise extends ExprPromiseModule {

    protected def provideFreshName[From: Type](nameGenerationStrategy: NameGenerationStrategy): ExprPromiseName =
      nameGenerationStrategy match {
        case NameGenerationStrategy.FromPrefix(src) => freshTermName(src)
        case NameGenerationStrategy.FromType        => freshTermName(Type[From])
        case NameGenerationStrategy.FromExpr(expr)  => freshTermName(expr)
      }

    protected def createRefToName[From: Type](name: ExprPromiseName): Expr[From] = c.Expr[From](q"$name")

    def createAndUseLambda[From: Type, To: Type, B](
        fromName: ExprPromiseName,
        to: Expr[To],
        usage: Expr[From => To] => B
    ): B =
      usage(c.Expr[From => To](q"($fromName: ${Type[From]}) => $to"))

    private def freshTermName(srcPrefixTree: Expr[?]): ExprPromiseName =
      freshTermName(toFieldName(srcPrefixTree))
    private def freshTermName(tpe: c.Type): ExprPromiseName =
      freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
    private def freshTermName(prefix: String): ExprPromiseName =
      c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$")

    private def toFieldName(srcPrefixTree: Expr[?]): String = {
      // TODO: document why it that a thing
      // undo the encoding of freshTermName
      srcPrefixTree.tree.toString
        .replaceAll("\\$\\d+", "")
        .replace("$u002E", ".")
    }
  }

  protected object PrependValsTo extends PrependValsToModule {

    def initializeVals[To](vals: Vector[(ExprPromiseName, ComputedExpr)], expr: Expr[To]): Expr[To] = {
      val statements = vals.map { case (name, initialValue) =>
        ComputedExpr.use(initialValue) { (tpe, expr) => q"val $name: $tpe = $expr" }
      }.toList
      c.Expr[To](q"..$statements; $expr")
    }
  }
}
