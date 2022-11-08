package io.scalaland.chimney.internal.macros

import scala.reflect.macros.blackbox

trait TargetConstructorMacros extends Model {

  val c: blackbox.Context

  import c.universe._

  def mkNewClass(classTpe: Type, args: Iterable[Tree]): Tree = {
    q"new $classTpe(..$args)"
  }

  def mkNewJavaBean(classTpe: Type, argsMapping: Iterable[(Target, Tree)]): Tree = {

    val fn = freshTermName(classTpe)

    val objCreation = q"val $fn = new $classTpe"
    val setterInvocations = argsMapping.map {
      case (target, argTree) =>
        val setterName = TermName("set" + target.name.capitalize)
        q"$fn.$setterName($argTree)"
    }.toSeq

    q"{..${objCreation +: setterInvocations}; $fn}"
  }

  def mkCoproductInstance(
      transformerDefinitionPrefix: Tree,
      srcPrefixTree: Tree,
      instSymbol: Symbol,
      To: Type,
      derivationTarget: DerivationTarget
  ): Tree = {
    val instFullName = instSymbol.fullName
    val fullTargetName = To.typeSymbol.fullName
    val finalTpe = derivationTarget.targetType(To)
    q"""
      $transformerDefinitionPrefix
        .instances(($instFullName, $fullTargetName))
        .asInstanceOf[Any => $finalTpe]
        .apply($srcPrefixTree)
        .asInstanceOf[$finalTpe]
    """
  }

  def mkTransformerBodyTree0(derivationTarget: DerivationTarget)(targetValueTree: Tree): Tree = {
    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        targetValueTree
      case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        q"${wrapperSupportInstance}.pure($targetValueTree)"
    }
  }

  def mkTransformerBodyTree1(
      To: Type,
      target: Target,
      transformerBodyTree: TransformerBodyTree,
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Tree => Tree
  ): Tree = {
    mkTransformerBodyTree(To, Seq(target), Seq(transformerBodyTree), derivationTarget) {
      case Seq(innerTree) => mkTargetValueTree(innerTree)
    }
  }

  def mkTransformerBodyTree(
      To: Type,
      targets: Seq[Target],
      bodyTreeArgs: Seq[TransformerBodyTree],
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Seq[Tree] => Tree
  ): Tree = {
    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        mkTargetValueTree(bodyTreeArgs.map(_.tree))
      case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        val (pureArgs, wrappedArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (wrappedArgs.isEmpty) {
          q"$wrapperSupportInstance.pure(${mkTargetValueTree(bodyTreeArgs.map(_.tree))})"
        } else {

          val (wrappedTargets, wrappedBodyTrees) = wrappedArgs.unzip
          val wrappedTrees = wrappedBodyTrees.map(_.tree)
          val productF = wrappedTrees.reduceRight { (tree, rest) =>
            q"$wrapperSupportInstance.product($tree, $rest)"
          }

          val argNames = wrappedTargets.map(target => freshTermName(target.name))
          val argTypes = wrappedTargets.map(_.tpe)
          val bindTreesF = argNames.map { termName =>
            Bind(termName, Ident(termNames.WILDCARD))
          }
          val productType = argTypes.map(tpe => tq"$tpe").reduceRight[Tree]((param, tree) => tq"($param, $tree)")
          val patternF = bindTreesF.reduceRight[Tree]((param, tree) => pq"(..${List(param, tree)})")

          val patRefArgsMap = (wrappedTargets zip argNames).map { case (target, argName) => target -> q"$argName" }.toMap
          val pureArgsMap = pureArgs.map { case (target, bt)                             => target -> bt.tree }.toMap
          val argsMap = pureArgsMap ++ patRefArgsMap

          val updatedArgs = targets.map(argsMap)

          q"""
            $wrapperSupportInstance.map[$productType, $To](
              $productF,
              { case $patternF => ${mkTargetValueTree(updatedArgs)} }
            )
          """
        }
    }
  }
}
