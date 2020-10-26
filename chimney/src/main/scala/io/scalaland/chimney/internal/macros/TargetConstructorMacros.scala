package io.scalaland.chimney.internal.macros

import scala.reflect.macros.blackbox

trait TargetConstructorMacros extends Model {

  val c: blackbox.Context

  import c.universe._

  def mkNewClass(classTpe: Type, args: Iterable[Tree]): Tree = {
    q"new $classTpe(..$args)"
  }

  def mkNewJavaBean(classTpe: Type, argsMapping: Iterable[(Target, Tree)]): Tree = {

    val fn = TermName(c.freshName(classTpe.typeSymbol.name.decodedName.toString.toLowerCase))

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
      wrapperType: Option[Type]
  ): Tree = {
    val instFullName = instSymbol.fullName
    val fullTargetName = To.typeSymbol.fullName
    val finalTpe = wrapperType.map(_.applyTypeArg(To)).getOrElse(To)
    q"""
      $transformerDefinitionPrefix
        .instances(($instFullName, $fullTargetName))
        .asInstanceOf[Any => $finalTpe]
        .apply($srcPrefixTree)
        .asInstanceOf[$finalTpe]
    """
  }

  def mkTransformerBodyTree0(
      config: TransformerConfig
  )(
      targetValueTree: Tree
  ): Tree = {
    if (config.wrapperType.isEmpty) {
      targetValueTree
    } else {
      q"${config.wrapperSupportInstance}.pure($targetValueTree)"
    }
  }

  def mkTransformerBodyTree1(
      To: Type,
      target: Target,
      transformerBodyTree: TransformerBodyTree,
      config: TransformerConfig
  )(
      mkTargetValueTree: Tree => Tree
  ): Tree = {
    mkTransformerBodyTree(To, Seq(target), Seq(transformerBodyTree), config) {
      case Seq(innerTree) => mkTargetValueTree(innerTree)
    }
  }

  def mkTransformerBodyTree(
      To: Type,
      targets: Seq[Target],
      bodyTreeArgs: Seq[TransformerBodyTree],
      config: TransformerConfig
  )(
      mkTargetValueTree: Seq[Tree] => Tree
  ): Tree = {
    if (config.wrapperType.isEmpty) {
      mkTargetValueTree(bodyTreeArgs.map(_.tree))
    } else {
      val fSupport = config.wrapperSupportInstance

      val (wrappedArgs, pureArgs) = (targets zip bodyTreeArgs).partition(_._2.isWrapped)

      if (wrappedArgs.isEmpty) {
        q"$fSupport.pure(${mkTargetValueTree(bodyTreeArgs.map(_.tree))})"
      } else {

        val (wrappedTargets, wrappedBodyTrees) = wrappedArgs.unzip
        val wrappedTrees = wrappedBodyTrees.map(_.tree)
        val productF = wrappedTrees.reduceRight { (tree, rest) =>
          q"$fSupport.product($tree, $rest)"
        }

        val argNames = wrappedTargets.map(target => TermName(c.freshName(target.name)))
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
          $fSupport.map[$productType, $To](
            $productF,
            { case $patternF => ${mkTargetValueTree(updatedArgs)} }
          )
        """
      }
    }
  }
}
