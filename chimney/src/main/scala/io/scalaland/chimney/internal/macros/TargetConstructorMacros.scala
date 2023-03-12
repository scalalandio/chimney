package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.AssertUtils

import io.scalaland.chimney.internal.utils.DslMacroUtils

import scala.reflect.macros.blackbox
import scala.collection.compat.*

trait TargetConstructorMacros extends Model with DslMacroUtils with AssertUtils with GenTrees {

  val c: blackbox.Context

  import c.universe.*

  def mkNewClass(classTpe: Type, args: Iterable[Tree]): Tree = {
    q"new $classTpe(..$args)"
  }

  def mkNewJavaBean(classTpe: Type, argsMapping: Iterable[(Target, Tree)]): Tree = {

    val fn = freshTermName(classTpe)

    val objCreation = q"val $fn = new $classTpe"
    val setterInvocations = argsMapping.map { case (target, argTree) =>
      val setterName = TermName("set" + target.name.capitalize)
      q"$fn.$setterName($argTree)"
    }.toSeq

    q"{..${objCreation +: setterInvocations}; $fn}"
  }

  def mkCoproductInstance(
      transformerDefinitionPrefix: Tree,
      srcPrefixTree: Tree,
      To: Type,
      runtimeDataIndex: Int,
      derivationTarget: DerivationTarget
  ): DerivedTree = {
    val finalTpe = derivationTarget.targetType(To)
    val tree = q"""
      ${transformerDefinitionPrefix.accessRuntimeData(runtimeDataIndex)}
        .asInstanceOf[Any => $finalTpe]
        .apply($srcPrefixTree)
        .asInstanceOf[$finalTpe]
    """
    DerivedTree(tree, derivationTarget)
  }

  /**
    * Generate result tree for given transformer derivation target from a value tree
    *
    * @param derivationTarget decides about if/how the value tree is wrapped
    * @param valueTree value tree for which we build target result tree
    * @return potentially wrapped value tree
    */
  def mkTransformerBodyTree0(derivationTarget: DerivationTarget)(valueTree: Tree): Tree = {
    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        valueTree
      case DerivationTarget.PartialTransformer(_) =>
        Trees.PartialResult.value(valueTree)
      case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        q"${wrapperSupportInstance}.pure($valueTree)"
    }
  }

  // TODO: docs
  def mkDerivedBodyTree(derivationTarget: DerivationTarget)(derivedTree: DerivedTree): DerivedTree = {
    if (derivedTree.target == derivationTarget) {
      derivedTree // do nothing if targets match
    } else if (derivedTree.isTotalTarget) {
      derivedTree.mapTree(mkTransformerBodyTree0(derivationTarget)(_)) // we can always lift total tree
    } else {
      c.abort(
        c.enclosingPosition,
        s"Unsupported lifting requested: from ${derivedTree.target} to $derivationTarget (tree: ${derivedTree.tree})"
      )
    }
  }

  // TODO: docs
  def mkTransformerBodyTree1(
      To: Type,
      target: Target,
      transformerBodyTree: DerivedTree,
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Tree => Tree
  ): DerivedTree = {
    mkTransformerBodyTree(To, Seq(target), Seq(transformerBodyTree), derivationTarget) { case Seq(innerTree) =>
      mkTargetValueTree(innerTree)
    }
  }

  /** Constructs tree body that constructs target `To` type (possibly wrapped depending on derivation target)
    * composing it from collection of value trees, using provided construction method.
    * It makes sure that depending on derivation target of input trees / output type, all the necessary
    * values are properly wrapped, unwrapped or composed, making sure the produced code will type-check and satisfy
    * any required semantic invariants.
    *
    * @param To target transformation result type
    * @param targets description of target parameters required to construct the `To` type
    * @param bodyTreeArgs derived input argument trees
    * @param derivationTarget desired type of resulting transformer
    * @param mkTargetValueTree constructor method that composes final tree out of unwrapped value trees
    * @return
    */
  def mkTransformerBodyTree(
      To: Type,
      targets: Seq[Target],
      bodyTreeArgs: Seq[DerivedTree],
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Seq[Tree] => Tree
  ): DerivedTree = {
    assert(targets.size == bodyTreeArgs.size, "targets arity must correspond to the argument trees arity")

    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        assertOrAbort(
          bodyTreeArgs.forall(_.isTotalTarget),
          "All derived body trees arguments must be total in Total target derivation!"
        )
        DerivedTree.fromTotalTree(mkTargetValueTree(bodyTreeArgs.map(_.tree)))

      case pt: DerivationTarget.PartialTransformer =>
        assertOrAbort(
          bodyTreeArgs.forall(a => a.isTotalTarget || a.isPartialTarget),
          "Only Total and Partial body tree arguments are supported in Partial target derivation!"
        )

        val (totalArgs, partialArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (partialArgs.isEmpty) {
          DerivedTree.fromTotalTree(mkTargetValueTree(bodyTreeArgs.map(_.tree)))
        } else if (partialArgs.sizeIs == 1) {
          val (target, bodyTree) = partialArgs.head
          val fn = freshTermName(target.name)
          val totalArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap
          val argsMap = totalArgsMap + (target -> q"$fn")
          val updatedArgs = targets.map(argsMap)

          DerivedTree(q"${bodyTree.tree}.map { ($fn: ${target.tpe}) => ${mkTargetValueTree(updatedArgs)} }", pt)
        } else if (partialArgs.sizeIs == 2) {
          val (target0, bodyTree0) = partialArgs.head
          val (target1, bodyTree1) = partialArgs.last
          val fn0 = freshTermName(target0.name)
          val fn1 = freshTermName(target1.name)

          val totalArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap
          val argsMap = totalArgsMap + (target0 -> q"$fn0") + (target1 -> q"$fn1")
          val updatedArgs = targets.map(argsMap)

          val tree = Trees.PartialResult
            .map2(
              target0.tpe,
              target1.tpe,
              To,
              bodyTree0.tree,
              bodyTree1.tree,
              q"{ case ($fn0: ${target0.tpe}, $fn1: ${target1.tpe}) => ${mkTargetValueTree(updatedArgs)} }",
              pt.failFastTree
            )
          DerivedTree(tree, pt)
        } else {
          val totalArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap

          val partialTargets = partialArgs.map(_._1)

          val localDefNames = partialTargets.map(t => freshTermName(s"rd_${t.name}"))
          val localTreeDefs = (localDefNames zip partialArgs).map { case (dn, (target, tbt)) =>
            q"final def $dn: ${Trees.PartialResult.tpe(target.tpe)} = { ${tbt.tree} }"
          }
          val localValNames = partialTargets.map(t => freshTermName(s"rv_${t.name}"))

          // short circuit branch (fail fast)
          val succFFValIdents = partialTargets.map(t => freshTermName(s"rvff_${t.name}"))
          val succFFFqs = (succFFValIdents zip localDefNames).map { case (rvff, rd) => fq"$rvff <- $rd" }
          val succValFFTrees = succFFValIdents.map(rvff => q"$rvff")
          val patRefArgsMapFF = (partialTargets zip succValFFTrees).toMap
          val argsMapFF = totalArgsMap ++ patRefArgsMapFF
          val updatedArgsFF = targets.map(argsMapFF)

          // long circuit branch (errors accumulation)
          val succValTrees = (localValNames zip partialTargets).map { case (rv, target) =>
            q"$rv.asInstanceOf[${Trees.PartialResult.valueTpe(target.tpe)}].value"
          }
          val patRefArgsMap = (partialTargets zip succValTrees).toMap
          val argsMap = totalArgsMap ++ patRefArgsMap
          val updatedArgs = targets.map(argsMap)
          val allErrorsIdent = freshTermName("allErrors")
          val errorsCaptureTrees = (localValNames zip localDefNames).flatMap { case (rv, rd) =>
            Seq(
              q"final val $rv = $rd",
              q"""$allErrorsIdent = ${Trees.PartialErrors.mergeResultNullable(q"$allErrorsIdent", q"$rv")}"""
            )
          }

          val tree = q"""{
                ..$localTreeDefs
                if(${pt.failFastTree}) {
                  for (..$succFFFqs) yield ${mkTargetValueTree(updatedArgsFF)}
                } else {
                  var $allErrorsIdent: ${Trees.PartialErrors.tpe} = null
                  ..$errorsCaptureTrees
                  if ($allErrorsIdent == null) {
                    ${Trees.PartialResult.value(mkTargetValueTree(updatedArgs))}
                  } else {
                    $allErrorsIdent
                  }
                }
              }"""

          DerivedTree(tree, pt)
        }

      case lt @ DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        assertOrAbort(
          bodyTreeArgs.forall(a => a.isTotalTarget || a.isLiftedTarget),
          "Only Total and Lifted body tree arguments are supported in Lifted target derivation!"
        )

        val (totalArgs, liftedArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (liftedArgs.isEmpty) {
          DerivedTree.fromTotalTree(mkTargetValueTree(bodyTreeArgs.map(_.tree)))
        } else {

          val (liftedTargets, liftedBodyTrees) = liftedArgs.unzip
          val liftedTrees = liftedBodyTrees.map(_.tree)
          val productF = liftedTrees.reduceRight { (tree, rest) =>
            q"$wrapperSupportInstance.product($tree, $rest)"
          }

          val argNames = liftedTargets.map(target => freshTermName(target.name))
          val argTypes = liftedTargets.map(_.tpe)
          val bindTreesF = argNames.map { termName =>
            Bind(termName, Ident(termNames.WILDCARD))
          }
          val productType = argTypes.map(tpe => tq"$tpe").reduceRight[Tree]((param, tree) => tq"($param, $tree)")
          val patternF = bindTreesF.reduceRight[Tree]((param, tree) => pq"(..${List(param, tree)})")

          val patRefArgsMap = (liftedTargets zip argNames).map { case (target, argName) => target -> q"$argName" }.toMap
          val pureArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap
          val argsMap = pureArgsMap ++ patRefArgsMap

          val updatedArgs = targets.map(argsMap)

          val tree = q"""
            $wrapperSupportInstance.map[$productType, $To](
              $productF,
              { case $patternF => ${mkTargetValueTree(updatedArgs)} }
            )
          """

          DerivedTree(tree, lt)
        }
    }
  }

  /** Composition code for coproduct pattern match, that creates resulting tree.
    */
  def mkCoproductPatternMatch(
      srcPrefixTree: Tree,
      clauses: Seq[InstanceClause],
      derivationTarget: DerivationTarget
  ): DerivedTree = {
    if (clauses.forall(_.body.isTotalTarget)) {
      val clausesTrees = clauses.map(_.toPatMatClauseTree)
      DerivedTree.fromTotalTree(
        q"${srcPrefixTree} match { case ..$clausesTrees }"
      )
    } else {
      val liftedClauses = clauses.map(_.mapBody(mkDerivedBodyTree(derivationTarget)))
      val liftedClausesTrees = liftedClauses.map(_.toPatMatClauseTree)
      DerivedTree(q"${srcPrefixTree} match { case ..$liftedClausesTrees }", derivationTarget)
    }
  }

  def mkEitherFold(
      srcPrefixTree: Tree,
      targetTpe: Type,
      clauseLeft: InstanceClause,
      clauseRight: InstanceClause,
      derivationTarget: DerivationTarget
  ): DerivedTree = {
    if (clauseLeft.body.isTotalTarget && clauseRight.body.isTotalTarget) {
      DerivedTree.fromTotalTree(
        q"""
              ${srcPrefixTree}.fold[$targetTpe](
                (${clauseLeft.matchName.get}: ${clauseLeft.matchTpe}) => ${clauseLeft.body.tree},
                (${clauseRight.matchName.get}: ${clauseRight.matchTpe}) => ${clauseRight.body.tree},
              )
              """
      )
    } else {
      val tree = q"""
        ${srcPrefixTree}.fold[${derivationTarget.targetType(targetTpe)}](
          (${clauseLeft.matchName.get}: ${clauseLeft.matchTpe}) => ${mkDerivedBodyTree(derivationTarget)(
          clauseLeft.body
        ).tree},
          (${clauseRight.matchName.get}: ${clauseRight.matchTpe}) => ${mkDerivedBodyTree(derivationTarget)(
          clauseRight.body
        ).tree},
        )
      """

      DerivedTree(tree, derivationTarget)
    }
  }
}
