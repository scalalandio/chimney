package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.AssertUtils

import scala.reflect.macros.blackbox

import scala.collection.compat._

trait TargetConstructorMacros extends Model with AssertUtils with GenTrees {

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
      bodyTreeArgs: Seq[TransformerBodyTree],
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Seq[Tree] => Tree
  ): Tree = {
    assert(targets.size == bodyTreeArgs.size, "targets arity must correspond to the argument trees arity")

    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        assertOrAbort(
          bodyTreeArgs.forall(_.isTotalTarget),
          "All derived body trees arguments must be total in Total target derivation!"
        )
        mkTargetValueTree(bodyTreeArgs.map(_.tree))

      case pt: DerivationTarget.PartialTransformer =>
        assertOrAbort(
          bodyTreeArgs.forall(a => a.isTotalTarget || a.isPartialTarget),
          "Only Total and Partial body tree arguments are supported in Partial target derivation!"
        )

        val (totalArgs, partialArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (partialArgs.isEmpty) {
          mkTransformerBodyTree0(pt)(mkTargetValueTree(bodyTreeArgs.map(_.tree)))
        } else if (partialArgs.sizeCompare(1) == 0) {
          val (target, bodyTree) = partialArgs.head
          val fn = freshTermName(target.name)
          val totalArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap
          val argsMap = totalArgsMap + (target -> q"$fn")
          val updatedArgs = targets.map(argsMap)

          q"${bodyTree.tree}.map { ($fn: ${target.tpe}) => ${mkTargetValueTree(updatedArgs)} }"
        } else if (partialArgs.sizeCompare(2) == 0) {
          val (target0, bodyTree0) = partialArgs.head
          val (target1, bodyTree1) = partialArgs.last
          val fn0 = freshTermName(target0.name)
          val fn1 = freshTermName(target1.name)

          val totalArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap
          val argsMap = totalArgsMap + (target0 -> q"$fn0") + (target1 -> q"$fn1")
          val updatedArgs = targets.map(argsMap)

          Trees.PartialResult
            .map2(
              target0.tpe,
              target1.tpe,
              To,
              bodyTree0.tree,
              bodyTree1.tree,
              q"{ case ($fn0: ${target0.tpe}, $fn1: ${target1.tpe}) => ${mkTargetValueTree(updatedArgs)} }",
              pt.failFastTree
            )
        } else {
          val (partialTargets, partialBodyTrees) = partialArgs.unzip
          val partialTrees = partialBodyTrees.map(_.tree)
          val totalArgsMap = totalArgs.map { case (target, bt) => target -> bt.tree }.toMap

          if (partialArgs.sizeCompare(22) <= 0) { // tuple-based encoding, type info preserved

            val localDefNames = partialTrees.map(_ => freshTermName("t"))
            val localTreeDefs = (localDefNames zip partialTrees).map { case (n, t) => q"final def $n = { $t }" }

            val succFFValIdents = partialArgs.map(_ => freshTermName("vff"))
            val succFFFqs = (succFFValIdents zip localDefNames).map { case (vId, lt) => fq"$vId <- $lt" }
            val succValFFTrees = succFFValIdents.map(vId => q"$vId")
            val patRefArgsMapFF = (partialTargets zip succValFFTrees).toMap
            val argsMapFF = totalArgsMap ++ patRefArgsMapFF
            val updatedArgsFF = targets.map(argsMapFF)

            val succValIdents = partialTrees.map(_ => freshTermName("v"))
            val localTreeRefs = localDefNames.map { lt =>
              q"$lt"
            }
            val succValPats = succValIdents.map(vId => Trees.PartialResult.patValue(vId))
            val allSuccPat = pq"(..${succValPats})"
            val succValTrees = succValIdents.map(vId => q"$vId")
            val patRefArgsMap = (partialTargets zip succValTrees).toMap
            val argsMap = totalArgsMap ++ patRefArgsMap
            val updatedArgs = targets.map(argsMap)

            val errIdents = partialTrees.map(_ => freshTermName("err"))
            val errPats = errIdents.map(errId => pq"$errId")
            val errPat = pq"(..${errPats})"

            val allErrorsIdent = freshTermName("allErrors")
            val allErrIfTrees = errIdents.map { errId =>
              val localErrsIdent = freshTermName("localErrs")
              q"""
                if ($errId.isInstanceOf[${Trees.PartialErrors.tpe}]) {
                  val $localErrsIdent = $errId.asInstanceOf[${Trees.PartialErrors.tpe}]
                  if ($allErrorsIdent == null) {
                    $allErrorsIdent = $localErrsIdent
                  } else {
                    $allErrorsIdent = ${Trees.PartialErrors.merge(allErrorsIdent, localErrsIdent)}
                  }
                }
               """
            }

            q"""
              {
                ..$localTreeDefs
                if(${pt.failFastTree}) {
                  for (..$succFFFqs) yield ${mkTargetValueTree(updatedArgsFF)}
                } else {
                  (..${localTreeRefs}) match {
                    case $allSuccPat =>
                      ${Trees.PartialResult.value(mkTargetValueTree(updatedArgs))}
                    case $errPat => // (r1, r2, .., rk)
                      var $allErrorsIdent: ${Trees.PartialErrors.tpe} = null
                      ..$allErrIfTrees
                      $allErrorsIdent
                  }
                }
              }
            """
          } else { // Array[Any] + sequence, type info lost
            // should be possible to optimize with nested tuples
            val partialTreesArray = Trees.array(partialTrees)
            val arrayFn = freshTermName("array")
            val argIndices = partialTargets.indices
            val patRefArgsMap = (partialTargets zip argIndices).map {
              case (target, argIndex) => target -> q"$arrayFn.apply($argIndex).asInstanceOf[${target.tpe}]"
            }.toMap
            val argsMap = totalArgsMap ++ patRefArgsMap
            val updatedArgs = targets.map(argsMap)

            q"""
               ${Trees.PartialResult.sequence(
              Trees.arrayAny,
              Trees.any,
              q"${partialTreesArray}.iterator",
              pt.failFastTree
            )}
               .map { ($arrayFn: ${Trees.arrayAny}) => ${mkTargetValueTree(updatedArgs)} }
             """
          }
        }

      case lt @ DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
        assertOrAbort(
          bodyTreeArgs.forall(a => a.isTotalTarget || a.isLiftedTarget),
          "Only Total and Lifted body tree arguments are supported in Lifted target derivation!"
        )

        val (totalArgs, liftedArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

        if (liftedArgs.isEmpty) {
          mkTransformerBodyTree0(lt)(mkTargetValueTree(bodyTreeArgs.map(_.tree)))
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
          val pureArgsMap = totalArgs.map { case (target, bt)                           => target -> bt.tree }.toMap
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
