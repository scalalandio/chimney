package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.internal.{DerivationConfig, DerivationError, PatcherCfg}

import scala.reflect.macros.blackbox

trait PatcherMacros {
  this: TransformerMacros with DerivationGuards with MacroUtils with DerivationConfig with EitherUtils =>

  val c: blackbox.Context

  import c.universe._

  def expandPatch[T: c.WeakTypeTag, Patch: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val piName = TermName(c.freshName("pi"))
    val config = capturePatcherConfig(C)

    val derivedPatcherTree = genPatcher[T, Patch](config).tree

    q"""
       val $piName = ${c.prefix.tree}
       $derivedPatcherTree.patch($piName.obj, $piName.objPatch)
    """
  }

  def genPatcher[T: c.WeakTypeTag, Patch: c.WeakTypeTag](
      config: PatcherConfig
  ): c.Expr[io.scalaland.chimney.Patcher[T, Patch]] = {

    val T = weakTypeOf[T]
    val Patch = weakTypeOf[Patch]

    if (!T.isCaseClass || !Patch.isCaseClass) {
      c.abort(c.enclosingPosition, s"Patcher derivation is only supported for case classes!")
    } else {

      val tParams = T.caseClassParams
      val tParamsByName = tParams.map(p => p.name -> p).toMap

      val patchParams = Patch.caseClassParams

      val fnObj = c.internal.reificationSupport.freshTermName("obj$")
      val fnPatch = c.internal.reificationSupport.freshTermName("patch$")

      val targetMapping = patchParams.toSeq.flatMap { pParam =>
        tParamsByName.get(pParam.name) match {
          case Some(tParam) if pParam.returnType <:< tParam.returnType =>
            Some(
              Right(q"$fnPatch.${pParam.name}")
            )
          case Some(tParam) =>
            Some(
              transformOptionalValue(fnPatch, pParam, tParam, fnObj)
            )
          case None =>
            if (config.enableIncompletePatches) {
              None
            } else {
              Some(
                Left(s"Field named '${pParam.name}' not found in target patching type $T!")
              )
            }
        }
      }

      if (targetMapping.exists(_.isLeft)) {
        val errors = targetMapping.collect { case Left(err) => err }.mkString("\n")
        c.abort(c.enclosingPosition, errors)
      } else {
        val paramTrees = targetMapping.map(_.getRight)
        val patchMapping = (patchParams zip paramTrees).map { case (param, tree) => param.name -> tree }.toMap

        val args = tParams.map { tParam =>
          patchMapping.getOrElse(tParam.name, q"$fnObj.${tParam.name}")
        }

        val resultTree = q"""
           new _root_.io.scalaland.chimney.Patcher[$T, $Patch] {
             def patch($fnObj: $T, $fnPatch: $Patch): $T = {
               new $T(..$args)
             }
           }
        """

        c.Expr[io.scalaland.chimney.Patcher[T, Patch]](resultTree)
      }
    }
  }

  def transformOptionalValue(
      fnPatch: c.universe.TermName,
      pParam: c.universe.MethodSymbol,
      tParam: c.universe.MethodSymbol,
      fnObj: c.universe.TermName
  ): Either[String, Tree] = {
    expandTransformerTree(q"$fnPatch.${pParam.name}", TransformerConfig())(pParam.returnType, tParam.returnType).left
      .flatMap { errors =>
        if (pParam.returnType.typeConstructor =:= optionTpe.typeConstructor) {
          expandTransformerTree(q"$fnPatch.${pParam.name}.get", TransformerConfig())(
            pParam.returnType.typeArgs.head,
            tParam.returnType
          ).mapRight { innerTransformerTree =>
              q"if($fnPatch.${pParam.name}.isDefined) { $innerTransformerTree } else { $fnObj.${pParam.name} }"
            }
            .mapLeft(errors ++ _)
            .mapLeft(DerivationError.printErrors)
        } else {
          Left(DerivationError.printErrors(errors))
        }
      }
  }

  def capturePatcherConfig(cfgTpe: Type, config: PatcherConfig = PatcherConfig()): PatcherConfig = {

    import PatcherCfg._

    val emptyT = typeOf[Empty]
    val enableIncompletePatches = typeOf[EnableIncompletePatches[_]].typeConstructor

    if (cfgTpe =:= emptyT) {
      config
    } else if (cfgTpe.typeConstructor =:= enableIncompletePatches) {
      capturePatcherConfig(cfgTpe.typeArgs.head, config.copy(enableIncompletePatches = true))
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal patcher config type shape!")
      // $COVERAGE-ON$
    }
  }
}
