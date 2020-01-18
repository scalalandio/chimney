package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}
import io.scalaland.chimney.internal.{DerivationError, PatcherConfiguration}

import scala.reflect.macros.blackbox

trait PatcherMacros extends PatcherConfiguration {
  this: TransformerMacros with DerivationGuards with MacroUtils with EitherUtils =>

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
        resolveFieldMapping(config, T, tParamsByName, fnObj, fnPatch, pParam)
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

  def resolveFieldMapping(
      config: PatcherConfig,
      T: Type,
      tParamsByName: Map[TermName, MethodSymbol],
      fnObj: TermName,
      fnPatch: TermName,
      pParam: MethodSymbol
  ): Option[Either[String, Tree]] = {

    def patchField = q"$fnPatch.${pParam.name}"
    def entityField = q"$fnObj.${pParam.name}"

    tParamsByName.get(pParam.name) match {
      case Some(tParam)
          if config.ignoreNoneInPatch &&
            bothOptions(pParam.returnType, tParam.returnType) =>
        Some {
          if (pParam.returnType <:< tParam.returnType) {
            Right(q"$patchField.orElse($entityField)")
          } else {
            expandTransformerTree(patchField, TransformerConfig())(
              pParam.returnType,
              tParam.returnType
            ).mapRight { transformerTree =>
                q"$transformerTree.orElse($entityField)"
              }
              .mapLeft(DerivationError.printErrors)
          }
        }
      case Some(tParam) if pParam.returnType <:< tParam.returnType =>
        Some(Right(patchField))
      case Some(tParam) =>
        Some(
          expandTransformerTree(patchField, TransformerConfig())(
            pParam.returnType,
            tParam.returnType
          ).left
            .flatMap { errors =>
              if (isOption(pParam.returnType)) {
                expandTransformerTree(q"$patchField.get", TransformerConfig())(
                  pParam.returnType.typeArgs.head,
                  tParam.returnType
                ).mapRight { innerTransformerTree =>
                    q"if($patchField.isDefined) { $innerTransformerTree } else { $entityField }"
                  }
                  .mapLeft(errors ++ _)
                  .mapLeft(DerivationError.printErrors)
              } else {
                Left(DerivationError.printErrors(errors))
              }
            }
        )
      case None =>
        if (config.ignoreRedundantPatcherFields) {
          None
        } else {
          Some(
            Left(s"Field named '${pParam.name}' not found in target patching type $T!")
          )
        }
    }
  }
}
