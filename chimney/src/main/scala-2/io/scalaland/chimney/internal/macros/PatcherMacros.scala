package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.compiletime.DerivationError
import io.scalaland.chimney.internal.compiletime.derivation.transformer
import io.scalaland.chimney.internal.PatcherConfiguration

import scala.reflect.macros.blackbox

trait PatcherMacros
    extends PatcherConfiguration
    with GenTrees
    with transformer.Gateway
    with transformer.DerivationPlatform {

  val c: blackbox.Context

  import c.universe.{internal as _, Transformer as _, Type as _, *}

  def expandPatch[T: WeakTypeTag, Patch: WeakTypeTag, C: WeakTypeTag]: Tree = {
    val C = weakTypeOf[C]
    val piName = freshTermName("pi")
    val config = capturePatcherConfig(C)

    val derivedPatcherTree = genPatcher[T, Patch](config).tree

    q"""
       val $piName = ${c.prefix.tree}
       $derivedPatcherTree.patch($piName.obj, $piName.objPatch)
    """
  }

  def genPatcher[T: WeakTypeTag, Patch: WeakTypeTag](
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

      val targetMapping = patchParams.flatMap { pParam =>
        resolveFieldMapping(config, T, Patch, tParamsByName, fnObj, fnPatch, pParam)
      }

      if (targetMapping.exists(_.isLeft)) {
        val errors = targetMapping.collect { case Left(err) => err }.mkString("\n")
        c.abort(c.enclosingPosition, errors)
      } else {
        val patchMapping = targetMapping.collect { case Right(tree) => tree }.toMap

        val args = tParams.map { tParam =>
          patchMapping.getOrElse(tParam.name, q"$fnObj.${tParam.name}")
        }

        val resultTree = q"""
           new ${Trees.Patcher.tpe(T, Patch)} {
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
      T: c.Type,
      Patch: c.Type,
      tParamsByName: Map[TermName, MethodSymbol],
      fnObj: TermName,
      fnPatch: TermName,
      pParam: MethodSymbol
  ): Option[Either[String, (TermName, Tree)]] = {

    def patchField = q"$fnPatch.${pParam.name}"
    def entityField = q"$fnObj.${pParam.name}"

    val patchParamTpe = pParam.resultTypeIn(Patch)

    tParamsByName.get(pParam.name) match {
      case Some(tParam) if config.ignoreNoneInPatch && bothOptions(patchParamTpe, tParam.resultTypeIn(T)) =>
        Some {
          val tParamTpe = tParam.resultTypeIn(T)
          if (patchParamTpe <:< tParamTpe) { // not really crucial branch, should be handled by transformer
            Right(pParam.name -> q"$patchField.orElse($entityField)")
          } else {
            expandTransformerTree(
              patchField,
              patchParamTpe,
              tParamTpe
            ).map { transformerTree =>
              pParam.name -> q"${transformerTree}.orElse($entityField)"
            }.left
              .map(DerivationError.printErrors)
          }
        }
      case Some(tParam) if patchParamTpe <:< tParam.resultTypeIn(T) =>
        Some(Right(pParam.name -> patchField))
      case Some(tParam) =>
        Some(
          expandTransformerTree(
            patchField,
            patchParamTpe,
            tParam.resultTypeIn(T)
          ).map { transformerTree =>
            pParam.name -> transformerTree
          }.left
            .flatMap { errors =>
              if (isOption(patchParamTpe)) {
                expandTransformerTree(
                  q"$patchField.get",
                  patchParamTpe.typeArgs.head,
                  tParam.resultTypeIn(T)
                ).map { innerTransformerTree =>
                  pParam.name -> q"if($patchField.isDefined) { ${innerTransformerTree} } else { $entityField }"
                }.left
                  .map(errors2 => DerivationError.printErrors(errors ++ errors2))
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

  private def expandTransformerTree(
      srcPrefixTree: c.Tree,
      patchParamTpe: c.Type,
      tParamTpe: c.Type
  ): Either[Seq[DerivationError], c.Tree] = {

    val fromType = Type.platformSpecific.fromUntyped(patchParamTpe).asExistential
    val toType = Type.platformSpecific.fromUntyped(tParamTpe).asExistential

    ExistentialType.use2(fromType, toType) { implicit from => implicit to =>
      val context = TransformationContext.ForTotal
        .create[fromType.Underlying, toType.Underlying](
          c.Expr(srcPrefixTree),
          TransformerConfig(),
          ChimneyExpr.RuntimeDataStore.empty
        )
        .updateConfig(_.allowFromToImplicitSearch)

      deriveFinalTransformationResultExpr(context).toEither
        .map(_.tree)
        .left
        .map(_.asVector.toSeq)
    }
  }
}
