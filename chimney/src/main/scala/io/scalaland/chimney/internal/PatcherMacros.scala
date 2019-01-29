package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait PatcherMacros {
  this: TransformerMacros with DerivationGuards with MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def genPatcher[T: c.WeakTypeTag, Patch: c.WeakTypeTag](): c.Expr[io.scalaland.chimney.Patcher[T, Patch]] = {

    val T = weakTypeOf[T]
    val Patch = weakTypeOf[Patch]

    def transformOptionalValue(fnPatch: c.universe.TermName,
                               pParam: c.universe.MethodSymbol,
                               tParam: c.universe.MethodSymbol,
                               fnObj: c.universe.TermName) = {
      expandTransformerTree(q"$fnPatch.${pParam.name}", Config())(pParam.returnType, tParam.returnType).left
        .flatMap { errors =>
          if (pParam.returnType.typeConstructor =:= optionTpe.typeConstructor) {
            expandTransformerTree(q"$fnPatch.${pParam.name}.get", Config())(
              pParam.returnType.typeArgs.head,
              tParam.returnType
            ).right
              .map { innerTransformerTree =>
                q"if($fnPatch.${pParam.name}.isDefined) { $innerTransformerTree } else { $fnObj.${pParam.name} }"
              }
              .left
              .map(errors ++ _)
          } else {
            Left(DerivationError.printErrors(errors))
          }
        }
    }

    if (!T.isCaseClass || !Patch.isCaseClass) {
      c.abort(c.enclosingPosition, s"Patcher derivation is only supported for case classes!")
    } else {

      val tParams = T.caseClassParams
      val tParamsByName = tParams.map(p => p.name -> p).toMap

      val patchParams = Patch.caseClassParams

      val fnObj = c.internal.reificationSupport.freshTermName("obj$")
      val fnPatch = c.internal.reificationSupport.freshTermName("patch$")

      val matchingParams = patchParams.toSeq flatMap { pParam =>
        tParamsByName.get(pParam.name).map(_ -> pParam)
      }

      val targetMapping = matchingParams.map {
        case (tParam, pParam) if pParam.returnType <:< tParam.returnType =>
          Right(q"$fnPatch.${pParam.name}")
        case (tParam, pParam) =>
          transformOptionalValue(fnPatch, pParam, tParam, fnObj)
      }

      if (targetMapping.exists(_.isLeft)) {
        val errors = targetMapping.collect { case Left(err) => err }.mkString("\n")
        c.abort(c.enclosingPosition, errors)
      } else {
        val paramTrees = targetMapping.map(_.right.get)
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
}
