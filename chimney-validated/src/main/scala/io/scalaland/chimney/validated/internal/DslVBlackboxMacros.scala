package io.scalaland.chimney.validated.internal

import io.scalaland.chimney.internal._

import scala.reflect.macros.blackbox

trait DslVBlackboxMacros {
  this: DslBlackboxMacros with VTransformerMacros with MacroUtils with DerivationVConfig with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def expandVTransform[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag, VC: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val VC = weakTypeOf[VC]
    val srcName = c.freshName("src")
    val underlying = captureConfiguration(C).copy(prefixValName = srcName)
    val config = captureVСonfig(VC, VConfig(underlying))

    val derivedTransformerTree = genVTransformer[From, To](config, findImplicit = false).tree

    q"""
       val ${TermName(srcName)} = ${c.prefix.tree}
       $derivedTransformerTree.transform(${TermName(srcName)}.source)
    """
  }

  def captureVСonfig(cfgTpe: Type, config: VConfig): VConfig = {

    val fieldConstVT = typeOf[FieldConstV[_, _]].typeConstructor
    val fieldComputedVT = typeOf[FieldComputedV[_, _]].typeConstructor

    if (cfgTpe == typeOf[VEmpty]) {
      config
    } else if (Set(fieldConstVT, fieldComputedVT).contains(cfgTpe.typeConstructor)) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      captureVСonfig(rest, config.copy(overridenVFields = config.overridenVFields + fieldName))
    } else {
      // $COVERAGE-OFF$
      c.abort(c.enclosingPosition, "Bad internal vconfig type shape!")
      // $COVERAGE-ON$
    }
  }
}
