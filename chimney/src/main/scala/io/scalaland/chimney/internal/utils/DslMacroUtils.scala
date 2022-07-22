package io.scalaland.chimney.internal.utils

import io.scalaland.chimney.internal.macros.TransformerConfigSupport

import scala.reflect.macros.blackbox

trait DslMacroUtils extends MacroUtils with TransformerConfigSupport {

  val c: blackbox.Context
  import CfgTpes._

  import c.universe._

  implicit class TransformerDefinitionTreeOps(td: Tree) {

    def accessOverriddenConstValue(name: String, targetTpe: Type): Tree = {
      q"""
        $td
          .overrides($name)
          .asInstanceOf[$targetTpe]
      """
    }

    def accessOverriddenComputedFunction(name: String, fromTpe: Type, targetTpe: Type): Tree = {
      q"""
        $td
          .overrides($name)
          .asInstanceOf[$fromTpe => $targetTpe]
      """
    }

    def overrideField(fieldName: Name, overrideTree: Tree, configWrapperTC: Type, underlyingConfigTpe: Type): Tree = {
      c.prefix.tree
        .addOverride(fieldName, overrideTree)
        .refineConfig(configWrapperTC.applyTypeArgs(fieldName.toSingletonTpe, underlyingConfigTpe))
    }

    def overrideCoproductInstance(
        instTpe: Type,
        targetTpe: Type,
        f: Tree,
        configWrapperTC: Type,
        underlyingConfigTpe: Type
    ): Tree = {
      c.prefix.tree
        .addInstance(instTpe.typeSymbol.fullName, targetTpe.typeSymbol.fullName, f)
        .refineConfig(configWrapperTC.applyTypeArgs(instTpe, targetTpe, underlyingConfigTpe))
    }

    def renameField(fromName: TermName, toName: TermName, underlyingConfigTpe: Type): Tree = {
      c.prefix.tree
        .refineConfig(
          fieldRelabelledT.applyTypeArgs(fromName.toSingletonTpe, toName.toSingletonTpe, underlyingConfigTpe)
        )
    }

    def refineTransformerDefinition_Hack(
        definitionRefinementFn: Map[String, Tree] => Tree,
        valTree: (String, Tree)
    ): Tree = {
      // normally, we would like to use refineTransformerDefinition, which works well on Scala 2.11
      // in few cases on Scala 2.12+ it ends up as 'Error while emitting XXX.scala' compiler error
      // with this hack, we can work around scalac bugs

      val (name, tree) = valTree
      val fnTermName = TermName(c.freshName(name))
      val fnMapTree = Map(name -> Ident(fnTermName))
      q"""
        {
          val ${fnTermName} = $tree
          $td.__refineTransformerDefinition(${definitionRefinementFn(fnMapTree)})
        }
      """
    }

    def refineTransformerDefinition(definitionRefinementFn: Tree): Tree = {
      q"$td.__refineTransformerDefinition($definitionRefinementFn)"
    }

    def addOverride(fieldName: Name, overrideTree: Tree): Tree = {
      q"$td.__addOverride(${fieldName.toNameLiteral}, $overrideTree)"
    }

    def addInstance(fullInstName: String, fullTargetName: String, f: Tree): Tree = {
      q"$td.__addInstance($fullInstName, $fullTargetName, $f)"
    }

    def refineConfig(cfgTpe: Type): Tree = {
      q"$td.__refineConfig[$cfgTpe]"
    }
  }
}
