package io.scalaland.chimney.internal.utils

import io.scalaland.chimney.internal.macros.TransformerConfigSupport

import scala.reflect.macros.blackbox

trait DslMacroUtils extends MacroUtils with TransformerConfigSupport {

  val c: blackbox.Context
  import CfgTpes.*

  import c.universe.*

  implicit class TransformerDefinitionTreeOps(td: Tree) {

    def overrideField[C: WeakTypeTag](fieldName: Name, overrideTree: Tree, configWrapperTC: Type): Tree = {
      c.prefix.tree
        .addOverride(overrideTree)
        .refineConfig(configWrapperTC.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }

    def overrideCoproductInstance[C: WeakTypeTag](
        instTpe: Type,
        targetTpe: Type,
        f: Tree,
        configWrapperTC: Type
    ): Tree = {
      c.prefix.tree
        .addInstance(f)
        .refineConfig(configWrapperTC.applyTypeArgs(instTpe, targetTpe, weakTypeOf[C]))
    }

    def renameField[C: WeakTypeTag](fromName: TermName, toName: TermName): Tree = {
      c.prefix.tree
        .refineConfig(
          fieldRelabelledT.applyTypeArgs(fromName.toSingletonTpe, toName.toSingletonTpe, weakTypeOf[C])
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
      val fnTermName = freshTermName(name)
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

    def addOverride(overrideTree: Tree): Tree = {
      q"$td.__addOverride(${overrideTree}.asInstanceOf[Any])"
    }

    def addInstance(f: Tree): Tree = {
      q"$td.__addInstance(${f}.asInstanceOf[Any])"
    }

    def refineConfig(cfgTpe: Type): Tree = {
      q"$td.__refineConfig[$cfgTpe]"
    }
  }
}
