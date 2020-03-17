package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.TransformerConfiguration

import scala.reflect.macros.blackbox

trait Model extends TransformerConfiguration {

  val c: blackbox.Context

  import c.universe._

  case class Target(name: String, tpe: Type)
  object Target {
    def fromJavaBeanSetter(ms: MethodSymbol, site: Type): Target =
      Target(ms.canonicalName, ms.beanSetterParamTypeIn(site))

    def fromField(ms: MethodSymbol, site: Type): Target =
      Target(ms.canonicalName, ms.resultTypeIn(site))
  }

  case class TransformerBodyTree(tree: Tree, isWrapped: Boolean)
  case class ResolvedAccessor(symbol: MethodSymbol, wasRenamed: Boolean)

}
