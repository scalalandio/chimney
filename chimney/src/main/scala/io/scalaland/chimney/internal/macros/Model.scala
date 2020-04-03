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

  sealed trait Accessor extends Product with Serializable
  object Accessor {
    implicit val accessorOrdering: Ordering[Accessor] = Ordering.by {
      case NotFound     => 0
      case DefAvailable => 1
      case _: Resolved  => 2
    }
    case object NotFound extends Accessor
    case class Resolved(symbol: MethodSymbol, wasRenamed: Boolean) extends Accessor
    case object DefAvailable extends Accessor
  }
}
