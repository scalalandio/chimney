package io.scalaland.chimney.internal.macros

import scala.reflect.macros.blackbox

trait Model extends TransformerConfigSupport {

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

  sealed trait AccessorResolution extends Product with Serializable {
    def isResolved: Boolean
  }
  object AccessorResolution {
    case object NotFound extends AccessorResolution {
      override def isResolved: Boolean = false
    }
    case class Resolved(symbol: MethodSymbol, wasRenamed: Boolean) extends AccessorResolution {
      override def isResolved: Boolean = true
    }
    case object DefAvailable extends AccessorResolution {
      override def isResolved: Boolean = false
    }
  }
}
