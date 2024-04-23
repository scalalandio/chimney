package io.scalaland.chimney.integrations

import scala.collection.mutable
import scala.reflect.ClassTag

private[integrations] trait FactoryCompatScala3Only { this: FactoryCompat.type =>

  def iarrayFactory[I: ClassTag]: scala.collection.compat.Factory[I, IArray[I]] = new FactoryCompat[I, IArray[I]] {

    override def newBuilder: mutable.Builder[I, IArray[I]] = IArray.newBuilder[I]
  }
}
