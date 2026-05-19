package io.scalaland.chimney.integrations

import java.util as ju
import scala.collection.mutable

abstract class FactoryCompatJavaCollectionBuilder[-A, +CC, Item, C <: ju.Collection[Item]](protected val impl: C)
    extends FactoryCompat.Builder[A, CC] {
  override def clear(): Unit = impl.clear()
}

abstract class FactoryCompatVarBuilder[-A, +CC, C](builder: () => C) extends FactoryCompat.Builder[A, CC] {
  protected var impl: C = builder()
  override def clear(): Unit = impl = builder()
}

abstract class FactoryCompatBuilder[-A, +CC, C <: mutable.Builder[?, ?]](protected val impl: C)
    extends FactoryCompat.Builder[A, CC] {
  override def clear(): Unit = impl.clear()
}
