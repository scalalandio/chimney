package io.scalaland.chimney.internal.runtime

import io.scalaland.chimney.integrations.{PartiallyBuildMap, TotallyBuildMap}

import scala.annotation.{implicitNotFound, unused}

// $COVERAGE-OFF$evidence used only within macro-erased expressions

/** Allow us to provide `.everyMapKey` and `.everyMapValue` extension methods only for the types where macros would
  * actually handle it.
  *
  * @since 1.0.0
  */
@implicitNotFound(
  "Expected map (type extending scala.collection.immutable.Map which has scala.collection.Factory instance), got ${M}"
)
sealed trait IsMap[M] {
  type Key
  type Value
}
object IsMap extends IsMapImplicits0 {
  @implicitNotFound(
    "Expected map (type extending scala.collection.immutable.Map which has scala.collection.Factory instance), got ${M}"
  )
  type Of[M, K, V] = IsMap[M] { type Key = K; type Value = V }

  protected object Impl extends IsMap[Nothing]
}
private[runtime] trait IsMapImplicits0 extends IsMapImplicits1 { this: IsMap.type =>

  // build-in Chimney support for maps assumes that they are BOTH Map and have a Factory
  implicit def scalaMapIsMap[K, V, M <: Map[K, V]](implicit
      @unused ev: scala.collection.Factory[(K, V), M]
  ): IsMap.Of[M, K, V] = Impl.asInstanceOf[IsMap.Of[M, K, V]]
}
private[runtime] trait IsMapImplicits1 extends IsMapImplicits2 { this: IsMap.type =>

  // TotallyBuildMap is supported by design
  implicit def totallyBuildIterableIsCollection[K, V, M](implicit
      @unused ev: TotallyBuildMap[M, K, V]
  ): IsMap.Of[M, K, V] = Impl.asInstanceOf[IsMap.Of[M, K, V]]
}

private[runtime] trait IsMapImplicits2 { this: IsMap.type =>

  // PartiallyBuildMap is supported by design
  implicit def partiallyBuildIterableIsCollection[K, V, M](implicit
      @unused ev: PartiallyBuildMap[M, K, V]
  ): IsMap.Of[M, K, V] = Impl.asInstanceOf[IsMap.Of[M, K, V]]
}
