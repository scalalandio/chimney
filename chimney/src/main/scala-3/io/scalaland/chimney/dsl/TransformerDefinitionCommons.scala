package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.runtime.TransformerCfg
import scala.annotation.static

object TransformerDefinitionCommons {
  type RuntimeDataStore = Vector[Any]
  final def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
  // @static annotation breaks Scala.js 3 with:
  // [error] ## Exception when compiling 73 sources to /Users/dev/Workspaces/GitHub/chimney/chimney/target/js-3/classes
  // [error] java.lang.AssertionError: assertion failed: Trying to access the this of another class:
  //   tree.symbol = trait TransformerDefinitionCommons, class symbol = module class extensions$package$
  // @static final def emptyRuntimeDataStore: RuntimeDataStore = Vector.empty[Any]
}

private[chimney] trait TransformerDefinitionCommons[UpdateCfg[_ <: TransformerCfg]] {

  import TransformerDefinitionCommons.*

  /** runtime storage for values and functions that transformer definition is customized with */
  val runtimeData: RuntimeDataStore
}
