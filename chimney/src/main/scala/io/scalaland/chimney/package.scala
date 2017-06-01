package io.scalaland

import shapeless.HList

package object chimney {

  type DerivedTransformer[From, To, Modifiers <: HList] = DerivedTransformerG[From, From, To, Modifiers]
}
