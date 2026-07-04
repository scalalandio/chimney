package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.language.reflectiveCalls

/** Scala 2-only: `val` members of structural refinement bounds are readable accessors OUT OF THE BOX (no
  * `.enableMethodAccessors` needed) - 1.x parity.
  *
  * Regression test for the Hearth migration: scalac represents `val value: String` of a refinement as a deferred STABLE
  * method with no accessed field, which Hearth 0.4.0's `Method.isVal` did not count as a val (hearth#326, fixed in
  * 0.4.1) - the getter was demoted from `ConstructorBodyVal` to the flag-gated `AccessorMethod` and the derivation
  * failed with "Consider using .enableMethodAccessors" (caught by the docs "Parametric types" snippet).
  *
  * On Scala 3 refinement members were never visible to the derivation (1.x parity, pinned in the scala-3 twin of this
  * spec).
  */
class TransformerStructuralRefinementSpec extends ChimneySpec {

  import TransformerStructuralRefinementSpec.*

  test("total transformation should read vals of a structural-refinement-bounded type param without any flag") {
    def refinedExample[A <: { val value: String }](foo: Foo[A]): Bar[Bar[String]] =
      foo.transformInto[Bar[Bar[String]]]

    refinedExample[Foo[String]](Foo(Foo("value"))) ==> Bar(Bar("value"))
  }

  test("partial transformation should read vals of a structural-refinement-bounded type param without any flag") {
    def refinedExample[A <: { val value: String }](foo: Foo[A]): partial.Result[Bar[Bar[String]]] =
      foo.transformIntoPartial[Bar[Bar[String]]]

    refinedExample[Foo[String]](Foo(Foo("value"))).asOption ==> Some(Bar(Bar("value")))
  }
}
object TransformerStructuralRefinementSpec {

  case class Foo[A](value: A)
  case class Bar[A](value: A)
}
