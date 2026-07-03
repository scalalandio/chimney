package io.scalaland.chimney

/** Scala 3 twin of the scala-2 `TransformerStructuralRefinementSpec`: on Scala 3, `val` members of structural
  * refinement bounds are NOT visible to the derivation - 1.x parity (verified against chimney 1.8.2, which fails with
  * the same "no accessor named value"; Scala 3 structural types are `Selectable`-based and the old engine's
  * `fieldMembers`-based extraction never listed refinement members). Pinned so an intentional future change is loud.
  */
class TransformerStructuralRefinementSpec extends ChimneySpec {

  test("transformation from a structural-refinement-bounded type param stays unsupported (1.x parity, pinned)") {
    compileErrors(
      """
      import io.scalaland.chimney.dsl.*

      case class Foo[A](value: A)
      case class Bar[A](value: A)

      def refinedExample[A <: { val value: String }](foo: Foo[A]): Bar[Bar[String]] =
        foo.transformInto[Bar[Bar[String]]]
      """
    ).check(
      "Chimney can't derive transformation from",
      "no accessor named value in source type"
    )
  }
}
