package io.scalaland.chimney.chimneyextensiontest

/** Special-cased leaf, supported EXCLUSIVELY through the engine-aware [[TestChimneyMacroExtension]] (a
  * `ChimneyMacroExtension` handler): private constructor, no structural single-field/value-class shape and no implicit -
  * so only the pair-specific handler can build it (`Int -> TestSpecialLeaf` total, `String -> TestSpecialLeaf`
  * partial).
  */
final class TestSpecialLeaf private (val value: Int) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestSpecialLeaf => other.value == value
    case _                      => false
  }
  override def hashCode: Int = value.hashCode
  override def toString: String = s"TestSpecialLeaf($value)"
}
object TestSpecialLeaf {
  def wrap(value: Int): TestSpecialLeaf = new TestSpecialLeaf(value)
}

/** Outer type the [[TestChimneyMacroExtension]] handler BUILDS while DEFERRING its N (= 2) inner values back to the
  * engine via `deriveInner` (proving recursive derivation through the SPI). Private constructor: no structural path.
  */
final class TestSpecialBox2[A, B] private (val first: A, val second: B) {
  override def equals(obj: Any): Boolean = obj match {
    case other: TestSpecialBox2[?, ?] => other.first == first && other.second == second
    case _                            => false
  }
  override def hashCode: Int = (first, second).hashCode
  override def toString: String = s"TestSpecialBox2($first, $second)"
}
object TestSpecialBox2 {
  def of[A, B](first: A, second: B): TestSpecialBox2[A, B] = new TestSpecialBox2(first, second)
}

/** Plain source product the specs construct; the handler turns it into a [[TestSpecialBox2]] by deferring both fields.
  */
final case class TestBox2[A, B](first: A, second: B)
