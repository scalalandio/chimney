package io.scalaland.chimney.javacollections

import io.scalaland.chimney.Transformer

/** @since 1.2.0 */
trait JavaPrimitivesImplicits {

  // Java to Scala

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaBooleanToScalaBoolean: Transformer[java.lang.Boolean, Boolean] = _.booleanValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaByteToScalaByte: Transformer[java.lang.Byte, Byte] = _.byteValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaCharacterToScalaChar: Transformer[java.lang.Character, Char] = _.charValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaIntegerToScalaInt: Transformer[java.lang.Integer, Int] = _.intValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaLongToScalaLong: Transformer[java.lang.Long, Long] = _.longValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaShortToScalaShort: Transformer[java.lang.Short, Short] = _.shortValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaFloatToScalaFloat: Transformer[java.lang.Float, Float] = _.floatValue()

  /** @since 1.2.0 */
  implicit val totalTransformerFromJavaDoubleToScalaDouble: Transformer[java.lang.Double, Double] = _.doubleValue()

  // Scala to Java

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaBooleanToJavaBoolean: Transformer[Boolean, java.lang.Boolean] = b => b

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaByteToJavaByte: Transformer[Byte, java.lang.Byte] = b => b

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaCharToJavaCharacter: Transformer[Char, java.lang.Character] = c => c

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaIntToJavaInteger: Transformer[Int, java.lang.Integer] = i => i

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaLongToJavaLong: Transformer[Long, java.lang.Long] = l => l

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaShortToJavaShort: Transformer[Short, java.lang.Short] = s => s

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaFloatToJavaFloat: Transformer[Float, java.lang.Float] = f => f

  /** @since 1.2.0 */
  implicit val totalTransformerFromScalaDoubleToJavaDouble: Transformer[Double, java.lang.Double] = d => d
}
