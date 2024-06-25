package io.scalaland.chimney.javacollections

import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.dsl.*

class TotalTransformerJavaPrimitivesSpec extends ChimneySpec {

  group("conversion between java.lang.Boolean and scala.Boolean") {

    val javaValue = java.lang.Boolean.valueOf("true")
    val scalaValue = true

    test("Java to Scala") {
      javaValue.transformInto[Boolean] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Boolean] ==> javaValue
    }
  }

  group("conversion between java.lang.Byte and scala.Byte") {

    val javaValue = java.lang.Byte.decode("#00")
    val scalaValue = 0.toByte

    test("Java to Scala") {
      javaValue.transformInto[Byte] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Byte] ==> javaValue
    }
  }

  group("conversion between java.lang.Character and scala.Char") {

    val javaValue = java.lang.Character.valueOf('c')
    val scalaValue = 'c'

    test("Java to Scala") {
      javaValue.transformInto[Char] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Character] ==> javaValue
    }
  }

  group("conversion between java.lang.Integer and scala.Int") {

    val javaValue = java.lang.Integer.valueOf(1024)
    val scalaValue = 1024

    test("Java to Scala") {
      javaValue.transformInto[Int] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Integer] ==> javaValue
    }
  }

  group("conversion between java.lang.Long and scala.Long") {

    val javaValue = java.lang.Long.valueOf(1024L)
    val scalaValue = 1024L

    test("Java to Scala") {
      javaValue.transformInto[Long] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Long] ==> javaValue
    }
  }

  group("conversion between java.lang.Short and scala.Short") {

    val javaValue = java.lang.Short.valueOf(1024.toShort)
    val scalaValue = 1024.toShort

    test("Java to Scala") {
      javaValue.transformInto[Short] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Short] ==> javaValue
    }
  }

  group("conversion between java.lang.Float and scala.Float") {

    val javaValue = java.lang.Float.valueOf(1024.0f)
    val scalaValue = 1024.0f

    test("Java to Scala") {
      javaValue.transformInto[Float] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Float] ==> javaValue
    }
  }

  group("conversion between java.lang.Double and scala.Double") {

    val javaValue = java.lang.Double.valueOf(1024.0)
    val scalaValue = 1024.0

    test("Java to Scala") {
      javaValue.transformInto[Double] ==> scalaValue
    }

    test("Scala to Java") {
      scalaValue.transformInto[java.lang.Double] ==> javaValue
    }
  }
}
