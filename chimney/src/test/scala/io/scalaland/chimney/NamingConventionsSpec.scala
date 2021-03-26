package io.scalaland.chimney

import utest._
import io.scalaland.chimney.dsl._

object NamingConventionsSpec extends TestSuite {

  val tests = Tests {
    "reading snake case objects" - {
      "should remap fields in snake case to camel case equivalent" - {
        val source = SnakeItem("test", 42)

        val target = source.into[CamelItem].enableSnakeToCamel.transform

        target.simpleField ==> source.simple_field
        target.other ==> source.other
      }
    }

    "reading camel case objects" - {
      "should remap fields in camel case to snake case equivalent" - {
        val source = CamelItem("test", 42)
        val target = source.into[SnakeItem].enableCamelToSnake.transform

        target.simple_field ==> source.simpleField
        target.other ==> source.other

      }
    }
  }
}

case class SnakeItem(simple_field: String, other: Int)
case class CamelItem(simpleField: String, other: Int)
