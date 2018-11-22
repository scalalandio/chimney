package io.scalaland.chimney

import utest._
import io.scalaland.chimney.dsl._

object JavaBeansSpec extends TestSuite {

  val tests = Tests {

    "reading from Java beans" - {

      "work with basic renaming when bean getter lookup is disabled" - {
        val source = new JavaBeanSource("test-id", "test-name")
        val target = source
          .into[CaseClassNoFlag]
          .withFieldRenamed(_.getId, _.id)
          .withFieldRenamed(_.getName, _.name)
          .transform

        target.id ==> source.getId
        target.name ==> source.getName
      }

      "support automatic reading from java bean getters" - {

        val source = new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
        val target = source
          .into[CaseClassWithFlag]
          .enableBeanGetters
          .transform

        target.id ==> source.getId
        target.name ==> source.getName
        target.flag ==> source.isFlag
      }

      "not compile when bean getter lookup is disabled" - {
        compileError(
          """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).into[CaseClassWithFlag].transform
          """
        )
      }

      "not compile when matching an is- getter with type other than Boolean" - {
        compileError("""
             case class MistypedTarget(flag: Int)
             class MistypedSource(private var flag: Int) {
               def isFlag: Int = flag
             }
             new MistypedSource(1).into[MistypedTarget].enableBeanGetters.transform
          """)
      }
    }

//    "writing to Java beans" - {
//
//      "convert case class to java bean using setters on the target object" - {
//
//        val expected = new JavaBeanTarget
//        expected.setId("100")
//        expected.setName("name")
//        expected.setFlag(true)
//
//        CaseClassWithFlag("100", "name", flag = true)
//          .into[JavaBeanTarget]
//          .enableBeanSetters
//          .transform ==> expected
//      }
//    }
  }

}

case class CaseClassNoFlag(id: String, name: String)

case class CaseClassWithFlag(id: String, name: String, flag: Boolean)

class JavaBeanSource(id: String, name: String) {
  def getId: String = id
  def getName: String = name
}

class JavaBeanSourceWithFlag(private var id: String, private var name: String, private var flag: Boolean) {
  def getId: String = id
  def getName: String = name
  def isFlag: Boolean = flag
}

class JavaBeanTarget {
  private var id: String = _
  private var name: String = _
  private var flag: Boolean = _

  def setId(id: String): Unit = { this.id = id }
  def setName(name: String): Unit = { this.name = name }
  def setFlag(flag: Boolean): Unit = { this.flag = flag }

  def getId: String = id
  def getName: String = name
  def isFlag: Boolean = flag

  override def equals(obj: Any): Boolean = {
    obj match {
      case jbt: JavaBeanTarget =>
        this.id == jbt.getId && this.name == jbt.getName && this.flag == jbt.isFlag
      case _ =>
        false
    }
  }
}
