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
        source
          .into[CaseClassWithFlag]
          .enableBeanGetters
          .transform
          .equalsToBean(source) ==> true
      }

      "not compile when bean getter lookup is disabled" - {

        compileError(
          """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).into[CaseClassWithFlag].transform
          """
        ).check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.JavaBeanSourceWithFlag to io.scalaland.chimney.CaseClassWithFlag"
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
          .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")
      }
    }

    "writing to Java beans" - {

      "convert case class to java bean using setters on the target object" - {

        val expected = new JavaBeanTarget
        expected.setId("100")
        expected.setName("name")
        expected.setFlag(true)

        CaseClassWithFlag("100", "name", flag = true)
          .into[JavaBeanTarget]
          .enableBeanSetters
          .transform ==> expected
      }

      "not compile when bean setters are not enabled" - {

        compileError("""
            CaseClassWithFlag("100", "name", flag = true)
              .into[JavaBeanTarget]
              .transform
          """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.CaseClassWithFlag to io.scalaland.chimney.JavaBeanTarget"
          )
      }

      "not compile when accessors are missing" - {

        compileError("""
            CaseClassNoFlag("100", "name")
              .into[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.CaseClassNoFlag to io.scalaland.chimney.JavaBeanTarget"
          )
      }

      "not compile when method accessor is disabled" - {
        compileError("""
            CaseClassWithFlagMethod("100", "name")
              .into[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.CaseClassWithFlagMethod to io.scalaland.chimney.JavaBeanTarget",
            "io.scalaland.chimney.JavaBeanTarget",
            "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.CaseClassWithFlagMethod",
            "There are methods in io.scalaland.chimney.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
      }

      "works if transform is configured with .enableMethodAccessors" - {
        val expected = new JavaBeanTarget
        expected.setId("100")
        expected.setName("name")
        expected.setFlag(true)

        CaseClassWithFlagMethod("100", "name")
          .into[JavaBeanTarget]
          .enableBeanSetters
          .enableMethodAccessors
          .transform ==> expected
      }

      "convert java bean to java bean" - {

        val source = new JavaBeanSourceWithFlag("200", "name", flag = false)

        val expected = new JavaBeanTarget
        expected.setId("200")
        expected.setName("name")
        expected.setFlag(false)

        // need to enable both setters and getters; only one of them is not enough for this use case!
        compileError("source.into[JavaBeanTarget].transform")
        compileError("source.into[JavaBeanTarget].enableBeanGetters.transform")
        compileError("source.into[JavaBeanTarget].enableBeanSetters.transform")

        source
          .into[JavaBeanTarget]
          .enableBeanGetters
          .enableBeanSetters
          .transform ==> expected
      }

      "convert to java bean involving recursive transformation" - {

        val expected = new EnclosingBean
        expected.setCcNoFlag(CaseClassNoFlag("300", "name"))

        EnclosingCaseClass(CaseClassNoFlag("300", "name"))
          .into[EnclosingBean]
          .enableBeanSetters
          .transform ==> expected
      }
    }

    "scoped Java beans configuration" - {

      implicit val transformerConfiguration = {
        TransformerConfiguration.default.enableBeanGetters.enableBeanSetters
      }
      val source = new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)

      "work without enabling flags" - {

        "beans reading" - {
          source.transformInto[CaseClassWithFlag].equalsToBean(source) ==> true
          source
            .into[CaseClassWithFlag]
            .disableBeanSetters // not needed when reading from bean
            .transform
            .equalsToBean(source) ==> true

          source.transformIntoF[Option, CaseClassWithFlag].get.equalsToBean(source) ==> true
          source
            .intoF[Option, CaseClassWithFlag]
            .disableBeanSetters // not needed when reading from bean
            .transform
            .get
            .equalsToBean(source)
        }

        "beans writing" - {
          val expected = new JavaBeanTarget
          expected.setId("100")
          expected.setName("name")
          expected.setFlag(true)

          CaseClassWithFlag("100", "name", flag = true).transformInto[JavaBeanTarget] ==> expected
          CaseClassWithFlag("100", "name", flag = true)
            .into[JavaBeanTarget]
            .disableBeanGetters // not needed when writing to bean
            .transform ==> expected

          CaseClassWithFlag("100", "name", flag = true).transformIntoF[Option, JavaBeanTarget] ==> Some(expected)
          CaseClassWithFlag("100", "name", flag = true)
            .intoF[Option, JavaBeanTarget]
            .disableBeanGetters // not needed when writing to bean
            .transform ==> Some(expected)
        }
      }

      "not work when disabled locally" - {

        "beans reading" - {
          compileError("""
            source.into[CaseClassWithFlag].disableBeanGetters.transform
          """)
            .check(
              "",
              "Chimney can't derive transformation from io.scalaland.chimney.JavaBeanSourceWithFlag to io.scalaland.chimney.CaseClassWithFlag"
            )
        }

        "beans writing" - {
          compileError("""
            CaseClassWithFlag("100", "name", flag = true).into[JavaBeanTarget].disableBeanSetters.transform
          """)
            .check(
              "",
              "Chimney can't derive transformation from io.scalaland.chimney.CaseClassWithFlag to io.scalaland.chimney.JavaBeanTarget"
            )
        }
      }
    }
  }
}

case class CaseClassNoFlag(id: String, name: String)
case class CaseClassWithFlagMethod(id: String, name: String) {
  def flag: Boolean = true
}

case class CaseClassWithFlag(id: String, name: String, flag: Boolean) {
  def equalsToBean(jbswf: JavaBeanSourceWithFlag): Boolean = {
    id == jbswf.getId && name == jbswf.getName && flag == jbswf.isFlag
  }
}

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

  // make sure that only public setters are taken into account
  protected def setFoo(foo: Unit): Unit = ()
  private def setBar(bar: Int): Unit = ()

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

case class EnclosingCaseClass(ccNoFlag: CaseClassNoFlag)

class EnclosingBean {
  private var ccNoFlag: CaseClassNoFlag = _

  def getCcNoFlag: CaseClassNoFlag = ccNoFlag
  def setCcNoFlag(ccNoFlag: CaseClassNoFlag): Unit = { this.ccNoFlag = ccNoFlag }

  override def equals(obj: Any): Boolean = {
    obj match {
      case eb: EnclosingBean =>
        this.ccNoFlag == eb.ccNoFlag
      case _ =>
        false
    }
  }
}
