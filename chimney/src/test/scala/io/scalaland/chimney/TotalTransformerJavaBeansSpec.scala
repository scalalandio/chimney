package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.javabeans.*

import scala.annotation.unused

class TotalTransformerJavaBeansSpec extends ChimneySpec {

  // FIXME: this test fail on Scala 3, even though the error message is as it should be!
  /*
  test("automatic reading from Java Bean getters should be disabled by default") {
    compileErrors(
      """new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).into[CaseClassWithFlag].transform"""
    ).check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag to io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag",
      "io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag",
      "id: java.lang.String - no accessor named id in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
      "name: java.lang.String - no accessor named name in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
      "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
      "Consult https://scalalandio.github.io/chimney for usage examples."
    )
  }
   */

  // FIXME: this test fail on Scala 3, even though the error message is as it should be!
  /*
  test("automatic writing to Java Bean setters should be disabled by default") {
    compileErrors("""CaseClassWithFlag("100", "name", flag = true).into[JavaBeanTarget].transform""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
      "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
      "derivation from caseclasswithflag: io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget is not supported in Chimney!",
      "Consult https://scalalandio.github.io/chimney for usage examples."
    )
  }
   */

  group("""setting .withFieldRenamed(_.getFrom, _.to)""") {

    // FIXME: check if setters are used instead of assuming that more than 1 setter field -> crash
    /*
    test("transform Java Bean to case class when all getters are passed explicitly") {
      val source = new JavaBeanSource("test-id", "test-name")
      val target = source
        .into[CaseClassNoFlag]
        .withFieldRenamed(_.getId, _.id)
        .withFieldRenamed(_.getName, _.name)
        .transform

      target.id ==> source.getId
      target.name ==> source.getName
    }
     */
  }

  group("""flag .enableBeanGetters""") {

    // FIXME: we use wrong naming - Java Bean should have e.g. default constructor while this spec describes POJO
    // test uses non-default constructor
    /*
    test("should enable automatic reading from Java Bean getters") {
      val source = new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
      source
        .into[CaseClassWithFlag]
        .enableBeanGetters
        .transform
        .equalsToBean(source) ==> true

      locally {
        implicit val config = TransformerConfiguration.default.enableBeanGetters

        source
          .into[CaseClassWithFlag]
          .transform
          .equalsToBean(source) ==> true
      }
    }
     */

    // FIXME: I'm not doing that check
    /*
    test("not compile when matching an is- getter with type other than Boolean") {
      compileErrors("""
             case class MistypedTarget(flag: Int)
             class MistypedSource(private var flag: Int) {
               def isFlag: Int = flag
             }
             new MistypedSource(1).into[MistypedTarget].enableBeanGetters.transform
          """)
        .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableBeanGetters

        compileErrors("""
               case class MistypedTarget(flag: Int)
               class MistypedSource(private var flag: Int) {
                 def isFlag: Int = flag
               }
               new MistypedSource(1).into[MistypedTarget].transform
            """)
          .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")
      }
    }
     */
  }

  group("""flag .disableBeanGetters""") {

    test("should disable globally enabled .enableBeanGetters") {
      @unused implicit val config = TransformerConfiguration.default.enableBeanGetters

      compileErrors(
        """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .into[CaseClassWithFlag]
              .disableBeanGetters
              .transform
          """
      ).check(
        "",
        "id: java.lang.String - no accessor named id in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
        "name: java.lang.String - no accessor named name in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
        "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag"
      )
    }
  }

  // FIXME: we use wrong naming - Java Bean should have e.g. default constructor while this spec describes POJO
  // test uses non-default constructor
  /*
  group("""flag .enableBeanSetters""") {

    test("should enable automatic writing to Java Bean setters") {
      val expected = new JavaBeanTarget
      expected.setId("100")
      expected.setName("name")
      expected.setFlag(true)

      CaseClassWithFlag("100", "name", flag = true)
        .into[JavaBeanTarget]
        .enableBeanSetters
        .transform ==> expected

      locally {
        implicit val config = TransformerConfiguration.default.enableBeanSetters

        CaseClassWithFlag("100", "name", flag = true)
          .into[JavaBeanTarget]
          .transform ==> expected
      }
    }

    test("should not compile when accessors are missing") {
      compileErrors("""
            CaseClassNoFlag("100", "name")
              .into[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
        .check(
          "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassNoFlag"
        )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableBeanSetters

        compileErrors("""
              CaseClassNoFlag("100", "name")
                .into[JavaBeanTarget]
                .transform
            """)
          .check(
            "",
            "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassNoFlag"
          )
      }
    }

    test("should not compile when method accessor is disabled") {
      compileErrors("""
            CaseClassWithFlagMethod("100", "name")
              .into[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod",
          "There are methods in io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableBeanSetters

        compileErrors("""
              CaseClassWithFlagMethod("100", "name")
                .into[JavaBeanTarget]
                .transform
            """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
            "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
            "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod",
            "There are methods in io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
      }

      test("should transform to Java Bean involving recursive transformation") {
        val expected = new EnclosingBean
        expected.setCcNoFlag(CaseClassNoFlag("300", "name"))

        EnclosingCaseClass(CaseClassNoFlag("300", "name"))
          .into[EnclosingBean]
          .enableBeanSetters
          .transform ==> expected

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanSetters

          EnclosingCaseClass(CaseClassNoFlag("300", "name"))
            .into[EnclosingBean]
            .transform ==> expected
        }
      }
    }
  }

  group("""flag .disableBeanSetters""") {

    test("should disable globally enabled .enableBeanSetters") {
      @unused implicit val config = TransformerConfiguration.default.enableBeanSetters

      compileErrors("""
            CaseClassWithFlag("100", "name", flag = true)
              .into[JavaBeanTarget]
              .disableBeanSetters
              .transform
          """)
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "derivation from caseclasswithflag: io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget is not supported in Chimney!",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
    }
  }

  group("""flag .enableMethodAccessors""") {

    test(
      "should enable reading from def methods other than case class vals and cooperate with writing to Java Beans"
    ) {
      val expected = new JavaBeanTarget
      expected.setId("100")
      expected.setName("name")
      expected.setFlag(true)

      CaseClassWithFlagMethod("100", "name")
        .into[JavaBeanTarget]
        .enableBeanSetters
        .enableMethodAccessors
        .transform ==> expected

      locally {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        CaseClassWithFlagMethod("100", "name")
          .into[JavaBeanTarget]
          .enableBeanSetters
          .transform ==> expected
      }
    }
  }

  group("""flag .enableMethodAccessors""") {

    test("should disable globally enabled .MethodAccessors") {
      @unused implicit val config = TransformerConfiguration.default.enableMethodAccessors

      compileErrors("""
            CaseClassWithFlagMethod("100", "name")
              .into[JavaBeanTarget]
              .enableBeanSetters
              .disableMethodAccessors
              .transform
          """)
        .check(
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod",
          "There are methods in io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
    }
  }

  group("""flags .enableBeanGetters and .enableBeanSetters together""") {

    test("should transform Java Bean to Java Bean") {
      val source = new JavaBeanSourceWithFlag("200", "name", flag = false)

      val expected = new JavaBeanTarget
      expected.setId("200")
      expected.setName("name")
      expected.setFlag(false)

      // need to enable both setters and getters; only one of them is not enough for this use case!
      compileErrors("source.into[JavaBeanTarget].transform").arePresent()
      compileErrors("source.into[JavaBeanTarget].enableBeanGetters.transform").arePresent()
      compileErrors("source.into[JavaBeanTarget].enableBeanSetters.transform").arePresent()

      source
        .into[JavaBeanTarget]
        .enableBeanGetters
        .enableBeanSetters
        .transform ==> expected

      locally {
        implicit val config = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

        source.into[JavaBeanTarget].transform ==> expected
      }
    }
  }
   */
}
