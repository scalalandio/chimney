package io.scalaland.chimney

import utest.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.javabeans.*

object PartialTransformerJavaBeanSpec extends TestSuite {

  val tests = Tests {

    test("automatic reading from Java Bean getters should be disabled by default") {
      compileError(
        """new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).intoPartial[CaseClassWithFlag].transform"""
      ).check(
        "",
        "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag to io.scalaland.chimney.examples.javabeans.CaseClassWithFlag",
        "io.scalaland.chimney.examples.javabeans.CaseClassWithFlag",
        "id: java.lang.String - no accessor named id in source type io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag",
        "name: java.lang.String - no accessor named name in source type io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag",
        "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag",
        "Consult https://scalalandio.github.io/chimney for usage examples."
      )
    }

    test("automatic writing to Java Bean setters should be disabled by default") {
      compileError("""CaseClassWithFlag("100", "name", flag = true).intoPartial[JavaBeanTarget].transform""").check(
        "",
        "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
        "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
        "derivation from caseclasswithflag: io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget is not supported in Chimney!",
        "Consult https://scalalandio.github.io/chimney for usage examples."
      )
    }

    test("""setting .withFieldRenamed(_.getFrom, _.to)""") {

      test("transform Java Bean to case class when all getters are passed explicitly") {
        val source = new JavaBeanSource("test-id", "test-name")
        val target = source
          .intoPartial[CaseClassNoFlag]
          .withFieldRenamed(_.getId, _.id)
          .withFieldRenamed(_.getName, _.name)
          .transform
          .asOption
          .get

        target.id ==> source.getId
        target.name ==> source.getName
      }
    }

    test("""flag .enableBeanGetters""") {

      test("should enable automatic reading from Java Bean getters") {
        val source = new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
        source
          .intoPartial[CaseClassWithFlag]
          .enableBeanGetters
          .transform
          .asOption
          .get
          .equalsToBean(source) ==> true

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanGetters

          source
            .intoPartial[CaseClassWithFlag]
            .transform
            .asOption
            .get
            .equalsToBean(source) ==> true
        }
      }

      test("not compile when matching an is- getter with type other than Boolean") {
        compileError("""
             case class MistypedTarget(flag: Int)
             class MistypedSource(private var flag: Int) {
               def isFlag: Int = flag
             }
             new MistypedSource(1).intoPartial[MistypedTarget].enableBeanGetters.transform
          """)
          .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanGetters

          compileError("""
               case class MistypedTarget(flag: Int)
               class MistypedSource(private var flag: Int) {
                 def isFlag: Int = flag
               }
               new MistypedSource(1).intoPartial[MistypedTarget].transform
            """)
            .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")
        }
      }
    }

    test("""flag .disableBeanGetters""") {

      test("should disable globally enabled .enableBeanGetters") {
        implicit val config = TransformerConfiguration.default.enableBeanGetters

        compileError(
          """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .intoPartial[CaseClassWithFlag]
              .disableBeanGetters
              .transform
          """
        ).check(
          "",
          "id: java.lang.String - no accessor named id in source type io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag",
          "name: java.lang.String - no accessor named name in source type io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag",
          "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.JavaBeanSourceWithFlag"
        )
      }
    }

    test("""flag .enableBeanSetters""") {

      test("should enable automatic writing to Java Bean setters") {
        val expected = new JavaBeanTarget
        expected.setId("100")
        expected.setName("name")
        expected.setFlag(true)

        CaseClassWithFlag("100", "name", flag = true)
          .intoPartial[JavaBeanTarget]
          .enableBeanSetters
          .transform
          .asOption ==> Some(expected)

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanSetters

          CaseClassWithFlag("100", "name", flag = true)
            .intoPartial[JavaBeanTarget]
            .transform
            .asOption ==> Some(expected)
        }
      }

      test("should not compile when accessors are missing") {
        compileError("""
            CaseClassNoFlag("100", "name")
              .intoPartial[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
          .check(
            "",
            "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassNoFlag"
          )

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanSetters

          compileError("""
              CaseClassNoFlag("100", "name")
                .intoPartial[JavaBeanTarget]
                .transform
            """)
            .check(
              "",
              "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassNoFlag"
            )
        }
      }

      test("should not compile when method accessor is disabled") {
        compileError("""
            CaseClassWithFlagMethod("100", "name")
              .intoPartial[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod",
            "There are methods in io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.examples.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanSetters

          compileError("""
              CaseClassWithFlagMethod("100", "name")
                .intoPartial[JavaBeanTarget]
                .transform
            """)
            .check(
              "",
              "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
              "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
              "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod",
              "There are methods in io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.examples.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
              "Consult https://scalalandio.github.io/chimney for usage examples."
            )
        }

        test("should transform to Java Bean involving recursive transformation") {
          val expected = new EnclosingBean
          expected.setCcNoFlag(CaseClassNoFlag("300", "name"))

          EnclosingCaseClass(CaseClassNoFlag("300", "name"))
            .intoPartial[EnclosingBean]
            .enableBeanSetters
            .transform
            .asOption ==> Some(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanSetters

            EnclosingCaseClass(CaseClassNoFlag("300", "name"))
              .intoPartial[EnclosingBean]
              .transform
              .asOption ==> Some(expected)
          }
        }
      }
    }

    test("""flag .disableBeanSetters""") {

      test("should disable globally enabled .enableBeanSetters") {
        implicit val config = TransformerConfiguration.default.enableBeanSetters

        compileError("""
            CaseClassWithFlag("100", "name", flag = true)
              .intoPartial[JavaBeanTarget]
              .disableBeanSetters
              .transform
          """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "derivation from caseclasswithflag: io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget is not supported in Chimney!",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
      }
    }

    test("""flag .enableMethodAccessors""") {

      test(
        "should enable reading from def methods other than case class vals and cooperate with writing to Java Beans"
      ) {
        val expected = new JavaBeanTarget
        expected.setId("100")
        expected.setName("name")
        expected.setFlag(true)

        CaseClassWithFlagMethod("100", "name")
          .intoPartial[JavaBeanTarget]
          .enableBeanSetters
          .enableMethodAccessors
          .transform
          .asOption ==> Some(expected)

        locally {
          implicit val config = TransformerConfiguration.default.enableMethodAccessors

          CaseClassWithFlagMethod("100", "name")
            .intoPartial[JavaBeanTarget]
            .enableBeanSetters
            .transform
            .asOption ==> Some(expected)
        }
      }
    }

    test("""flag .enableMethodAccessors""") {

      test("should disable globally enabled .MethodAccessors") {
        implicit val config = TransformerConfiguration.default.enableMethodAccessors

        compileError("""
            CaseClassWithFlagMethod("100", "name")
              .intoPartial[JavaBeanTarget]
              .enableBeanSetters
              .disableMethodAccessors
              .transform
          """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod",
            "There are methods in io.scalaland.chimney.examples.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.examples.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
      }
    }

    test("""flags .enableBeanGetters and .enableBeanSetters together""") {

      test("should transform Java Bean to Java Bean") {
        val source = new JavaBeanSourceWithFlag("200", "name", flag = false)

        val expected = new JavaBeanTarget
        expected.setId("200")
        expected.setName("name")
        expected.setFlag(false)

        // need to enable both setters and getters; only one of them is not enough for this use case!
        compileError("source.intoPartial[JavaBeanTarget].transform")
        compileError("source.intoPartial[JavaBeanTarget].enableBeanGetters.transform")
        compileError("source.intoPartial[JavaBeanTarget].enableBeanSetters.transform")

        source
          .intoPartial[JavaBeanTarget]
          .enableBeanGetters
          .enableBeanSetters
          .transform
          .asOption ==> Some(expected)

        locally {
          implicit val config = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

          source.intoPartial[JavaBeanTarget].transform.asOption ==> Some(expected)
        }
      }
    }
  }
}
