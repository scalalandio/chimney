package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.javabeans.*

import scala.annotation.unused

class PartialTransformerJavaBeanSpec extends ChimneySpec {

  test("automatic reading from Java Bean getters should be disabled by default") {
    compileErrorsFixed(
      """new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).intoPartial[CaseClassWithFlag].transform"""
    ).check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag to io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag",
      "io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag",
      "id: java.lang.String - no accessor named id in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
      "name: java.lang.String - no accessor named name in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
      "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  test("automatic writing to Java Bean setters should be disabled by default") {
    compileErrorsFixed("""CaseClassWithFlag("100", "name", flag = true).intoPartial[JavaBeanTarget].transform""").check(
      "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
      "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
      "derivation from caseclasswithflag: io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget is not supported in Chimney!",
      "Consult https://chimney.readthedocs.io for usage examples."
    )
  }

  group("""setting .withFieldRenamed(_.getFrom, _.to)""") {

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

  group("""settings .withField*(_.getTo, ...) and .withFieldRenamed(_.from, _.getTo)""") {

    test("transform case class to Java Bean, allowing using getters as a way to rename into matching setters") {
      val source = CaseClassWithFlagRenamed("test-id", "test-name", renamedFlag = true)
      val target = source
        .intoPartial[JavaBeanTarget]
        .withFieldConstPartial(_.getId, partial.Result.fromValue(source.id))
        .withFieldComputedPartial(_.getName, cc => partial.Result.fromCatching(cc.name))
        .withFieldRenamed(_.renamedFlag, _.isFlag)
        .transform
        .asOption
        .get

      target.getId ==> source.id
      target.getName ==> source.name
      target.isFlag ==> source.renamedFlag
    }

    test("should fail to compile when getter is not paired with the right setter") {
      compileErrorsFixed(
        """CaseClassWithFlagRenamed("test-id", "test-name", renamedFlag = true).intoPartial[JavaBeanTargetNoIdSetter].withFieldRenamed(_.id, _.getId).transform"""
      ).check(
        "Assumed that parameter/setter getId is a part of io.scalaland.chimney.fixtures.javabeans.JavaBeanTargetNoIdSetter, but wasn't found"
      )
    }
  }

  group("""flag .enableBeanGetters""") {

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
      compileErrorsFixed("""
             case class MistypedTarget(flag: Int)
             class MistypedSource(private var flag: Int) {
               def isFlag: Int = flag
             }
             new MistypedSource(1).intoPartial[MistypedTarget].enableBeanGetters.transform
          """)
        .check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerJavaBeanSpec.MistypedSource to io.scalaland.chimney.PartialTransformerJavaBeanSpec.MistypedTarget"
        )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableBeanGetters

        compileErrorsFixed("""
               case class MistypedTarget(flag: Int)
               class MistypedSource(private var flag: Int) {
                 def isFlag: Int = flag
               }
               new MistypedSource(1).intoPartial[MistypedTarget].transform
            """)
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerJavaBeanSpec.MistypedSource to io.scalaland.chimney.PartialTransformerJavaBeanSpec.MistypedTarget"
          )
      }
    }
  }

  group("""flag .disableBeanGetters""") {

    test("should disable globally enabled .enableBeanGetters") {
      @unused implicit val config = TransformerConfiguration.default.enableBeanGetters

      compileErrorsFixed(
        """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .intoPartial[CaseClassWithFlag]
              .disableBeanGetters
              .transform
          """
      ).check(
        "id: java.lang.String - no accessor named id in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
        "name: java.lang.String - no accessor named name in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag",
        "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.JavaBeanSourceWithFlag"
      )
    }
  }

  group("""flag .enableBeanSetters""") {

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
      compileErrorsFixed("""
            CaseClassNoFlag("100", "name")
              .intoPartial[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
        .check(
          "",
          "setFlag(flag: scala.Boolean) - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassNoFlag"
        )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableBeanSetters

        compileErrorsFixed("""
              CaseClassNoFlag("100", "name")
                .intoPartial[JavaBeanTarget]
                .transform
            """)
          .check(
            "setFlag(flag: scala.Boolean) - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassNoFlag"
          )
      }
    }

    test("should not compile when method accessor is disabled") {

      compileErrorsFixed("""
            CaseClassWithFlagMethod("100", "name")
              .intoPartial[JavaBeanTarget]
              .enableBeanSetters
              .transform
          """)
        .check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "setFlag(flag: scala.Boolean) - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod",
          "There are methods in io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
          "Consult https://chimney.readthedocs.io for usage examples."
        )

      locally {
        @unused implicit val config = TransformerConfiguration.default.enableBeanSetters

        compileErrorsFixed("""
              CaseClassWithFlagMethod("100", "name")
                .intoPartial[JavaBeanTarget]
                .transform
            """)
          .check(
            "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
            "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
            "setFlag(flag: scala.Boolean) - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod",
            "There are methods in io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
            "Consult https://chimney.readthedocs.io for usage examples."
          )
      }
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

  group("""flag .disableBeanSetters""") {

    test("should disable globally enabled .enableBeanSetters") {
      @unused implicit val config = TransformerConfiguration.default.enableBeanSetters

      compileErrorsFixed("""
            CaseClassWithFlag("100", "name", flag = true)
              .intoPartial[JavaBeanTarget]
              .disableBeanSetters
              .transform
          """)
        .check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "derivation from caseclasswithflag: io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlag to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget is not supported in Chimney!",
          "Consult https://chimney.readthedocs.io for usage examples."
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

  group("""flag .disableMethodAccessors""") {

    test("should disable globally enabled .MethodAccessors") {
      @unused implicit val config = TransformerConfiguration.default.enableMethodAccessors

      compileErrorsFixed("""
            CaseClassWithFlagMethod("100", "name")
              .intoPartial[JavaBeanTarget]
              .enableBeanSetters
              .disableMethodAccessors
              .transform
          """)
        .check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod to io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget",
          "setFlag(flag: scala.Boolean) - no accessor named flag in source type io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod",
          "There are methods in io.scalaland.chimney.fixtures.javabeans.CaseClassWithFlagMethod that might be used as accessors for `flag` fields in io.scalaland.chimney.fixtures.javabeans.JavaBeanTarget. Consider using `.enableMethodAccessors`.",
          "Consult https://chimney.readthedocs.io for usage examples."
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
      compileErrorsFixed("source.intoPartial[JavaBeanTarget].transform").arePresent()
      compileErrorsFixed("source.intoPartial[JavaBeanTarget].enableBeanGetters.transform").arePresent()
      compileErrorsFixed("source.intoPartial[JavaBeanTarget].enableBeanSetters.transform").arePresent()

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
