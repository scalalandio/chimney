package io.scalaland.chimney

import utest.*
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.javabeans.*

object LiftedTransformerJavaBeanSpec extends TestSuite {

  val tests = Tests {

    test("automatic reading from Java Bean getters should be disabled by default") {

      test("when F = Option") {
        compileError(
          """new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).intoF[Option, CaseClassWithFlag].transform"""
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

      test("when F = Either[List[String], +*]") {
        type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
        compileError(
          """new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true).intoF[EitherList, CaseClassWithFlag].transform"""
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
    }

    test("automatic writing to Java Bean setters should be disabled by default") {

      test("when F = Option") {
        compileError("""CaseClassWithFlag("100", "name", flag = true).intoF[Option, JavaBeanTarget].transform""").check(
          "",
          "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
          "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
          "derivation from caseclasswithflag: io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget is not supported in Chimney!",
          "Consult https://scalalandio.github.io/chimney for usage examples."
        )
      }

      test("when F = Either[List[String], +*]") {
        type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
        compileError("""CaseClassWithFlag("100", "name", flag = true).intoF[EitherList, JavaBeanTarget].transform""")
          .check(
            "",
            "Chimney can't derive transformation from io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "io.scalaland.chimney.examples.javabeans.JavaBeanTarget",
            "derivation from caseclasswithflag: io.scalaland.chimney.examples.javabeans.CaseClassWithFlag to io.scalaland.chimney.examples.javabeans.JavaBeanTarget is not supported in Chimney!",
            "Consult https://scalalandio.github.io/chimney for usage examples."
          )
      }
    }

    test("""setting .withFieldRenamed(_.getFrom, _.to)""") {

      test("transform Java Bean to case class when all getters are passed explicitly") {

        test("when F = Option") {
          val source = new JavaBeanSource("test-id", "test-name")
          val target = source
            .intoF[Option, CaseClassNoFlag]
            .withFieldRenamed(_.getId, _.id)
            .withFieldRenamed(_.getName, _.name)
            .transform
            .get

          target.id ==> source.getId
          target.name ==> source.getName
        }

        test("when F = Either[List[String], +*]") {
          val source = new JavaBeanSource("test-id", "test-name")
          val target = source
            .intoF[Either[List[String], +*], CaseClassNoFlag]
            .withFieldRenamed(_.getId, _.id)
            .withFieldRenamed(_.getName, _.name)
            .transform
            .toOption
            .get

          target.id ==> source.getId
          target.name ==> source.getName
        }
      }
    }

    test("""flag .enableBeanGetters""") {

      test("should enable automatic reading from Java Bean getters") {

        test("when F = Option") {
          val source = new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
          source
            .intoF[Option, CaseClassWithFlag]
            .enableBeanGetters
            .transform
            .get
            .equalsToBean(source) ==> true

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanGetters

            source
              .intoF[Option, CaseClassWithFlag]
              .transform
              .get
              .equalsToBean(source) ==> true
          }
        }

        test("when F = Either[List[String], +*]") {
          val source = new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
          source
            .intoF[Either[List[String], +*], CaseClassWithFlag]
            .enableBeanGetters
            .transform
            .toOption
            .get
            .equalsToBean(source) ==> true

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanGetters

            source
              .intoF[Either[List[String], +*], CaseClassWithFlag]
              .transform
              .toOption
              .get
              .equalsToBean(source) ==> true
          }
        }
      }

      test("not compile when matching an is- getter with type other than Boolean") {

        test("when F = Option") {
          compileError("""
             case class MistypedTarget(flag: Int)
             class MistypedSource(private var flag: Int) {
               def isFlag: Int = flag
             }
             new MistypedSource(1).intoF[Option, MistypedTarget].enableBeanGetters.transform
          """)
            .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanGetters

            compileError("""
               case class MistypedTarget(flag: Int)
               class MistypedSource(private var flag: Int) {
                 def isFlag: Int = flag
               }
               new MistypedSource(1).intoF[Option, MistypedTarget].transform
            """)
              .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")
          }
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          compileError("""
             case class MistypedTarget(flag: Int)
             class MistypedSource(private var flag: Int) {
               def isFlag: Int = flag
             }
             new MistypedSource(1).intoF[EitherList, MistypedTarget].enableBeanGetters.transform
          """)
            .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanGetters

            compileError("""
               case class MistypedTarget(flag: Int)
               class MistypedSource(private var flag: Int) {
                 def isFlag: Int = flag
               }
               new MistypedSource(1).intoF[EitherList, MistypedTarget].transform
            """)
              .check("", "Chimney can't derive transformation from MistypedSource to MistypedTarget")
          }
        }
      }
    }

    test("""flag .disableBeanGetters""") {

      test("should disable globally enabled .enableBeanGetters") {

        test("when F = Option") {
          implicit val config = TransformerConfiguration.default.enableBeanGetters

          compileError(
            """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .intoF[Option, CaseClassWithFlag]
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

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          implicit val config = TransformerConfiguration.default.enableBeanGetters

          compileError(
            """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .intoF[EitherList, CaseClassWithFlag]
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

      test("should disable globally enabled .enableBeanGetters") {

        test("when F = Option") {
          implicit val config = TransformerConfiguration.default.enableBeanGetters

          compileError(
            """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .intoF[Option, CaseClassWithFlag]
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

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          implicit val config = TransformerConfiguration.default.enableBeanGetters

          compileError(
            """
            new JavaBeanSourceWithFlag(id = "test-id", name = "test-name", flag = true)
              .intoF[EitherList, CaseClassWithFlag]
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
    }

    test("""flag .enableBeanSetters""") {

      test("should enable automatic writing to Java Bean setters") {

        val expected = new JavaBeanTarget
        expected.setId("100")
        expected.setName("name")
        expected.setFlag(true)

        test("when F = Option") {
          CaseClassWithFlag("100", "name", flag = true)
            .intoF[Option, JavaBeanTarget]
            .enableBeanSetters
            .transform ==> Some(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanSetters

            CaseClassWithFlag("100", "name", flag = true).intoF[Option, JavaBeanTarget].transform ==> Some(expected)
          }
        }

        test("when F = Either[List[String], +*]") {
          CaseClassWithFlag("100", "name", flag = true)
            .intoF[Either[List[String], +*], JavaBeanTarget]
            .enableBeanSetters
            .transform ==> Right(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanSetters

            CaseClassWithFlag("100", "name", flag = true)
              .intoF[Either[List[String], +*], JavaBeanTarget]
              .transform ==> Right(expected)
          }
        }
      }

      test("should not compile when accessors are missing") {

        test("when F = Option") {
          compileError("""
            CaseClassNoFlag("100", "name")
              .intoF[Option,JavaBeanTarget]
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
                .intoF[Option, JavaBeanTarget]
                .transform
            """)
              .check(
                "",
                "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassNoFlag"
              )
          }
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          compileError("""
            CaseClassNoFlag("100", "name")
              .intoF[EitherList, JavaBeanTarget]
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
                .intoF[EitherList, JavaBeanTarget]
                .transform
            """)
              .check(
                "",
                "flag: scala.Boolean - no accessor named flag in source type io.scalaland.chimney.examples.javabeans.CaseClassNoFlag"
              )
          }
        }
      }

      test("should not compile when method accessor is disabled") {

        test("when F = Option") {
          compileError("""
            CaseClassWithFlagMethod("100", "name")
              .intoF[Option, JavaBeanTarget]
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
                .intoF[Option,JavaBeanTarget]
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

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          compileError("""
            CaseClassWithFlagMethod("100", "name")
              .intoF[EitherList, JavaBeanTarget]
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
                .intoF[EitherList,JavaBeanTarget]
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
      }

      test("should transform to Java Bean involving recursive transformation") {

        test("when F = Option") {
          val expected = new EnclosingBean
          expected.setCcNoFlag(CaseClassNoFlag("300", "name"))

          EnclosingCaseClass(CaseClassNoFlag("300", "name"))
            .intoF[Option, EnclosingBean]
            .enableBeanSetters
            .transform ==> Some(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanSetters

            EnclosingCaseClass(CaseClassNoFlag("300", "name"))
              .intoF[Option, EnclosingBean]
              .transform ==> Some(expected)
          }
        }

        test("when F = Either[List[String], +*]") {
          val expected = new EnclosingBean
          expected.setCcNoFlag(CaseClassNoFlag("300", "name"))

          EnclosingCaseClass(CaseClassNoFlag("300", "name"))
            .intoF[Either[List[String], +*], EnclosingBean]
            .enableBeanSetters
            .transform ==> Right(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanSetters

            EnclosingCaseClass(CaseClassNoFlag("300", "name"))
              .intoF[Either[List[String], +*], EnclosingBean]
              .transform ==> Right(expected)
          }
        }
      }
    }

    test("""flag .disableBeanSetters""") {

      test("should disable globally enabled .enableBeanSetters") {

        test("when F = Option") {
          implicit val config = TransformerConfiguration.default.enableBeanSetters

          compileError("""
            CaseClassWithFlag("100", "name", flag = true)
              .intoF[Option, JavaBeanTarget]
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

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          implicit val config = TransformerConfiguration.default.enableBeanSetters

          compileError("""
            CaseClassWithFlag("100", "name", flag = true)
              .intoF[EitherList, JavaBeanTarget]
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
    }

    test("""flag .enableMethodAccessors""") {

      test(
        "should enable reading from def methods other than case class vals and cooperate with writing to Java Beans"
      ) {

        val expected = new JavaBeanTarget
        expected.setId("100")
        expected.setName("name")
        expected.setFlag(true)

        test("when F = Option") {
          CaseClassWithFlagMethod("100", "name")
            .intoF[Option, JavaBeanTarget]
            .enableBeanSetters
            .enableMethodAccessors
            .transform ==> Some(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableMethodAccessors

            CaseClassWithFlagMethod("100", "name")
              .intoF[Option, JavaBeanTarget]
              .enableBeanSetters
              .transform ==> Some(expected)
          }
        }

        test("when F = Either[List[String], +*]") {
          CaseClassWithFlagMethod("100", "name")
            .intoF[Either[List[String], +*], JavaBeanTarget]
            .enableBeanSetters
            .enableMethodAccessors
            .transform ==> Right(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableMethodAccessors

            CaseClassWithFlagMethod("100", "name")
              .intoF[Either[List[String], +*], JavaBeanTarget]
              .enableBeanSetters
              .transform ==> Right(expected)
          }
        }
      }
    }

    test("""flag .enableMethodAccessors""") {

      test("should disable globally enabled .MethodAccessors") {

        test("when F = Option") {
          implicit val config = TransformerConfiguration.default.enableMethodAccessors

          compileError("""
            CaseClassWithFlagMethod("100", "name")
              .intoF[Option, JavaBeanTarget]
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

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type
          implicit val config = TransformerConfiguration.default.enableMethodAccessors

          compileError("""
            CaseClassWithFlagMethod("100", "name")
              .intoF[EitherList, JavaBeanTarget]
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
    }

    test("""flags .enableBeanGetters and .enableBeanSetters together""") {

      test("should transform Java Bean to Java Bean") {

        val source = new JavaBeanSourceWithFlag("200", "name", flag = false)

        val expected = new JavaBeanTarget
        expected.setId("200")
        expected.setName("name")
        expected.setFlag(false)

        test("when F = Option") {

          // need to enable both setters and getters; only one of them is not enough for this use case!
          compileError("source.intoF[Option, JavaBeanTarget].transform")
          compileError("source.intoF[Option, JavaBeanTarget].enableBeanGetters.transform")
          compileError("source.intoF[Option, JavaBeanTarget].enableBeanSetters.transform")

          source
            .intoF[Option, JavaBeanTarget]
            .enableBeanGetters
            .enableBeanSetters
            .transform ==> Some(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

            source.intoF[Option, JavaBeanTarget].transform ==> Some(expected)
          }
        }

        test("when F = Either[List[String], +*]") {
          type EitherList[+A] = Either[List[String], A] // String parsing macro cannot accept +* as type

          // need to enable both setters and getters; only one of them is not enough for this use case!
          compileError("source.intoF[EitherList, JavaBeanTarget].transform")
          compileError("source.intoF[EitherList, JavaBeanTarget].enableBeanGetters.transform")
          compileError("source.intoF[EitherList, JavaBeanTarget].enableBeanSetters.transform")

          source
            .intoF[Either[List[String], +*], JavaBeanTarget]
            .enableBeanGetters
            .enableBeanSetters
            .transform ==> Right(expected)

          locally {
            implicit val config = TransformerConfiguration.default.enableBeanGetters.enableBeanSetters

            source.intoF[Either[List[String], +*], JavaBeanTarget].transform ==> Right(expected)
          }
        }
      }
    }
  }
}
