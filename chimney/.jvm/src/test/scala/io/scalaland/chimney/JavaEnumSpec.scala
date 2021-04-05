package io.scalaland.chimney

import utest._
import io.scalaland.chimney.examples.JavaEnums
import io.scalaland.chimney.examples._

object JavaEnumSpec extends TestSuite {

  val tests = Tests {

    "transformerF" - {
      "enum to enum" - {
        "narrow to wide" - {
          val t: TransformerF[Option, JavaEnums.Colors3, JavaEnums.Colors6] = TransformerF
            .define[Option, JavaEnums.Colors3, JavaEnums.Colors6]
            .buildTransformer

          assert(t.transform(JavaEnums.Colors3.Black).contains(JavaEnums.Colors6.Black))
          assert(t.transform(JavaEnums.Colors3.Red).contains(JavaEnums.Colors6.Red))
          assert(t.transform(JavaEnums.Colors3.Green).contains(JavaEnums.Colors6.Green))
          assert(t.transform(JavaEnums.Colors3.Blue).contains(JavaEnums.Colors6.Blue))
        }

        "wide to narrow auto-derivation fails" - {
          compileError(
            "implicit val t: TransformerF[Option, JavaEnums.Colors6, JavaEnums.Colors3] = TransformerF.derive[Option, JavaEnums.Colors6, JavaEnums.Colors3]"
          ).check(
            "",
            "Chimney can't derive transformation",
            "JavaEnums.Colors6.Yellow",
            "JavaEnums.Colors6.Teal",
            "JavaEnums.Colors6.Magenta"
          )
        }

        "wide to narrow with custom mapping" - {
          val t: TransformerF[Option, JavaEnums.Colors6, JavaEnums.Colors3] = TransformerF
            .define[Option, JavaEnums.Colors6, JavaEnums.Colors3]
            .withCoproductInstanceF { _: JavaEnums.Colors6.Magenta.type =>
              None
            }
            .withCoproductInstanceF { _: JavaEnums.Colors6.Yellow.type =>
              None
            }
            .withCoproductInstance { _: JavaEnums.Colors6.Teal.type =>
              JavaEnums.Colors3.Blue
            }
            .buildTransformer

          assert(t.transform(JavaEnums.Colors6.Black).contains(JavaEnums.Colors3.Black))
          assert(t.transform(JavaEnums.Colors6.Red).contains(JavaEnums.Colors3.Red))
          assert(t.transform(JavaEnums.Colors6.Green).contains(JavaEnums.Colors3.Green))
          assert(t.transform(JavaEnums.Colors6.Blue).contains(JavaEnums.Colors3.Blue))
          assert(t.transform(JavaEnums.Colors6.Teal).contains(JavaEnums.Colors3.Blue))
          assert(t.transform(JavaEnums.Colors6.Yellow).isEmpty)
          assert(t.transform(JavaEnums.Colors6.Magenta).isEmpty)
        }

        "using total function" - {
          val t = TransformerF
            .define[Option, JavaEnums.Colors3, JavaEnums.Colors6]
            .withCoproductInstance[JavaEnums.Colors3] {
              case JavaEnums.Colors3.Black => JavaEnums.Colors6.Black
              case JavaEnums.Colors3.Red   => JavaEnums.Colors6.Red
              case JavaEnums.Colors3.Green => JavaEnums.Colors6.Green
              case JavaEnums.Colors3.Blue  => JavaEnums.Colors6.Blue
            }
            .buildTransformer

          assert(t.transform(JavaEnums.Colors3.Black).contains(JavaEnums.Colors6.Black))
          assert(t.transform(JavaEnums.Colors3.Blue).contains(JavaEnums.Colors6.Blue))
          assert(t.transform(JavaEnums.Colors3.Green).contains(JavaEnums.Colors6.Green))
          assert(t.transform(JavaEnums.Colors3.Red).contains(JavaEnums.Colors6.Red))
        }

        "being nested" - {
          val t = TransformerF.derive[Option, Domain1.Drawing, Domain2.Drawing]

          assert(
            t.transform(
                Domain1.Drawing(Domain1.Background(JavaEnums.Colors3.Blue), Domain1.Foreground(JavaEnums.Colors3.Green))
              )
              .contains(
                Domain2.Drawing(Domain2.Background(JavaEnums.Colors6.Blue), Domain2.Foreground(JavaEnums.Colors6.Green))
              )
          )
        }
      }

      "enum to sealed hierarchy" - {
        "one to one" - {
          val t = TransformerF.define[Option, JavaEnums.Colors3, colors2.Color].buildTransformer

          assert(t.transform(JavaEnums.Colors3.Black).contains(colors2.Black))
          assert(t.transform(JavaEnums.Colors3.Blue).contains(colors2.Blue))
          assert(t.transform(JavaEnums.Colors3.Green).contains(colors2.Green))
          assert(t.transform(JavaEnums.Colors3.Red).contains(colors2.Red))
        }

        "wide to narrow" - {
          compileError(
            "implicit val t: TransformerF[Option, JavaEnums.Colors3, colors1.Color] = TransformerF.derive[Option, JavaEnums.Colors3, colors1.Color]"
          ).check(
            "",
            "Chimney can't derive transformation",
            "JavaEnums.Colors3.Black"
          )
        }

        "wide to narrow with custom mapping" - {
          val t = TransformerF
            .define[Option, JavaEnums.Colors6, colors2.Color]
            .withCoproductInstanceF { _: JavaEnums.Colors6.Magenta.type =>
              None
            }
            .withCoproductInstanceF { _: JavaEnums.Colors6.Yellow.type =>
              None
            }
            .withCoproductInstance { _: JavaEnums.Colors6.Teal.type =>
              colors2.Blue
            }
            .buildTransformer

          assert(t.transform(JavaEnums.Colors6.Black).contains(colors2.Black))
          assert(t.transform(JavaEnums.Colors6.Blue).contains(colors2.Blue))
          assert(t.transform(JavaEnums.Colors6.Green).contains(colors2.Green))
          assert(t.transform(JavaEnums.Colors6.Red).contains(colors2.Red))
          assert(t.transform(JavaEnums.Colors6.Teal).contains(colors2.Blue))
          assert(t.transform(JavaEnums.Colors6.Yellow).isEmpty)
          assert(t.transform(JavaEnums.Colors6.Magenta).isEmpty)
        }
      }

      "sealed hierarchy to enum" - {
        "one to one" - {
          val t =
            TransformerF.define[Option, colors2.Color, JavaEnums.Colors3].buildTransformer

          assert(t.transform(colors2.Black).contains(JavaEnums.Colors3.Black))
          assert(t.transform(colors2.Blue).contains(JavaEnums.Colors3.Blue))
          assert(t.transform(colors2.Green).contains(JavaEnums.Colors3.Green))
          assert(t.transform(colors2.Red).contains(JavaEnums.Colors3.Red))
        }
      }
    }

    "transformer" - {
      "enum to enum" - {
        "narrow to wide" - {
          val t = Transformer
            .define[JavaEnums.Colors3, JavaEnums.Colors6]
            .buildTransformer

          t.transform(JavaEnums.Colors3.Black) ==> JavaEnums.Colors6.Black
          t.transform(JavaEnums.Colors3.Blue) ==> JavaEnums.Colors6.Blue
          t.transform(JavaEnums.Colors3.Green) ==> JavaEnums.Colors6.Green
          t.transform(JavaEnums.Colors3.Red) ==> JavaEnums.Colors6.Red
        }

        "wide to narrow auto-derivation fails" - {
          compileError(
            "implicit val t: Transformer[JavaEnums.Colors6, JavaEnums.Colors3] = Transformer.derive[JavaEnums.Colors6, JavaEnums.Colors3]"
          ).check(
            "",
            "Chimney can't derive transformation",
            "JavaEnums.Colors6.Yellow",
            "JavaEnums.Colors6.Teal",
            "JavaEnums.Colors6.Magenta"
          )
        }

        "wide to narrow with custom mapping" - {
          val t = Transformer
            .define[JavaEnums.Colors6, JavaEnums.Colors3]
            .withCoproductInstance { _: JavaEnums.Colors6.Magenta.type =>
              JavaEnums.Colors3.Red
            }
            .withCoproductInstance { _: JavaEnums.Colors6.Yellow.type =>
              JavaEnums.Colors3.Green
            }
            .withCoproductInstance { _: JavaEnums.Colors6.Teal.type =>
              JavaEnums.Colors3.Blue
            }
            .buildTransformer

          t.transform(JavaEnums.Colors6.Black) ==> JavaEnums.Colors3.Black
          t.transform(JavaEnums.Colors6.Blue) ==> JavaEnums.Colors3.Blue
          t.transform(JavaEnums.Colors6.Green) ==> JavaEnums.Colors3.Green
          t.transform(JavaEnums.Colors6.Red) ==> JavaEnums.Colors3.Red
          t.transform(JavaEnums.Colors6.Teal) ==> JavaEnums.Colors3.Blue
          t.transform(JavaEnums.Colors6.Yellow) ==> JavaEnums.Colors3.Green
          t.transform(JavaEnums.Colors6.Magenta) ==> JavaEnums.Colors3.Red
        }

        "using total function" - {
          val t = Transformer
            .define[JavaEnums.Colors3, JavaEnums.Colors6]
            .withCoproductInstance[JavaEnums.Colors3] {
              case JavaEnums.Colors3.Black => JavaEnums.Colors6.Black
              case JavaEnums.Colors3.Red   => JavaEnums.Colors6.Red
              case JavaEnums.Colors3.Green => JavaEnums.Colors6.Green
              case JavaEnums.Colors3.Blue  => JavaEnums.Colors6.Blue
            }
            .buildTransformer

          t.transform(JavaEnums.Colors3.Black) ==> JavaEnums.Colors6.Black
          t.transform(JavaEnums.Colors3.Blue) ==> JavaEnums.Colors6.Blue
          t.transform(JavaEnums.Colors3.Green) ==> JavaEnums.Colors6.Green
          t.transform(JavaEnums.Colors3.Red) ==> JavaEnums.Colors6.Red
        }

        "being nested" - {
          val t = Transformer.derive[Domain1.Drawing, Domain2.Drawing]

          t.transform(
            Domain1.Drawing(Domain1.Background(JavaEnums.Colors3.Blue), Domain1.Foreground(JavaEnums.Colors3.Green))
          ) ==>
            Domain2.Drawing(Domain2.Background(JavaEnums.Colors6.Blue), Domain2.Foreground(JavaEnums.Colors6.Green))
        }
      }

      "enum to sealed hierarchy" - {
        "one to one" - {
          val t = Transformer.define[JavaEnums.Colors3, colors2.Color].buildTransformer

          t.transform(JavaEnums.Colors3.Black) ==> colors2.Black
          t.transform(JavaEnums.Colors3.Blue) ==> colors2.Blue
          t.transform(JavaEnums.Colors3.Green) ==> colors2.Green
          t.transform(JavaEnums.Colors3.Red) ==> colors2.Red
        }

        "wide to narrow" - {
          compileError(
            "implicit val t: Transformer[JavaEnums.Colors3, colors1.Color] = Transformer.derive[JavaEnums.Colors3, colors1.Color]"
          ).check(
            "",
            "Chimney can't derive transformation",
            "JavaEnums.Colors3.Black"
          )
        }

        "wide to narrow with custom mapping" - {
          val t = Transformer
            .define[JavaEnums.Colors6, colors2.Color]
            .withCoproductInstance { _: JavaEnums.Colors6.Magenta.type =>
              colors2.Red
            }
            .withCoproductInstance { _: JavaEnums.Colors6.Yellow.type =>
              colors2.Green
            }
            .withCoproductInstance { _: JavaEnums.Colors6.Teal.type =>
              colors2.Blue
            }
            .buildTransformer

          t.transform(JavaEnums.Colors6.Black) ==> colors2.Black
          t.transform(JavaEnums.Colors6.Blue) ==> colors2.Blue
          t.transform(JavaEnums.Colors6.Green) ==> colors2.Green
          t.transform(JavaEnums.Colors6.Red) ==> colors2.Red
          t.transform(JavaEnums.Colors6.Teal) ==> colors2.Blue
          t.transform(JavaEnums.Colors6.Yellow) ==> colors2.Green
          t.transform(JavaEnums.Colors6.Magenta) ==> colors2.Red
        }
      }

      "sealed hierarchy to enum" - {
        "one to one" - {
          val t =
            Transformer.define[colors2.Color, JavaEnums.Colors3].buildTransformer

          t.transform(colors2.Black) ==> JavaEnums.Colors3.Black
          t.transform(colors2.Blue) ==> JavaEnums.Colors3.Blue
          t.transform(colors2.Green) ==> JavaEnums.Colors3.Green
          t.transform(colors2.Red) ==> JavaEnums.Colors3.Red
        }
      }
    }
  }

  object Domain1 {
    case class Foreground(color: JavaEnums.Colors3)
    case class Background(color: JavaEnums.Colors3)
    case class Drawing(bg: Background, fg: Foreground)
  }

  object Domain2 {
    case class Foreground(color: JavaEnums.Colors6)
    case class Background(color: JavaEnums.Colors6)
    case class Drawing(bg: Background, fg: Foreground)
  }

  object Domain3 {
    case class Foreground(color: Option[JavaEnums.Colors6])
    case class Background(color: Option[JavaEnums.Colors6])
    case class Drawing(bg: Background, fg: Foreground)
  }
}
