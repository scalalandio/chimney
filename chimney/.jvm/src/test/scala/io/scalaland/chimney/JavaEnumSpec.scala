package io.scalaland.chimney

import utest._
import io.scalaland.chimney.examples.JavaEnums
import io.scalaland.chimney.examples.JavaEnums.Colors3
import io.scalaland.chimney.examples._

object JavaEnumSpec extends TestSuite {

  val tests = Tests {

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
          "implicit val t: Transformer[JavaEnums.Colors6, JavaEnums.Colors3] = Transformer.define.buildTransformer"
        )
      }

      "wide to narrow with custom mapping" - {
        val t = Transformer
          .define[JavaEnums.Colors6, JavaEnums.Colors3]
          .withCoproductInstance { _: JavaEnums.Colors6.Marine.type =>
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
        t.transform(JavaEnums.Colors6.Marine) ==> JavaEnums.Colors3.Red
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
    }

    "enum to sealed hierarchy" - {
      "one to one" - {
        val t = Transformer.define[Colors3, colors2.Color].buildTransformer

        t.transform(JavaEnums.Colors3.Black) ==> colors2.Black
        t.transform(JavaEnums.Colors3.Blue) ==> colors2.Blue
        t.transform(JavaEnums.Colors3.Green) ==> colors2.Green
        t.transform(JavaEnums.Colors3.Red) ==> colors2.Red
      }

      "wide to narrow" - {
        compileError(
          "implicit val t: Transformer[JavaEnums.Colors3, colors1.Color] = Transformer.define.buildTransformer"
        )
      }

      "wide to narrow with custom mapping" - {
        val t = Transformer
          .define[JavaEnums.Colors6, colors2.Color]
          .withCoproductInstance { _: JavaEnums.Colors6.Marine.type =>
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
        t.transform(JavaEnums.Colors6.Marine) ==> colors2.Red
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
