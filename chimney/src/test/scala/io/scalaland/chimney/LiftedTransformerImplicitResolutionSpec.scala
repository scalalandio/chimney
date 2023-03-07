package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.examples.*
import utest.*

object LiftedTransformerImplicitResolutionSpec {

  val tests = Tests {

    test("transform using implicit Total Transformer for whole transformation when available") {
      import products.Domain1.*

      test("when F = Option") {
        implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

        UserName("Batman").intoF[Option, String].transform ==> Some("BatmanT")
        UserName("Batman").transformIntoF[Option, String] ==> Some("BatmanT")
      }

      test("when F = Either[List[String], +*]") {
        implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

        UserName("Batman").intoF[Either[List[String], +*], String].transform ==> Right("BatmanT")
        UserName("Batman").transformIntoF[Either[List[String], +*], String] ==> Right("BatmanT")
      }
    }

    test("transform using implicit Lifted Transformer for whole transformation when available") {
      import products.Domain1.*

      test("when F = Option") {
        implicit def instance: TransformerF[Option, UserName, String] = userNameToStringLiftedTransformer(Option(_))

        UserName("Batman").intoF[Option, String].transform ==> Some("BatmanT")
        UserName("Batman").transformIntoF[Option, String] ==> Some("BatmanT")
      }

      test("when F = Either[List[String], +*]") {
        implicit def instance: TransformerF[Either[List[String], +*], UserName, String] =
          userNameToStringLiftedTransformer(Right(_))

        UserName("Batman").intoF[Either[List[String], +*], String].transform ==> Right("BatmanT")
        UserName("Batman").transformIntoF[Either[List[String], +*], String] ==> Right("BatmanT")
      }
    }

    test("transform using implicit Total Transformer for nested field when available") {
      import products.Domain1.*

      implicit def instance: Transformer[UserName, String] = userNameToStringTransformer

      test("when F = Option") {
        User("123", UserName("Batman")).intoF[Option, UserDTO].transform ==> Some(UserDTO("123", "BatmanT"))
        User("123", UserName("Batman")).transformIntoF[Option, UserDTO] ==> Some(UserDTO("123", "BatmanT"))
      }

      test("when F = Either[List[String], +*]") {
        User("123", UserName("Batman")).intoF[Either[List[String], +*], UserDTO].transform ==> Right(
          UserDTO("123", "BatmanT")
        )
        User("123", UserName("Batman")).transformIntoF[Either[List[String], +*], UserDTO] ==> Right(
          UserDTO("123", "BatmanT")
        )
      }
    }

    test("transform using implicit Lifted Transformer for nested field when available") {
      import products.Domain1.*

      test("when F = Option") {
        implicit def instance: TransformerF[Option, UserName, String] = userNameToStringLiftedTransformer(Option(_))

        User("123", UserName("Batman")).intoF[Option, UserDTO].transform ==> Some(UserDTO("123", "BatmanT"))
        User("123", UserName("Batman")).transformIntoF[Option, UserDTO] ==> Some(UserDTO("123", "BatmanT"))
      }

      test("when F = Either[List[String], +*]") {
        implicit def instance: TransformerF[Either[List[String], +*], UserName, String] =
          userNameToStringLiftedTransformer(Right(_))

        User("123", UserName("Batman")).intoF[Either[List[String], +*], UserDTO].transform ==> Right(
          UserDTO("123", "BatmanT")
        )
        User("123", UserName("Batman")).transformIntoF[Either[List[String], +*], UserDTO] ==> Right(
          UserDTO("123", "BatmanT")
        )
      }
    }
  }
}
