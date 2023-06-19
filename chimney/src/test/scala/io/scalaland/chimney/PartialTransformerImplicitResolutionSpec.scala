package io.scalaland.chimney

//import io.scalaland.chimney.dsl.*
//import io.scalaland.chimney.fixtures.*

class PartialTransformerImplicitResolutionSpec extends ChimneySpec {

//  test("transform using implicit Total Transformer for whole transformation when available") {
//    import products.Domain1.*
//    implicit def instance: Transformer[UserName, String] = userNameToStringTransformer
//
//    val result = UserName("Batman").intoPartial[String].transform
//    result.asOption ==> Some("BatmanT")
//    result.asEither ==> Right("BatmanT")
//    result.asErrorPathMessageStrings ==> Iterable.empty
//
//    val result2 = UserName("Batman").transformIntoPartial[String]
//    result2.asOption ==> Some("BatmanT")
//    result2.asEither ==> Right("BatmanT")
//    result2.asErrorPathMessageStrings ==> Iterable.empty
//  }
//
//  test("transform using implicit Partial Transformer for whole transformation when available") {
//    import products.Domain1.*
//    implicit def instance: PartialTransformer[UserName, String] = userNameToStringPartialTransformer
//
//    val result = UserName("Batman").intoPartial[String].transform
//    result.asOption ==> Some("BatmanT")
//    result.asEither ==> Right("BatmanT")
//    result.asErrorPathMessageStrings ==> Iterable.empty
//
//    val result2 = UserName("Batman").transformIntoPartial[String]
//    result2.asOption ==> Some("BatmanT")
//    result2.asEither ==> Right("BatmanT")
//    result2.asErrorPathMessageStrings ==> Iterable.empty
//  }
//
//  test("transform using implicit Total Transformer for nested field when available") {
//    import products.Domain1.*
//    implicit def instance: Transformer[UserName, String] = userNameToStringTransformer
//
//    val expected = UserDTO("123", "BatmanT")
//
//    val result = User("123", UserName("Batman")).intoPartial[UserDTO].transform
//    result.asOption ==> Some(expected)
//    result.asEither ==> Right(expected)
//    result.asErrorPathMessageStrings ==> Iterable.empty
//
//    val result2 = User("123", UserName("Batman")).transformIntoPartial[UserDTO]
//    result2.asOption ==> Some(expected)
//    result2.asEither ==> Right(expected)
//    result2.asErrorPathMessageStrings ==> Iterable.empty
//  }
//
//  test("transform using implicit Partial Transformer for nested field when available") {
//    import products.Domain1.*
//    implicit def instance: PartialTransformer[UserName, String] = userNameToStringPartialTransformer
//
//    val expected = UserDTO("123", "BatmanT")
//
//    val result = User("123", UserName("Batman")).intoPartial[UserDTO].transform
//    result.asOption ==> Some(expected)
//    result.asEither ==> Right(expected)
//    result.asErrorPathMessageStrings ==> Iterable.empty
//
//    val result2 = User("123", UserName("Batman")).transformIntoPartial[UserDTO]
//    result2.asOption ==> Some(expected)
//    result2.asEither ==> Right(expected)
//    result2.asErrorPathMessageStrings ==> Iterable.empty
//  }
}
