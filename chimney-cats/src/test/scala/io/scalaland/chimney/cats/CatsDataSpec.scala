package io.scalaland.chimney.cats

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySeq, NonEmptySet, NonEmptyVector}
import io.scalaland.chimney.ChimneySpec
import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.*

class CatsDataSpec extends ChimneySpec {

  test("DSL should always allow transformation when outer type has Traverse") {
    implicit val intToStr: Transformer[Int, String] = _.toString

    NonEmptyChain.one(1).transformInto[NonEmptyChain[String]] ==> NonEmptyChain.one("1")
    NonEmptyList.one(1).transformInto[NonEmptyList[String]] ==> NonEmptyList.one("1")
    NonEmptyMap.one(1, 1).transformInto[NonEmptyMap[String, String]] ==> NonEmptyMap.one("1", "1")
    NonEmptySeq.one(1).transformInto[NonEmptySeq[String]] ==> NonEmptySeq.one("1")
    NonEmptyVector.one(1).transformInto[NonEmptyVector[String]] ==> NonEmptyVector.one("1")
  }

  test("DSL should always allow transformation between NonEmptyMaps") {
    implicit val intToStr: Transformer[Int, String] = _.toString

    NonEmptyMap.one(1, 1).transformInto[NonEmptyMap[String, String]] ==> NonEmptyMap.one("1", "1")
  }

  test("DSL should always allow transformation between NonEmptySets") {
    implicit val intToStr: Transformer[Int, String] = _.toString

    NonEmptySet.one(1).transformInto[NonEmptySet[String]] ==> NonEmptySet.one("1")
  }

  test("DSL should handle transformation to and from cats.data.Chain") {
    List("test").transformInto[Chain[String]] ==> Chain("test")

    Chain("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyChain") {
    List("test").transformIntoPartial[NonEmptyChain[String]].asOption ==> Some(NonEmptyChain.one("test"))
    List.empty[String].transformIntoPartial[NonEmptyChain[String]].asOption ==> None

    NonEmptyChain.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyList") {
    List("test").transformIntoPartial[NonEmptyList[String]].asOption ==> Some(NonEmptyList.one("test"))
    List.empty[String].transformIntoPartial[NonEmptyList[String]].asOption ==> None

    NonEmptyList.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyMap") {
    List("test" -> "test").transformIntoPartial[NonEmptyMap[String, String]].asOption ==> Some(
      NonEmptyMap.one("test", "test")
    )
    List.empty[(String, String)].transformIntoPartial[NonEmptyMap[String, String]].asOption ==> None

    NonEmptyMap.one("test", "test").transformInto[List[(String, String)]] ==> List("test" -> "test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptySeq") {
    List("test").transformIntoPartial[NonEmptySeq[String]].asOption ==> Some(NonEmptySeq.one("test"))
    List.empty[String].transformIntoPartial[NonEmptySeq[String]].asOption ==> None

    NonEmptySeq.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptySet") {
    List("test").transformIntoPartial[NonEmptySet[String]].asOption ==> Some(NonEmptySet.one("test"))
    List.empty[String].transformIntoPartial[NonEmptySet[String]].asOption ==> None

    NonEmptySet.one("test").transformInto[List[String]] ==> List("test")
  }

  test("DSL should handle transformation to and from cats.data.NonEmptyVector") {
    List("test").transformIntoPartial[NonEmptyVector[String]].asOption ==> Some(NonEmptyVector.one("test"))
    List.empty[String].transformIntoPartial[NonEmptyVector[String]].asOption ==> None

    NonEmptyVector.one("test").transformInto[List[String]] ==> List("test")
  }
}
