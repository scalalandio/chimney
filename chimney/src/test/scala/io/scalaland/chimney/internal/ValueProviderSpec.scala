//package io.scalaland.chimney.internal
//
//import org.scalatest.{MustMatchers, WordSpec}
//import shapeless.{HList, HNil, LabelledGeneric, Witness}
//
//class ValueProviderSpec extends WordSpec with MustMatchers {
//
//  "ValueProvider" should {
//
//    case class Source(foo: String)
//
//    "provide value for field with no modifiers" in {
//
//      provide(Source("test"), 'foo, classOf[String]) mustBe "test"
//    }
//
//    "provide value for field with field constant modifier" in {
//
//      provide(Source("test"), 'foo, classOf[String], Modifier.fieldConstant[Source, String]('foo, "provided") :: HNil) mustBe
//        "provided"
//    }
//
//    "provide value for field with field function modifier" in {
//
//      provide(Source("test"), 'foo, classOf[String], Modifier.fieldFunction[Source, String]('foo, _.foo * 2) :: HNil) mustBe
//        "testtest"
//    }
//
//    "provide value for field with relabelling modifier" in {
//
//      provide(Source("test"), 'bar, classOf[String], Modifier.relabel('foo, 'bar) :: HNil) mustBe
//        "test"
//    }
//
//    "pick applicable modifier" when {
//
//      "applicable is first" in {
//        provide(
//          Source("test"),
//          'foo,
//          classOf[String],
//          Modifier.fieldConstant[Source, String]('foo, "provided") ::
//            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
//            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
//            HNil
//        ) mustBe
//          "provided"
//      }
//
//      "applicable is in the middle" in {
//        provide(
//          Source("test"),
//          'foo,
//          classOf[String],
//          Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
//            Modifier.fieldConstant[Source, String]('foo, "provided") ::
//            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
//            HNil
//        ) mustBe
//          "provided"
//      }
//
//      "applicable is last" in {
//        provide(
//          Source("test"),
//          'foo,
//          classOf[String],
//          Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
//            Modifier.fieldConstant[Source, String]('na, "non-applicable") ::
//            Modifier.fieldConstant[Source, String]('foo, "provided") ::
//            HNil
//        ) mustBe
//          "provided"
//      }
//    }
//
//    "pick first of applicable modifiers" in {
//
//      provide(
//        Source("test"),
//        'foo,
//        classOf[String],
//        Modifier.fieldConstant[Source, String]('foo, "provided1") ::
//          Modifier.fieldConstant[Source, String]('foo, "provided2") ::
//          Modifier.fieldConstant[Source, String]('foo, "provided3") ::
//          HNil
//      ) mustBe
//        "provided1"
//    }
//
//    "lub the value into the target type" in {
//      provide(
//        Source("test"),
//        'foo,
//        classOf[Option[Int]],
//        Modifier.fieldConstant[Source, Option[Int]]('foo, None) ::
//          HNil
//      ) mustBe
//        None
//    }
//
//    "provide value for coproduct field" in {
//
//      provide(Response(No), 'answer, classOf[Answer]) mustBe
//        No
//
//      provide(Response(Yes), 'answer, classOf[Answer]) mustBe
//        Yes
//
//      provide(Response2("no", No), 'answer, classOf[Answer]) mustBe
//        No
//
//      provide(Response2("si", Yes), 'answer, classOf[Answer]) mustBe
//        Yes
//
//      provide(Response2("no", No), 'other, classOf[String]) mustBe
//        "no"
//
//      provide(Response2("si", Yes), 'other, classOf[String]) mustBe
//        "si"
//    }
//  }
//
//  final def provide[From, FromLG <: HList, TargetT, Modifiers <: HList](from: From,
//                                                                        targetLabel: Witness.Lt[Symbol],
//                                                                        clz: Class[TargetT],
//                                                                        modifiers: Modifiers)(
//    implicit lg: LabelledGeneric.Aux[From, FromLG],
//    vp: ValueProvider[From, FromLG, TargetT, targetLabel.T, Modifiers]
//  ): TargetT = vp.provide(lg.to(from), modifiers)
//
//  final def provide[From, FromLG <: HList, TargetT](from: From, targetLabel: Witness.Lt[Symbol], clz: Class[TargetT])(
//    implicit lg: LabelledGeneric.Aux[From, FromLG],
//    vp: ValueProvider[From, FromLG, TargetT, targetLabel.T, HNil]
//  ): TargetT = vp.provide(lg.to(from), HNil)
//
//}
//
//sealed trait Answer
//case object Yes extends Answer
//case object No extends Answer
//case class Response(answer: Answer)
//case class Response2(other: String, answer: Answer)
