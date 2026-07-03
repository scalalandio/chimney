package io.scalaland.chimney.protobufs.internal.compiletime

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.std.{ProviderResult, StandardMacroExtension, StdExtensions}

/** Hearth `StandardMacroExtension` (registered via `META-INF/services/hearth.std.StandardMacroExtension`, shipped in
  * the published chimney-protobufs jar) providing std-extension support for the Protobuf well-known types. Since
  * Chimney's engine consults Hearth `IsValueType`/`IsCollection` providers as its built-in fallback layer, having
  * chimney-protobufs on the compile classpath makes these conversions derivable WITHOUT any import (the implicits they
  * replaced needed `import io.scalaland.chimney.protobufs.*`); user/integrations implicits keep overriding them:
  *
  *   - `IsCollection` for `com.google.protobuf.ByteString` (a collection of `Byte`),
  *   - `IsValueType` for the `com.google.protobuf.wrappers.*Value` wrappers (`BoolValue` <-> `Boolean`, ...,
  *     `BytesValue` <-> `ByteString` - the latter composes TRANSITIVELY with the `ByteString` collection support, so
  *     e.g. `Seq[Byte]` -> `BytesValue` derives through TypeToValueClass + IterableToIterable),
  *   - `IsValueType` for `com.google.protobuf.timestamp.Timestamp` <-> `java.time.Instant` (the "inner type" is a
  *     COMPUTED conversion, not a field - Hearth's `IsValueType` contract only requires `unwrap` to be an Expr
  *     function).
  *
  * What deliberately STAYS as implicits in `io.scalaland.chimney.protobufs` (see the ScalaDocs there):
  * `com.google.protobuf.duration.Duration` (three conversion partners, but `IsValueType` allows exactly ONE inner type
  * per type - see `ProtobufsTransformerImplicits`), `Empty` <-> anything, the empty-oneof/`UnrecognizedEnum` partial
  * instances, and `DefaultValue[UnknownFieldSet]` - none of these are expressible through std-extension providers.
  *
  * Implementation notes (Kindlings `hearth-value-types`/`hearth-collection-map` skill patterns):
  *   - all matched types are MONOMORPHIC, so `parse` uses plain `tpe =:= Type.of[X]` checks (no `Ctor1.fromUntyped`
  *     machinery needed) and no existential imports appear anywhere,
  *   - every companion/static call inside a quote is routed FULLY QUALIFIED through the companion-less
  *     [[io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions]] helper object (Scala 2 resolves a
  *     companion-object qualifier from a separately compiled module to the CLASS at the downstream expansion site),
  *   - quotes are prepared OUTSIDE the anonymous `IsCollectionOf` class (its implicit `CtorResult` member would be
  *     ambiguous with the local implicit `Type` when cross-quotes resolve implicits),
  *   - this file compiles with the module's stripped-down `scalacOptions` (no `-Xsource:3`, no kind-projector), hence
  *     `with`-intersections, `_`-imports and local type aliases instead of `*`-lambdas.
  */
final class ProtobufsMacroExtension extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons with StdExtensions): Unit = {
    import ctx._

    // --- com.google.protobuf.ByteString as a collection of Byte ---

    IsCollection.registerProvider(new IsCollection.Provider {

      override def name: String = s"${loader.getClass.getName}#ByteString"

      private lazy val ByteStringType: Type[com.google.protobuf.ByteString] =
        Type.of[com.google.protobuf.ByteString]

      private def byteStringSupport: IsCollection[com.google.protobuf.ByteString] = {
        implicit val ByteStringT: Type[com.google.protobuf.ByteString] = ByteStringType
        implicit val ByteT: Type[Byte] = Type.of[Byte]
        implicit val IterableByteT: Type[Iterable[Byte]] = Type.of[Iterable[Byte]]
        implicit val FactoryT: Type[scala.collection.Factory[Byte, com.google.protobuf.ByteString]] =
          Type.of[scala.collection.Factory[Byte, com.google.protobuf.ByteString]]
        implicit val BuilderT: Type[scala.collection.mutable.Builder[Byte, com.google.protobuf.ByteString]] =
          Type.of[scala.collection.mutable.Builder[Byte, com.google.protobuf.ByteString]]
        // All quotes are prepared OUTSIDE the anonymous class - see the extension's ScalaDoc.
        val asIterableFn: Expr[com.google.protobuf.ByteString] => Expr[Iterable[Byte]] =
          value =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .byteStringAsIterable(Expr.splice(value))
            )
        val factoryExpr: Expr[scala.collection.Factory[Byte, com.google.protobuf.ByteString]] =
          Expr.quote(io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.byteStringFactory)
        val buildCtor: CtorLikeOf[
          scala.collection.mutable.Builder[Byte, com.google.protobuf.ByteString],
          com.google.protobuf.ByteString
        ] = CtorLikeOf.PlainValue(
          (builder: Expr[scala.collection.mutable.Builder[Byte, com.google.protobuf.ByteString]]) =>
            Expr.quote(Expr.splice(builder).result()),
          None
        )
        type IsCollectionOfByteString[Item] = IsCollectionOf[com.google.protobuf.ByteString, Item]
        Existential[IsCollectionOfByteString, Byte](
          new IsCollectionOf[com.google.protobuf.ByteString, Byte] {
            override def asIterable(value: Expr[com.google.protobuf.ByteString]): Expr[Iterable[Byte]] =
              asIterableFn(value)
            override type CtorResult = com.google.protobuf.ByteString
            implicit override val CtorResult: Type[CtorResult] = ByteStringT
            override def factory: Expr[scala.collection.Factory[Byte, CtorResult]] = factoryExpr
            override def build: CtorLikeOf[
              scala.collection.mutable.Builder[Byte, CtorResult],
              com.google.protobuf.ByteString
            ] = buildCtor
          }
        )
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
        if (tpe =:= ByteStringType) ProviderResult.Matched(byteStringSupport.asInstanceOf[IsCollection[A]])
        else skipped(s"${tpe.prettyPrint} is not com.google.protobuf.ByteString")
    })

    // --- com.google.protobuf.wrappers.*Value + com.google.protobuf.timestamp.Timestamp as value types ---

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = s"${loader.getClass.getName}#WellKnownValueTypes"

      // Hoisted like all cross-quotes expansions (evaluated lazily, once per macro context).
      private lazy val BoolValueType = Type.of[com.google.protobuf.wrappers.BoolValue]
      private lazy val BytesValueType = Type.of[com.google.protobuf.wrappers.BytesValue]
      private lazy val DoubleValueType = Type.of[com.google.protobuf.wrappers.DoubleValue]
      private lazy val FloatValueType = Type.of[com.google.protobuf.wrappers.FloatValue]
      private lazy val Int32ValueType = Type.of[com.google.protobuf.wrappers.Int32Value]
      private lazy val Int64ValueType = Type.of[com.google.protobuf.wrappers.Int64Value]
      private lazy val UInt32ValueType = Type.of[com.google.protobuf.wrappers.UInt32Value]
      private lazy val UInt64ValueType = Type.of[com.google.protobuf.wrappers.UInt64Value]
      private lazy val StringValueType = Type.of[com.google.protobuf.wrappers.StringValue]
      private lazy val TimestampType = Type.of[com.google.protobuf.timestamp.Timestamp]

      /** Builds the `IsValueType` proof from per-type unwrap/wrap quotes (all total, `CtorLikeOf.PlainValue`).
        * `method = None`, so chimney's fallback derives the `fieldName` default `"value"` (which happens to be the
        * actual field name for every `*Value` wrapper).
        */
      private def plainWrapperSupport[Outer, Inner](
          outerType: Type[Outer],
          innerType: Type[Inner]
      )(
          unwrapFn: Expr[Outer] => Expr[Inner],
          wrapFn: Expr[Inner] => Expr[Outer]
      ): IsValueType[Outer] = {
        implicit val OuterT: Type[Outer] = outerType
        implicit val InnerT: Type[Inner] = innerType
        type IsValueTypeOfOuter[I] = IsValueTypeOf[Outer, I]
        type CtorLikeOfOuter[I] = CtorLikeOf[I, Outer]
        val plainValue = CtorLikeOf.PlainValue[Inner, Outer](ctor = wrapFn, method = None)
        Existential[IsValueTypeOfOuter, Inner](
          new IsValueTypeOf[Outer, Inner] {
            override val unwrap: Expr[Outer] => Expr[Inner] = unwrapFn
            override val wrap: CtorLikeOf[Inner, Outer] = plainValue
            override lazy val ctors: CtorLikes[Outer] =
              NonEmptyList.one(Existential[CtorLikeOfOuter, Inner](plainValue))
          }
        )
      }

      private def boolValueSupport: IsValueType[com.google.protobuf.wrappers.BoolValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.BoolValue] = BoolValueType
        implicit val InnerT: Type[Boolean] = Type.of[Boolean]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapBoolValue(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapBoolValue(Expr.splice(inner))
            )
        )
      }

      private def bytesValueSupport: IsValueType[com.google.protobuf.wrappers.BytesValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.BytesValue] = BytesValueType
        implicit val InnerT: Type[com.google.protobuf.ByteString] = Type.of[com.google.protobuf.ByteString]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapBytesValue(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapBytesValue(Expr.splice(inner))
            )
        )
      }

      private def doubleValueSupport: IsValueType[com.google.protobuf.wrappers.DoubleValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.DoubleValue] = DoubleValueType
        implicit val InnerT: Type[Double] = Type.of[Double]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapDoubleValue(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapDoubleValue(Expr.splice(inner))
            )
        )
      }

      private def floatValueSupport: IsValueType[com.google.protobuf.wrappers.FloatValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.FloatValue] = FloatValueType
        implicit val InnerT: Type[Float] = Type.of[Float]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapFloatValue(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapFloatValue(Expr.splice(inner))
            )
        )
      }

      private def int32ValueSupport: IsValueType[com.google.protobuf.wrappers.Int32Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.Int32Value] = Int32ValueType
        implicit val InnerT: Type[Int] = Type.of[Int]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapInt32Value(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapInt32Value(Expr.splice(inner))
            )
        )
      }

      private def int64ValueSupport: IsValueType[com.google.protobuf.wrappers.Int64Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.Int64Value] = Int64ValueType
        implicit val InnerT: Type[Long] = Type.of[Long]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapInt64Value(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapInt64Value(Expr.splice(inner))
            )
        )
      }

      private def uint32ValueSupport: IsValueType[com.google.protobuf.wrappers.UInt32Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.UInt32Value] = UInt32ValueType
        implicit val InnerT: Type[Int] = Type.of[Int]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapUInt32Value(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapUInt32Value(Expr.splice(inner))
            )
        )
      }

      private def uint64ValueSupport: IsValueType[com.google.protobuf.wrappers.UInt64Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.UInt64Value] = UInt64ValueType
        implicit val InnerT: Type[Long] = Type.of[Long]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapUInt64Value(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapUInt64Value(Expr.splice(inner))
            )
        )
      }

      private def stringValueSupport: IsValueType[com.google.protobuf.wrappers.StringValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.StringValue] = StringValueType
        implicit val InnerT: Type[String] = Type.of[String]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .unwrapStringValue(Expr.splice(wrapper))
            ),
          wrapFn = inner =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions.wrapStringValue(Expr.splice(inner))
            )
        )
      }

      private def timestampSupport: IsValueType[com.google.protobuf.timestamp.Timestamp] = {
        implicit val OuterT: Type[com.google.protobuf.timestamp.Timestamp] = TimestampType
        implicit val InnerT: Type[java.time.Instant] = Type.of[java.time.Instant]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = timestamp =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .timestampToInstant(Expr.splice(timestamp))
            ),
          wrapFn = instant =>
            Expr.quote(
              io.scalaland.chimney.protobufs.internal.runtime.ProtobufsConversions
                .instantToTimestamp(Expr.splice(instant))
            )
        )
      }

      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        if (tpe =:= BoolValueType) ProviderResult.Matched(boolValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= BytesValueType) ProviderResult.Matched(bytesValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= DoubleValueType) ProviderResult.Matched(doubleValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= FloatValueType) ProviderResult.Matched(floatValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= Int32ValueType) ProviderResult.Matched(int32ValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= Int64ValueType) ProviderResult.Matched(int64ValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= UInt32ValueType) ProviderResult.Matched(uint32ValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= UInt64ValueType) ProviderResult.Matched(uint64ValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= StringValueType) ProviderResult.Matched(stringValueSupport.asInstanceOf[IsValueType[A]])
        else if (tpe =:= TimestampType) ProviderResult.Matched(timestampSupport.asInstanceOf[IsValueType[A]])
        else skipped(s"${tpe.prettyPrint} is not a supported Protobuf well-known value type")
    })
  }
}
