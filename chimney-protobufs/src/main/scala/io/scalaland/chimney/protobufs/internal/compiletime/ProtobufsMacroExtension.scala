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
  * Implementation notes:
  *   - the quotes are self-contained: they reference only scalapb/protobuf/std-library API (FULLY QUALIFIED - the
  *     expansion site's imports are unknown), never anything from this jar, so chimney-protobufs is a compile-only
  *     dependency as far as the providers are concerned,
  *   - quotes are prepared OUTSIDE the anonymous `IsCollectionOf` class (its implicit `CtorResult` member would be
  *     ambiguous with the local implicit `Type` when cross-quotes resolve implicits),
  *   - this file compiles with the module's stripped-down `scalacOptions` (no `-Xsource:3`, no kind-projector), hence
  *     `with`-intersections, `_`-imports and local type aliases instead of `*`-lambdas.
  */
final class ProtobufsMacroExtension extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons with StdExtensions): Unit = {
    // scalafmt would rewrite `._` to `.*`, but this module compiles without -Xsource:3 (see build.sbt)
    // format: off
    import ctx._
    // format: on

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
              // toByteArray() returns a fresh array, safe to wrap
              scala.collection.immutable.ArraySeq.unsafeWrapArray(
                Expr.splice(value).toByteArray()
              ): Iterable[Byte]
            )
        val factoryExpr: Expr[scala.collection.Factory[Byte, com.google.protobuf.ByteString]] =
          Expr.quote {
            new scala.collection.Factory[Byte, com.google.protobuf.ByteString] {
              def fromSpecific(it: scala.collection.IterableOnce[Byte]): com.google.protobuf.ByteString =
                newBuilder.addAll(it).result()
              // ArrayBuilder.ofByte, NOT Array.newBuilder[Byte]: the latter needs an implicit ClassTag, whose
              // inserted reference does not survive the downstream re-typecheck on Scala 2
              def newBuilder: scala.collection.mutable.Builder[Byte, com.google.protobuf.ByteString] =
                (new scala.collection.mutable.ArrayBuilder.ofByte)
                  .mapResult(array => com.google.protobuf.ByteString.copyFrom(array))
            }
          }
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
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.BoolValue.of(Expr.splice(inner)))
        )
      }

      private def bytesValueSupport: IsValueType[com.google.protobuf.wrappers.BytesValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.BytesValue] = BytesValueType
        implicit val InnerT: Type[com.google.protobuf.ByteString] = Type.of[com.google.protobuf.ByteString]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.BytesValue.of(Expr.splice(inner)))
        )
      }

      private def doubleValueSupport: IsValueType[com.google.protobuf.wrappers.DoubleValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.DoubleValue] = DoubleValueType
        implicit val InnerT: Type[Double] = Type.of[Double]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.DoubleValue.of(Expr.splice(inner)))
        )
      }

      private def floatValueSupport: IsValueType[com.google.protobuf.wrappers.FloatValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.FloatValue] = FloatValueType
        implicit val InnerT: Type[Float] = Type.of[Float]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.FloatValue.of(Expr.splice(inner)))
        )
      }

      private def int32ValueSupport: IsValueType[com.google.protobuf.wrappers.Int32Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.Int32Value] = Int32ValueType
        implicit val InnerT: Type[Int] = Type.of[Int]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.Int32Value.of(Expr.splice(inner)))
        )
      }

      private def int64ValueSupport: IsValueType[com.google.protobuf.wrappers.Int64Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.Int64Value] = Int64ValueType
        implicit val InnerT: Type[Long] = Type.of[Long]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.Int64Value.of(Expr.splice(inner)))
        )
      }

      private def uint32ValueSupport: IsValueType[com.google.protobuf.wrappers.UInt32Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.UInt32Value] = UInt32ValueType
        implicit val InnerT: Type[Int] = Type.of[Int]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.UInt32Value.of(Expr.splice(inner)))
        )
      }

      private def uint64ValueSupport: IsValueType[com.google.protobuf.wrappers.UInt64Value] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.UInt64Value] = UInt64ValueType
        implicit val InnerT: Type[Long] = Type.of[Long]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.UInt64Value.of(Expr.splice(inner)))
        )
      }

      private def stringValueSupport: IsValueType[com.google.protobuf.wrappers.StringValue] = {
        implicit val OuterT: Type[com.google.protobuf.wrappers.StringValue] = StringValueType
        implicit val InnerT: Type[String] = Type.of[String]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = wrapper => Expr.quote(Expr.splice(wrapper).value),
          wrapFn = inner => Expr.quote(com.google.protobuf.wrappers.StringValue.of(Expr.splice(inner)))
        )
      }

      private def timestampSupport: IsValueType[com.google.protobuf.timestamp.Timestamp] = {
        implicit val OuterT: Type[com.google.protobuf.timestamp.Timestamp] = TimestampType
        implicit val InnerT: Type[java.time.Instant] = Type.of[java.time.Instant]
        plainWrapperSupport(OuterT, InnerT)(
          unwrapFn = timestamp =>
            Expr.quote {
              val ts = Expr.splice(timestamp)
              java.time.Instant.ofEpochSecond(ts.seconds, ts.nanos.toLong)
            },
          wrapFn = instant =>
            Expr.quote {
              val i = Expr.splice(instant)
              com.google.protobuf.timestamp.Timestamp.of(i.getEpochSecond, i.getNano)
            }
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
