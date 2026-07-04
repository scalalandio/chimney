package io.scalaland.chimney.protobufs.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.MIO
import io.scalaland.chimney.dsl.PreferPartialTransformer
import io.scalaland.chimney.integrations.ChimneyMacroExtension
import io.scalaland.chimney.internal.compiletime.derivation.transformer.ChimneyEngineExtensionApi

/** Chimney `ChimneyMacroExtension` (registered via
  * `META-INF/services/io.scalaland.chimney.integrations.ChimneyMacroExtension`, shipped in the published
  * chimney-protobufs jar) providing engine-aware, PAIR-SPECIFIC transformations for the Protobuf well-known types whose
  * shape Hearth's std-extension providers cannot express (see the sibling [[ProtobufsMacroExtension]] for the ones that
  * CAN - ByteString/wrappers.*Value/Timestamp).
  *
  * Since 2.0.0 these replaced the corresponding IMPLICITS (deleted from `ProtobufsTransformerImplicits` /
  * `ProtobufsPartialTransformerImplicits`), so the conversions work WITHOUT any import once this jar is on the
  * classpath (user/integration implicits still override them - the special-cased rule sits below the implicit rules):
  *
  *   - `com.google.protobuf.duration.Duration` <-> `java.time.Duration` / `scala.concurrent.duration.FiniteDuration`
  *     (total both ways) and <-> `scala.concurrent.duration.Duration` (total FROM proto via upcast; partial TO proto,
  *     rejecting `Duration.Infinite`). `IsValueType` allows exactly ONE inner type per outer type, so it could not
  *     express proto `Duration`'s THREE partners with the total/partial asymmetry - a handler can (it inspects
  *     `Type[From]`/`Type[To]` for each pair and the total-vs-partial context),
  *   - `com.google.protobuf.empty.Empty`: `A -> Empty` for ANY `A` (total), `Empty -> Unit` (total, or partial via
  *     `enableImplicitConflictResolution(PreferPartialTransformer)`), `Empty -> A` for any `A` (partial). `Transformer
  *     [A, Empty]` works for ANY `A`, which no `IsValueType[Empty]` (inner `Unit`) could cover.
  *
  * What STAYS as implicits in `io.scalaland.chimney.protobufs`: the empty-oneof/`UnrecognizedEnum` partial instances
  * (they match a BOUNDED `From` type family for ANY `To`, and one is referenced directly by a spec), and
  * `DefaultValue[UnknownFieldSet]`.
  *
  * Implementation notes (mirroring [[ProtobufsMacroExtension]]): all `Expr.quote` bodies reference only
  * scalapb/protobuf/std-library API (FULLY QUALIFIED), never anything from this jar; concretely-typed transformation
  * objects are built once and `asInstanceOf`-cast to the abstract `(From, To)` in `apply` (`Type[?]` `=:=` guards make
  * the cast sound); this file compiles with the module's stripped-down `scalacOptions` (no `-Xsource:3`), hence
  * `with`-intersections and `_`-imports.
  */
final class ProtobufsChimneyMacroExtension extends ChimneyMacroExtension { loader =>

  override def extend(ctx: MacroCommons with ChimneyEngineExtensionApi): Unit = {
    // scalafmt would rewrite `._` to `.*`, but this module compiles without -Xsource:3 (see build.sbt)
    // format: off
    import ctx._
    // format: on

    val ProtoDurationType = Type.of[com.google.protobuf.duration.Duration]
    val JavaDurationType = Type.of[java.time.Duration]
    val FiniteDurationType = Type.of[scala.concurrent.duration.FiniteDuration]
    val ScalaDurationType = Type.of[scala.concurrent.duration.Duration]
    val EmptyType = Type.of[com.google.protobuf.empty.Empty]
    val UnitType = Type.of[Unit]

    // --- com.google.protobuf.duration.Duration <-> java.time / scala.concurrent.duration ---

    def protoToJava: SpecialCasedTransformation[com.google.protobuf.duration.Duration, java.time.Duration] =
      new SpecialCasedTransformation[com.google.protobuf.duration.Duration, java.time.Duration] {
        override def specialCase(implicit
            context: SpecialCaseContext[com.google.protobuf.duration.Duration, java.time.Duration]
        ): MIO[Option[DerivedExpr[java.time.Duration]]] =
          specialCasedTotal(Expr.quote {
            val d = Expr.splice(sourceOf(context))
            java.time.Duration.ofSeconds(d.seconds, d.nanos.toLong)
          })
      }

    def javaToProto: SpecialCasedTransformation[java.time.Duration, com.google.protobuf.duration.Duration] =
      new SpecialCasedTransformation[java.time.Duration, com.google.protobuf.duration.Duration] {
        override def specialCase(implicit
            context: SpecialCaseContext[java.time.Duration, com.google.protobuf.duration.Duration]
        ): MIO[Option[DerivedExpr[com.google.protobuf.duration.Duration]]] =
          specialCasedTotal(Expr.quote {
            val d = Expr.splice(sourceOf(context))
            com.google.protobuf.duration.Duration.of(d.getSeconds, d.getNano)
          })
      }

    def protoToFinite
        : SpecialCasedTransformation[com.google.protobuf.duration.Duration, scala.concurrent.duration.FiniteDuration] =
      new SpecialCasedTransformation[
        com.google.protobuf.duration.Duration,
        scala.concurrent.duration.FiniteDuration
      ] {
        override def specialCase(implicit
            context: SpecialCaseContext[com.google.protobuf.duration.Duration, scala.concurrent.duration.FiniteDuration]
        ): MIO[Option[DerivedExpr[scala.concurrent.duration.FiniteDuration]]] =
          specialCasedTotal(Expr.quote {
            // Duration.fromNanos(Long): FiniteDuration - a companion METHOD (qualifies cleanly), NOT the
            // `SECONDS`/`NANOSECONDS` TimeUnit CONSTANTS (Scala 2 cross-quotes reification emits enum/package-object
            // constants by their SIMPLE name, which then fails to resolve at the splice site).
            val d = Expr.splice(sourceOf(context))
            scala.concurrent.duration.Duration.fromNanos(d.seconds * 1000000000L + d.nanos.toLong)
          })
      }

    def finiteToProto
        : SpecialCasedTransformation[scala.concurrent.duration.FiniteDuration, com.google.protobuf.duration.Duration] =
      new SpecialCasedTransformation[
        scala.concurrent.duration.FiniteDuration,
        com.google.protobuf.duration.Duration
      ] {
        override def specialCase(implicit
            context: SpecialCaseContext[scala.concurrent.duration.FiniteDuration, com.google.protobuf.duration.Duration]
        ): MIO[Option[DerivedExpr[com.google.protobuf.duration.Duration]]] =
          specialCasedTotal(Expr.quote {
            val d = Expr.splice(sourceOf(context))
            val nanosInSecond = 1000000000L
            val seconds = d.toNanos / nanosInSecond
            val nanos = d.toNanos - (seconds * nanosInSecond)
            com.google.protobuf.duration.Duration.of(seconds, nanos.toInt)
          })
      }

    // proto Duration -> scala.concurrent.duration.Duration is the FiniteDuration conversion upcast to the abstract type.
    def protoToScala
        : SpecialCasedTransformation[com.google.protobuf.duration.Duration, scala.concurrent.duration.Duration] =
      new SpecialCasedTransformation[com.google.protobuf.duration.Duration, scala.concurrent.duration.Duration] {
        override def specialCase(implicit
            context: SpecialCaseContext[com.google.protobuf.duration.Duration, scala.concurrent.duration.Duration]
        ): MIO[Option[DerivedExpr[scala.concurrent.duration.Duration]]] =
          specialCasedTotal(Expr.quote {
            val d = Expr.splice(sourceOf(context))
            (scala.concurrent.duration.Duration
              .fromNanos(d.seconds * 1000000000L + d.nanos.toLong)): scala.concurrent.duration.Duration
          })
      }

    def scalaToProto
        : SpecialCasedTransformation[scala.concurrent.duration.Duration, com.google.protobuf.duration.Duration] =
      new SpecialCasedTransformation[scala.concurrent.duration.Duration, com.google.protobuf.duration.Duration] {
        override def specialCase(implicit
            context: SpecialCaseContext[scala.concurrent.duration.Duration, com.google.protobuf.duration.Duration]
        ): MIO[Option[DerivedExpr[com.google.protobuf.duration.Duration]]] =
          specialCasedPartial(Expr.quote {
            Expr.splice(sourceOf(context)) match {
              case _: scala.concurrent.duration.Duration.Infinite =>
                io.scalaland.chimney.partial.Result.fromErrorString(
                  "scala.concurrent.duration.Duration.Infinite cannot be encoded as com.google.protobuf.duration.Duration"
                )
              case d: scala.concurrent.duration.FiniteDuration =>
                io.scalaland.chimney.partial.Result.fromValue {
                  val nanosInSecond = 1000000000L
                  val seconds = d.toNanos / nanosInSecond
                  val nanos = d.toNanos - (seconds * nanosInSecond)
                  com.google.protobuf.duration.Duration.of(seconds, nanos.toInt)
                }
            }
          })
      }

    // --- com.google.protobuf.empty.Empty ---

    // Empty -> Unit: total by default; partial (Result.fromEmpty) only when the user opted into PreferPartialTransformer
    // in a partial context. This collapses the old Transformer[Empty, Unit] + PartialTransformer[Empty, A] pair into a
    // single handler that reproduces enableImplicitConflictResolution for the Empty <-> Unit pair.
    def emptyToUnit: SpecialCasedTransformation[com.google.protobuf.empty.Empty, Unit] =
      new SpecialCasedTransformation[com.google.protobuf.empty.Empty, Unit] {
        override def specialCase(implicit
            context: SpecialCaseContext[com.google.protobuf.empty.Empty, Unit]
        ): MIO[Option[DerivedExpr[Unit]]] =
          if (isPartialContext(context) && prefersPartialTransformer(context))
            specialCasedPartial(Expr.quote(io.scalaland.chimney.partial.Result.fromEmpty[Unit]))
          else specialCasedTotal(Expr.quote(()))
      }

    // Empty -> A (A != Unit): only a partial path exists (Result.fromEmpty); in a total context there is nothing to do,
    // so yield and let derivation report "not supported" as before.
    def emptyToAny[A](implicit A: Type[A]): SpecialCasedTransformation[com.google.protobuf.empty.Empty, A] =
      new SpecialCasedTransformation[com.google.protobuf.empty.Empty, A] {
        override def specialCase(implicit
            context: SpecialCaseContext[com.google.protobuf.empty.Empty, A]
        ): MIO[Option[DerivedExpr[A]]] =
          if (isPartialContext(context))
            specialCasedPartial(Expr.quote(io.scalaland.chimney.partial.Result.fromEmpty[A]))
          else specialCaseYield[A]
      }

    // A -> Empty for ANY A (total): the case IsValueType[Empty] could not express.
    def anyToEmpty[A](implicit A: Type[A]): SpecialCasedTransformation[A, com.google.protobuf.empty.Empty] =
      new SpecialCasedTransformation[A, com.google.protobuf.empty.Empty] {
        override def specialCase(implicit
            context: SpecialCaseContext[A, com.google.protobuf.empty.Empty]
        ): MIO[Option[DerivedExpr[com.google.protobuf.empty.Empty]]] =
          specialCasedTotal(Expr.quote(com.google.protobuf.empty.Empty.of()))
      }

    registerSpecialCase(new SpecialCaseHandler {
      override def apply[From, To](implicit
          From: Type[From],
          To: Type[To]
      ): Option[SpecialCasedTransformation[From, To]] = {
        def some[F, T](t: SpecialCasedTransformation[F, T]): Option[SpecialCasedTransformation[From, To]] =
          Some(t.asInstanceOf[SpecialCasedTransformation[From, To]])

        if (From =:= ProtoDurationType && To =:= JavaDurationType) some(protoToJava)
        else if (From =:= JavaDurationType && To =:= ProtoDurationType) some(javaToProto)
        else if (From =:= ProtoDurationType && To =:= FiniteDurationType) some(protoToFinite)
        else if (From =:= FiniteDurationType && To =:= ProtoDurationType) some(finiteToProto)
        else if (From =:= ProtoDurationType && To =:= ScalaDurationType) some(protoToScala)
        else if (From =:= ScalaDurationType && To =:= ProtoDurationType) some(scalaToProto)
        else if (From =:= EmptyType && To =:= UnitType) some(emptyToUnit)
        else if (From =:= EmptyType) some(emptyToAny[To](To))
        else if (To =:= EmptyType) some(anyToEmpty[From](From))
        else None
      }
    })
  }
}
