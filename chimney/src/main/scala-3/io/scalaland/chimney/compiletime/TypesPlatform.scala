package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.quoted

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import quotes.*
  import quotes.reflect.*

  final override protected type Type[T] = quoted.Type[T]
  protected object typeImpl extends TypeDefinitionsImpl {
    def fromTC[Unswapped <: AnyKind: quoted.Type, T](args: Type[?]*): Type[T] = {
      val U = TypeRepr.of[Unswapped]
      // $COVERAGE-OFF$
      if U.typeArgs.size != args.size then {
        val een = U.typeArgs.size
        val argsn = args.size
        report.errorAndAbort(s"Type ${U.show} has different arity ($een) than applied to applyTypeArgs ($argsn)!")
      }
      // $COVERAGE-ON$
      U.appliedTo(args.map(t => TypeRepr.of(using t)).toList).asType.asInstanceOf[Type[T]]
    }

    override val Any: Type[Any] = quoted.Type.of[Any]
    override val Int: Type[Int] = quoted.Type.of[Int]
    override val Unit: Type[Unit] = quoted.Type.of[Unit]

    override def Function1[From: Type, To: Type]: Type[From => To] =
      fromTC[* => *, From => To](Type[From], Type[To])
    override def Array[T: Type]: Type[Array[T]] = fromTC[Array[*], Array[T]](Type[T])
    override def Option[T: Type]: Type[Option[T]] = fromTC[Option[*], Option[T]](Type[T])
    override def Either[L: Type, R: Type]: Type[Either[L, R]] = fromTC[Either[*, *], Either[L, R]](Type[L], Type[R])

    override def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] =
      fromTC[Transformer[*, *], Transformer[From, To]](Type[From], Type[To])
    override def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      fromTC[PartialTransformer[*, *], PartialTransformer[From, To]](Type[From], Type[To])
    override def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] =
      fromTC[Patcher[*, *], Patcher[T, Patch]](Type[T], Type[Patch])

    override def PartialResult[T: Type]: Type[partial.Result[T]] =
      fromTC[partial.Result[*], partial.Result[T]](Type[T])
    override def PartialResultValue[T: Type]: Type[partial.Result.Value[T]] =
      fromTC[partial.Result.Value[*], partial.Result.Value[T]](Type[T])
    override def PartialResultErrors: Type[partial.Result.Errors] =
      quoted.Type.of[partial.Result.Errors]

    override def PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    override def PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      quoted.Type.of[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    override def TransformerFlagsDefault: Type[internal.TransformerFlags.Default] =
      quoted.Type.of[internal.TransformerFlags.Default]
    // def TransformerFlagsEnable[]: Type[internal.TransformerFlags.Default]
    override def TransformerFlagsDefaultValues: Type[internal.TransformerFlags.DefaultValues] =
      quoted.Type.of[internal.TransformerFlags.DefaultValues]
    override def TransformerFlagsBeanGetters: Type[internal.TransformerFlags.BeanGetters] =
      quoted.Type.of[internal.TransformerFlags.BeanGetters]
    override def TransformerFlagsBeanSetters: Type[internal.TransformerFlags.BeanSetters] =
      quoted.Type.of[internal.TransformerFlags.BeanSetters]
    override def TransformerFlagsMethodAccessors: Type[internal.TransformerFlags.MethodAccessors] =
      quoted.Type.of[internal.TransformerFlags.MethodAccessors]
    override def TransformerFlagsOptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone] =
      quoted.Type.of[internal.TransformerFlags.OptionDefaultsToNone]
    override def TransformerFlagsImplicitConflictResolution[R <: dsls.ImplicitTransformerPreference: Type]
        : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] = fromTC[
      [R0 <: dsls.ImplicitTransformerPreference] =>> internal.TransformerFlags.ImplicitConflictResolution[R0],
      internal.TransformerFlags.ImplicitConflictResolution[R],
    ](Type[R])

    def isSubtypeOf[S, T](S: Type[S], T: Type[T]): Boolean = S.<:<(T)
    def isSameAs[S, T](S: Type[S], T: Type[T]): Boolean = S.=:=(T)
  }
}
