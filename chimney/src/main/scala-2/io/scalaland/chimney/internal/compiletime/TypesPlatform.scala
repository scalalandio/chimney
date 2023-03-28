package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import DefinitionsPlatform.*
  import c.universe.{internal as _, Transformer as _, *}

  protected type Tagged[U] = { type Tag = U }
  protected type @@[T, U] = T & Tagged[U]

  final override protected type Type[T] = c.Type @@ T
  protected object typeImpl extends TypeDefinitionsImpl {
    def fromUntyped[T](untyped: c.Type): Type[T] = untyped.asInstanceOf[Type[T]]
    def fromWeak[T: WeakTypeTag]: Type[T] = fromUntyped(weakTypeOf[T])
    def fromWeakTC[Unswapped: WeakTypeTag, T](args: c.Type*): Type[T] = fromUntyped {
      val ee = weakTypeOf[Unswapped].etaExpand
      // $COVERAGE-OFF$
      if (ee.typeParams.size != args.size) {
        val een = ee.typeParams.size
        val argsn = args.size
        c.abort(c.enclosingPosition, s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
      }
      // $COVERAGE-ON$
      ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
    }

    override val Any: Type[Any] = fromWeak[Any]
    override val Int: Type[Int] = fromWeak[Int]
    override val Unit: Type[Unit] = fromWeak[Unit]

    override def Function1[From: Type, To: Type]: Type[From => To] =
      fromWeakTC[Any => Any, From => To](Type[From], Type[To])
    override def Array[T: Type]: Type[Array[T]] = fromWeakTC[Array[Arbitrary], Array[T]](Type[T])
    override def Option[T: Type]: Type[Option[T]] = fromWeakTC[Option[Arbitrary], Option[T]](Type[T])
    override def Either[L: Type, R: Type]: Type[Either[L, R]] =
      fromWeakTC[Either[Arbitrary, Arbitrary2], Either[L, R]](Type[L], Type[R])

    override def Transformer[From: Type, To: Type]: Type[Transformer[From, To]] =
      fromWeakTC[Transformer[Arbitrary, Arbitrary2], Transformer[From, To]](Type[From], Type[To])
    override def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]] =
      fromWeakTC[PartialTransformer[Arbitrary, Arbitrary2], PartialTransformer[From, To]](Type[From], Type[To])
    override def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] =
      fromWeakTC[Patcher[Arbitrary, Arbitrary2], Patcher[T, Patch]](Type[T], Type[Patch])

    override def PartialResult[T: Type]: Type[partial.Result[T]] =
      fromWeakTC[partial.Result[Arbitrary], partial.Result[T]](Type[T])
    override def PartialResultValue[T: Type]: Type[partial.Result.Value[T]] =
      fromWeakTC[partial.Result.Value[Arbitrary], partial.Result.Value[T]](Type[T])
    override def PartialResultErrors: Type[partial.Result.Errors] =
      fromWeak[partial.Result.Errors]

    override def PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      fromWeak[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    override def PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      fromWeak[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    override def TransformerFlagsDefault: Type[internal.TransformerFlags.Default] =
      fromWeak[internal.TransformerFlags.Default]
    // def TransformerFlagsEnable[]: Type[internal.TransformerFlags.Default]
    override def TransformerFlagsDefaultValues: Type[internal.TransformerFlags.DefaultValues] =
      fromWeak[internal.TransformerFlags.DefaultValues]
    override def TransformerFlagsBeanGetters: Type[internal.TransformerFlags.BeanGetters] =
      fromWeak[internal.TransformerFlags.BeanGetters]
    override def TransformerFlagsBeanSetters: Type[internal.TransformerFlags.BeanSetters] =
      fromWeak[internal.TransformerFlags.BeanSetters]
    override def TransformerFlagsMethodAccessors: Type[internal.TransformerFlags.MethodAccessors] =
      fromWeak[internal.TransformerFlags.MethodAccessors]
    override def TransformerFlagsOptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone] =
      fromWeak[internal.TransformerFlags.OptionDefaultsToNone]
    override def TransformerFlagsImplicitConflictResolution[R <: dsls.ImplicitTransformerPreference: Type]
        : Type[internal.TransformerFlags.ImplicitConflictResolution[R]] = fromWeakTC[
      internal.TransformerFlags.ImplicitConflictResolution[io.scalaland.chimney.dsl.PreferTotalTransformer.type],
      internal.TransformerFlags.ImplicitConflictResolution[R],
    ](Type[R])

    def isSubtypeOf[S, T](S: Type[S], T: Type[T]): Boolean = S.<:<(T)
    def isSameAs[S, T](S: Type[S], T: Type[T]): Boolean = S.=:=(T)
  }

  implicit class UntypedTypeOps(private val tpe: c.Type) {

    /** Assumes that this `tpe` is String singleton type and extracts its value */
    def asStringSingletonType: String = tpe
      .asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
      .value
      .value
      .asInstanceOf[String]
  }
}
