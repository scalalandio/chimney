package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.reflect.macros.blackbox

private[compiletime] trait DefinitionsPlatform
    extends Definitions
    with ConfigurationDefinitionsPlatform
    with ResultDefinitionsPlatform {

  val c: blackbox.Context

  import DefinitionsPlatform.*
  import c.universe.{internal as _, Transformer as _, *}

  type Tagged[U] = { type Tag = U }
  type @@[T, U] = T & Tagged[U]

  final override type Type[T] = c.Type @@ T
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
      fromUntyped(c.typeOf[internal.TransformerFlags.Default])
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

  final override type Expr[A] = c.Expr[A]
  protected object exprImpl extends ExprDefinitionsImpl {

    override def Unit: Expr[Unit] = c.Expr(q"()")

    override def Array[A: Type](args: Expr[A]*): Expr[Array[A]] = c.Expr(q"_root_.scala.Array[${Type[A]}](..${args})")

    override def Option[A: Type](value: Expr[A]): Expr[Option[A]] =
      c.Expr(q"_root_.scala.Option[${Type[A]}]($value)")
    override def OptionEmpty[A: Type]: Expr[Option[A]] =
      c.Expr(q"_root_.scala.Option.empty[${Type[A]}]")
    override def OptionApply[A: Type]: Expr[A => Option[A]] =
      c.Expr(q"_root_.scala.Option.apply[${Type[A]}](_)")
    override def None: Expr[scala.None.type] = c.Expr(q"_root_.scala.None")

    override def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] =
      c.Expr(q"new _root_.scala.util.Left[${Type[L]}, ${Type[R]}]($value)")
    override def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] =
      c.Expr(q"new _root_.scala.util.Right[${Type[L]}, ${Type[R]}]($value)")

    override def PartialResultValue[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.Value[${Type[T]}]($value)")

    override def PartialResultErrorsMerge(
        errors1: Expr[partial.Result.Errors],
        errors2: Expr[partial.Result.Errors]
    ): Expr[partial.Result.Errors] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.Errors.merge($errors1, $errors2)")

    override def PartialResultErrorsMergeResultNullable[T: Type](
        errorsNullable: Expr[partial.Result.Errors],
        result: Expr[partial.Result[T]]
    ): Expr[partial.Result.Errors] =
      c.Expr(
        q"_root_.io.scalaland.chimney.partial.Result.Errors.__mergeResultNullable[${Type[T]}]($errorsNullable, $result)"
      )
    override def PartialResultEmpty[T: Type]: Expr[partial.Result[T]] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.fromEmpty[${Type[T]}]")
    override def PartialResultFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.fromFunction[${Type[S]}, ${Type[T]}]($f)")
    override def PartialResultTraverse[M: Type, A: Type, B: Type](
        it: Expr[Iterator[A]],
        f: Expr[A => partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]] =
      c.Expr(
        q"_root_.io.scalaland.chimney.partial.Result.traverse[${Type[M]}, ${Type[A]}, ${Type[B]}]($it, $f, $failFast)"
      )
    override def PartialResultSequence[M: Type, A: Type](
        it: Expr[Iterator[partial.Result[A]]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.sequence[${Type[M]}, ${Type[A]}]($it, $failFast)")
    override def PartialResultMap2[A: Type, B: Type, C: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        f: Expr[(A, B) => C],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[C]] =
      c.Expr(
        q"_root_.io.scalaland.chimney.partial.Result.map2[${Type[A]}, ${Type[B]}, ${Type[C]}]($fa, $fb, $f, $failFast)"
      )
    override def PartialResultProduct[A: Type, B: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[(A, B)]] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.Result.product[${Type[A]}, ${Type[B]}]($fa, $fb, $failFast)")

    override def PathElementAccessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.Accessor($targetName)")
    override def PathElementIndex(index: Expr[Int]): Expr[partial.PathElement.Index] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.Index($index)")
    override def PathElementMapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.MapKey($key)")
    override def PathElementMapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue] =
      c.Expr(q"_root_.io.scalaland.chimney.partial.PathElement.MapValue($key)")

    override def AsInstanceOf[T, S: Type](expr: Expr[T]): c.Expr[S] = c.Expr(q"${expr}.asInstanceOf[${Type[S]}]")
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
private[compiletime] object DefinitionsPlatform {
  type Arbitrary
  type Arbitrary2
}
