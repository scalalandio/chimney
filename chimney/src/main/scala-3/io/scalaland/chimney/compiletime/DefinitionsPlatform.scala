package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.quoted

private[compiletime] trait DefinitionsPlatform(using val quotes: quoted.Quotes)
    extends Definitions
    with ConfigurationDefinitionsPlatform
    with ResultDefinitionsPlatform {

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

  final override type Expr[A] = quoted.Expr[A]
  protected object exprImpl extends ExprDefinitionsImpl {

    override def Unit: Expr[Unit] = '{ () }

    override def Array[A: Type](args: Expr[A]*): Expr[Array[A]] =
      '{ scala.Array.apply[A](${ quoted.Varargs(args.toSeq) }*)(???) } // TODO: classTag?

    override def Option[A: Type](value: Expr[A]): Expr[Option[A]] = '{ scala.Option(${ value }) }
    override def OptionEmpty[A: Type]: Expr[Option[A]] = '{ scala.Option.empty[A] }
    override def OptionApply[A: Type]: Expr[A => Option[A]] = '{ scala.Option.apply[A](_) }
    override def None: Expr[scala.None.type] = '{ scala.None }

    override def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = '{ scala.Left[L, R](${ value }) }
    override def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = '{ scala.Right[L, R](${ value }) }

    override def PartialResultValue[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] =
      '{ partial.Result.Value[T](${ value }) }

    override def PartialResultErrorsMerge(
        errors1: Expr[partial.Result.Errors],
        errors2: Expr[partial.Result.Errors]
    ): Expr[partial.Result.Errors] = '{ partial.Result.Errors.merge(${ errors1 }, ${ errors2 }) }

    override def PartialResultErrorsMergeResultNullable[T: Type](
        errorsNullable: Expr[partial.Result.Errors],
        result: Expr[partial.Result[T]]
    ): Expr[partial.Result.Errors] =
      '{ partial.Result.Errors.__mergeResultNullable[T](${ errorsNullable }, ${ result }) }
    override def PartialResultEmpty[T: Type]: Expr[partial.Result[T]] =
      '{ partial.Result.fromEmpty[T] }
    override def PartialResultFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
      '{ partial.Result.fromFunction[S, T](${ f }) }
    // TODO: summon factory?
    override def PartialResultTraverse[M: Type, A: Type, B: Type](
        it: Expr[Iterator[A]],
        f: Expr[A => partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]] = '{ partial.Result.traverse[M, A, B](${ it }, ${ f }, ${ failFast })(???) }
    // TODO: summon factory?
    override def PartialResultSequence[M: Type, A: Type](
        it: Expr[Iterator[partial.Result[A]]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]] = '{ partial.Result.sequence[M, A](${ it }, ${ failFast })(???) }
    override def PartialResultMap2[A: Type, B: Type, C: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        f: Expr[(A, B) => C],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[C]] = '{ partial.Result.map2[A, B, C](${ fa }, ${ fb }, ${ f }, ${ failFast }) }
    override def PartialResultProduct[A: Type, B: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[(A, B)]] = '{ partial.Result.product[A, B](${ fa }, ${ fb }, ${ failFast }) }

    override def PathElementAccessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
      '{ partial.PathElement.Accessor(${ targetName }) }
    override def PathElementIndex(index: Expr[Int]): Expr[partial.PathElement.Index] =
      '{ partial.PathElement.Index(${ index }) }
    override def PathElementMapKey(key: Expr[Any]): Expr[partial.PathElement.MapKey] =
      '{ partial.PathElement.MapKey(${ key }) }
    override def PathElementMapValue(key: Expr[Any]): Expr[partial.PathElement.MapValue] =
      '{ partial.PathElement.MapValue(${ key }) }

    override def AsInstanceOf[T: Type, S: Type](expr: Expr[T]): Expr[S] = '{ ${ expr }.asInstanceOf[S] }
  }
}
