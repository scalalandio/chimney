package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

private[compiletime] trait Definitions extends ConfigurationDefinitions with ResultDefinitions {

  /** Platform-specific type representation (c.universe.Type in 2, scala.quoted.Type[A] in 3) */
  protected type Type[T]
  protected object Type {
    final def apply[T](implicit T: Type[T]): Type[T] = T

    val Any: Type[Any] = typeImpl.Any
    val Int: Type[Int] = typeImpl.Int
    val Unit: Type[Unit] = typeImpl.Unit

    object Function1 {
      def apply[From: Type, To: Type]: Type[From => To] = typeImpl.Function1[From, To]
    }

    object Array {
      def apply[T: Type]: Type[Array[T]] = typeImpl.Array[T]
      val Any: Type[Array[Any]] = apply(Type.Any)
    }

    object Option {
      def apply[T: Type]: Type[Option[T]] = typeImpl.Option[T]
    }

    object Either {
      def apply[L: Type, R: Type]: Type[Either[L, R]] = typeImpl.Either[L, R]
    }

    object Transformer {
      def apply[From: Type, To: Type]: Type[Transformer[From, To]] = typeImpl.Transformer[From, To]
    }
    object PartialTransformer {
      def apply[From: Type, To: Type]: Type[PartialTransformer[From, To]] = typeImpl.PartialTransformer[From, To]
    }
    object Patcher {
      def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]] = typeImpl.Patcher[T, Patch]
    }

    object PartialResult {
      object Value {
        def apply[T: Type]: Type[partial.Result.Value[T]] = typeImpl.PartialResultValue[T]
      }
      val Errors: Type[partial.Result.Errors] = typeImpl.PartialResultErrors
      def apply[T: Type]: Type[partial.Result[T]] = typeImpl.PartialResult[T]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type] =
      typeImpl.PreferTotalTransformer
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type] =
      typeImpl.PreferPartialTransformer

    object TransformerFlags {

      val Default: Type[internal.TransformerFlags.Default] = typeImpl.TransformerFlagsDefault
      // TODO: Enable
      // TODO: Disable

      object Flags {

        val DefaultValues: Type[internal.TransformerFlags.DefaultValues] = typeImpl.TransformerFlagsDefaultValues
        val BeanGetters: Type[internal.TransformerFlags.BeanGetters] = typeImpl.TransformerFlagsBeanGetters
        val BeanSetters: Type[internal.TransformerFlags.BeanSetters] = typeImpl.TransformerFlagsBeanSetters
        val MethodAccessors: Type[internal.TransformerFlags.MethodAccessors] = typeImpl.TransformerFlagsMethodAccessors
        val OptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone] =
          typeImpl.TransformerFlagsOptionDefaultsToNone
        // TODO: ImplicitConflictResolution
      }
    }
  }
  implicit class TypeOps[T](private val tpe: Type[T]) {

    def <:<[S](another: Type[S]): Boolean = typeImpl.isSubtypeOf(tpe, another)
    def =:=[S](another: Type[S]): Boolean = typeImpl.isSameAs(tpe, another)
  }

  /** Used to erase the type of Type, while providing the utilities to still make it useful */
  type ComputedType = { type Underlying }
  object ComputedType {
    def apply[T](tpe: Type[T]): ComputedType = tpe.asInstanceOf[ComputedType]
  }
  implicit class ComputedTypeOps(val ct: ComputedType) {
    def Type: Type[ct.Underlying] = ct.asInstanceOf[Type[ct.Underlying]]
    def use[Out](thunk: Type[ct.Underlying] => Out): Out = thunk(Type)
  }

  /** Platform-specific expression representation (c.universe.Expr[A] in 2, quotes.Expr[A] in 3 */
  protected type Expr[A]
  protected object Expr {

    val Unit: Expr[Unit] = exprImpl.Unit

    object Array {
      def apply[A: Type](args: Expr[A]*): Expr[Array[A]] = exprImpl.Array[A](args*)
    }

    object Option {
      def apply[A: Type](a: Expr[A]): Expr[Option[A]] = exprImpl.Option[A](a)
      def empty[A: Type]: Expr[Option[A]] = exprImpl.OptionEmpty[A]
      def apply[A: Type]: Expr[A => Option[A]] = exprImpl.OptionApply[A]
    }
    val None: Expr[scala.None.type] = exprImpl.None

    object Either {
      def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]] = exprImpl.Left[L, R](value)
      def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]] = exprImpl.Right[L, R](value)
    }

    object PartialResult {
      object Value {
        def apply[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]] = exprImpl.PartialResultValue[T](value)
      }
      object Errors {
        def merge(
            errors1: Expr[partial.Result.Errors],
            errors2: Expr[partial.Result.Errors]
        ): Expr[partial.Result.Errors] = exprImpl.PartialResultErrorsMerge(errors1, errors2)
        def mergeResultNullable[T: Type](
            errorsNullable: Expr[partial.Result.Errors],
            result: Expr[partial.Result[T]]
        ): Expr[partial.Result.Errors] = exprImpl.PartialResultErrorsMergeResultNullable(errorsNullable, result)
      }

      def fromEmpty[T: Type]: Expr[partial.Result[T]] = exprImpl.PartialResultEmpty
      def fromFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]] =
        exprImpl.PartialResultFunction[S, T](f)

      def traverse[M: Type, A: Type, B: Type](
          it: Expr[Iterator[A]],
          f: Expr[A => partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] = exprImpl.PartialResultTraverse[M, A, B](it, f, failFast)
      def sequence[M: Type, A: Type](
          it: Expr[Iterator[partial.Result[A]]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[M]] = exprImpl.PartialResultSequence[M, A](it, failFast)
      def map2[A: Type, B: Type, C: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          f: Expr[(A, B) => C],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[C]] = exprImpl.PartialResultMap2[A, B, C](fa, fb, f, failFast)
      def product[A: Type, B: Type](
          fa: Expr[partial.Result[A]],
          fb: Expr[partial.Result[B]],
          failFast: Expr[Boolean]
      ): Expr[partial.Result[(A, B)]] = exprImpl.PartialResultProduct[A, B](fa, fb, failFast)
    }

    object PathElement {
      object Accessor {
        def apply(targetName: Expr[String]): Expr[partial.PathElement.Accessor] =
          exprImpl.PathElementAccessor(targetName)
      }
      object Index {
        def apply(index: Expr[Int]): Expr[partial.PathElement.Index] = exprImpl.PathElementIndex(index)
      }
      object MapKey {
        def apply(key: Expr[Any]): Expr[partial.PathElement.MapKey] = exprImpl.PathElementMapKey(key)
      }
      object MapValue {
        def apply(key: Expr[Any]): Expr[partial.PathElement.MapValue] = exprImpl.PathElementMapValue(key)
      }
    }
  }
  implicit class ExprOps[T](private val expr: Expr[T]) {

    def asInstanceOf[S: Type]: Expr[S] = exprImpl.AsInstanceOf[T, S](expr)
  }

  // Platform specific implementations gathered in a few variables to not pollute scope

  protected def typeImpl: TypeDefinitionsImpl
  protected trait TypeDefinitionsImpl {

    def Any: Type[Any]
    def Int: Type[Int]
    def Unit: Type[Unit]

    def Function1[From: Type, To: Type]: Type[From => To]
    def Array[T: Type]: Type[Array[T]]
    def Option[T: Type]: Type[Option[T]]
    def Either[L: Type, R: Type]: Type[Either[L, R]]

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]]
    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]]
    def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]]

    def PartialResult[T: Type]: Type[partial.Result[T]]
    def PartialResultValue[T: Type]: Type[partial.Result.Value[T]]
    def PartialResultErrors: Type[partial.Result.Errors]

    def PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    def PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    def TransformerFlagsDefault: Type[internal.TransformerFlags.Default]
    // def TransformerFlagsEnable[]: Type[internal.TransformerFlags.Default]
    def TransformerFlagsDefaultValues: Type[internal.TransformerFlags.DefaultValues]
    def TransformerFlagsBeanGetters: Type[internal.TransformerFlags.BeanGetters]
    def TransformerFlagsBeanSetters: Type[internal.TransformerFlags.BeanSetters]
    def TransformerFlagsMethodAccessors: Type[internal.TransformerFlags.MethodAccessors]
    def TransformerFlagsOptionDefaultsToNone: Type[internal.TransformerFlags.OptionDefaultsToNone]
    def TransformerFlagsImplicitConflictResolution[R <: ImplicitTransformerPreference: Type]
        : Type[internal.TransformerFlags.ImplicitConflictResolution[R]]

    def isSubtypeOf[S, T](S: Type[S], T: Type[T]): Boolean
    def isSameAs[S, T](S: Type[S], T: Type[T]): Boolean
  }
  protected def exprImpl: ExprDefinitionsImpl
  protected trait ExprDefinitionsImpl {

    def Unit: Expr[Unit]

    def Array[A: Type](args: Expr[A]*): Expr[Array[A]]

    def Option[A: Type](value: Expr[A]): Expr[Option[A]]
    def OptionEmpty[A: Type]: Expr[Option[A]]
    def OptionApply[A: Type]: Expr[A => Option[A]]
    def None: Expr[scala.None.type]

    def Left[L: Type, R: Type](value: Expr[L]): Expr[Left[L, R]]
    def Right[L: Type, R: Type](value: Expr[R]): Expr[Right[L, R]]

    def PartialResultValue[T: Type](value: Expr[T]): Expr[partial.Result.Value[T]]
    def PartialResultErrorsMerge(
        errors1: Expr[partial.Result.Errors],
        errors2: Expr[partial.Result.Errors]
    ): Expr[partial.Result.Errors]
    def PartialResultErrorsMergeResultNullable[T: Type](
        errorsNullable: Expr[partial.Result.Errors],
        result: Expr[partial.Result[T]]
    ): Expr[partial.Result.Errors]
    def PartialResultEmpty[T: Type]: Expr[partial.Result[T]]
    def PartialResultFunction[S: Type, T: Type](f: Expr[S => T]): Expr[S => partial.Result[T]]
    def PartialResultTraverse[M: Type, A: Type, B: Type](
        it: Expr[Iterator[A]],
        f: Expr[A => partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]]
    def PartialResultSequence[M: Type, A: Type](
        it: Expr[Iterator[partial.Result[A]]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[M]]
    def PartialResultMap2[A: Type, B: Type, C: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        f: Expr[(A, B) => C],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[C]]
    def PartialResultProduct[A: Type, B: Type](
        fa: Expr[partial.Result[A]],
        fb: Expr[partial.Result[B]],
        failFast: Expr[Boolean]
    ): Expr[partial.Result[(A, B)]]

    def PathElementAccessor(targetName: Expr[String]): Expr[partial.PathElement.Accessor]
    def PathElementIndex(index: Expr[Int]): Expr[partial.PathElement.Index]
    def PathElementMapKey(index: Expr[Any]): Expr[partial.PathElement.MapKey]
    def PathElementMapValue(index: Expr[Any]): Expr[partial.PathElement.MapValue]

    def AsInstanceOf[T, S: Type](expr: Expr[T]): Expr[S]
  }
}
