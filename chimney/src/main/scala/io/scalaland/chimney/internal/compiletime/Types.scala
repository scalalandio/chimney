package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl.ImplicitTransformerPreference
import io.scalaland.chimney.{internal, partial, PartialTransformer, Patcher, Transformer}

private[compiletime] trait Types { this: Definitions =>

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
}
