package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.*

private[compiletime] trait ChimneyTypes { this: Types =>

  val ChimneyType: ChimneyTypeModule
  trait ChimneyTypeModule {

    def Transformer[From: Type, To: Type]: Type[Transformer[From, To]]
    def PartialTransformer[From: Type, To: Type]: Type[PartialTransformer[From, To]]
    def Patcher[T: Type, Patch: Type]: Type[Patcher[T, Patch]]

    val PartialResult: PartialResultModule
    trait PartialResultModule {
      def apply[T: Type]: Type[partial.Result[T]]

      def Value[T: Type]: Type[partial.Result.Value[T]]
      val Errors: Type[partial.Result.Errors]
    }

    val PreferTotalTransformer: Type[io.scalaland.chimney.dsl.PreferTotalTransformer.type]
    val PreferPartialTransformer: Type[io.scalaland.chimney.dsl.PreferPartialTransformer.type]

    val TransformerFlags: TransformerFlagsModule
    trait TransformerFlagsModule {
      val Default: Type[internal.TransformerFlags]
      def Enable[F: Type, Flags: Type]: Type[internal.TransformerFlags]
      def Disable[F: Type, Flags: Type]: Type[internal.TransformerFlags]

      val Flags: FlagsModule
      trait FlagsModule {
        val DefaultValues: Type[internal.TransformerFlags]
        val BeanGetters: Type[internal.TransformerFlags]
        val BeanSetters: Type[internal.TransformerFlags]
        val MethodAccessors: Type[internal.TransformerFlags]
        val OptionDefaultsToNone: Type[internal.TransformerFlags]
        def ImplicitConflictResolution[R: Type]: Type[internal.TransformerFlags]
      }
    }
  }
}
