package io.scalaland.chimney

import io.scalaland.chimney.internal.Modifier
import shapeless.ops.hlist.FilterNot
import shapeless.{::, HList, HNil, Witness}

import scala.language.experimental.macros



object olddsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, Modifier.enableDefaultValues :: HNil] =
      new TransformerInto(source, Modifier.enableDefaultValues :: HNil)

    final def transformInto[To](
      implicit derivedTransformer: DerivedTransformer[From, To, Modifier.enableDefaultValues :: HNil]
    ): To =
      derivedTransformer.transform(source, Modifier.enableDefaultValues :: HNil)
  }

  final class TransformerInto[From, To, Modifiers <: HList](val source: From, val modifiers: Modifiers) {

    def withFieldConst[T](
      selector: To => T,
      value: T
    ): TransformerInto[From, To, _ <: Modifier.fieldFunction[Symbol, From, T] :: Modifiers] =
      macro OldDslMacros.constFieldSelector

    def withFieldConst[T](label: Witness.Lt[Symbol],
                          value: T): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
      withFieldComputed(label, (_ => value): From => T)

    def withFieldComputed[T](
      selector: To => T,
      map: From => T
    ): TransformerInto[From, To, _ <: Modifier.fieldFunction[Symbol, From, T] :: Modifiers] =
      macro OldDslMacros.computedFieldSelector

    def withFieldComputed[T](
      label: Witness.Lt[Symbol],
      map: From => T
    ): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
      new TransformerInto(source, new Modifier.fieldFunction[label.T, From, T](map) :: modifiers)

    def withFieldRenamed[T](
      selectorFrom: From => T,
      selectorTo: To => T
    ): TransformerInto[From, To, _ <: Modifier.relabel[Symbol, Symbol] :: Modifiers] =
      macro OldDslMacros.renamedFieldSelector

    def withFieldRenamed(
      labelFrom: Witness.Lt[Symbol],
      labelTo: Witness.Lt[Symbol]
    ): TransformerInto[From, To, Modifier.relabel[labelFrom.T, labelTo.T] :: Modifiers] =
      new TransformerInto(source, new Modifier.relabel[labelFrom.T, labelTo.T] :: modifiers)

    def withCoproductInstance[Inst](
      f: Inst => To
    ): TransformerInto[From, To, Modifier.coproductInstance[Inst, To] :: Modifiers] =
      new TransformerInto(source, new Modifier.coproductInstance[Inst, To](f) :: modifiers)

    def disableDefaultValues[Modifiers2 <: HList](
      implicit filterNot: FilterNot.Aux[Modifiers, Modifier.enableDefaultValues, Modifiers2]
    ): TransformerInto[From, To, Modifiers2] =
      new TransformerInto(source, filterNot(modifiers))

    def transform(implicit transformer: DerivedTransformer[From, To, Modifiers]): To =
      transformer.transform(source, modifiers)
  }

  implicit class PatcherOps[T](val obj: T) extends AnyVal {

    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)
  }
}
