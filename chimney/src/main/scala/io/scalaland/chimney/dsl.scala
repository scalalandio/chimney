package io.scalaland.chimney

import shapeless.{::, HList, HNil, Witness}

/** Provides syntax for API user. */
object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, HNil] =
      new TransformerInto(source, HNil)

    final def transformInto[To](implicit derivedTransformer: DerivedTransformer[From, To, HNil]): To =
      derivedTransformer.transform(source, HNil)
  }

  final class TransformerInto[From, To, Modifiers <: HList](val source: From, val modifiers: Modifiers) {

    def withFieldConst[T](label: Witness.Lt[Symbol],
                          value: T): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
      withFieldComputed(label, _ => value)

    def withFieldComputed[T](
      label: Witness.Lt[Symbol],
      map: From => T
    ): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
      new TransformerInto(source, new Modifier.fieldFunction[label.T, From, T](map) :: modifiers)

    def withFieldRenamed(
      labelFrom: Witness.Lt[Symbol],
      labelTo: Witness.Lt[Symbol]
    ): TransformerInto[From, To, Modifier.relabel[labelFrom.T, labelTo.T] :: Modifiers] =
      new TransformerInto(source, new Modifier.relabel[labelFrom.T, labelTo.T] :: modifiers)

    def withCoproductInstance[Inst](
      f: Inst => To
    ): TransformerInto[From, To, Modifier.coproductInstance[Inst, To] :: Modifiers] =
      new TransformerInto(source, new Modifier.coproductInstance[Inst, To](f) :: modifiers)

    def transform(implicit transformer: DerivedTransformer[From, To, Modifiers]): To =
      transformer.transform(source, modifiers)
  }

  implicit class PatcherOps[T](val obj: T) extends AnyVal {

    final def patchWith[P](patch: P)(implicit patcher: Patcher[T, P]): T =
      patcher.patch(obj, patch)
  }
}
