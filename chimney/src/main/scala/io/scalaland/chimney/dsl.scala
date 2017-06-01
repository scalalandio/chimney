package io.scalaland.chimney

import shapeless.{HList, HNil, Witness, ::}

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    def into[To]: TransformerInto[From, To, HNil] =
      new TransformerInto(source, HNil)

    def transformInto[To](implicit derivedTransformer: DerivedTransformer[From, From, To, HNil]): To =
      derivedTransformer.transform(source, HNil)
  }

  class TransformerInto[From, To, Modifiers <: HList](val source: From,
                                                      val modifiers: Modifiers) {

    def withFieldConst[T](label: Witness.Lt[Symbol],
                          value: T) = withFieldComputed(label, _ => value)

    def withFieldComputed[T](label: Witness.Lt[Symbol],
                             f: From => T): TransformerInto[From, To, Modifier.fieldFunction[label.T, From, T] :: Modifiers] =
      new TransformerInto(source, new Modifier.fieldFunction[label.T, From, T](f) :: modifiers)

    def withFieldRenamed(label1: Witness.Lt[Symbol],
                         label2: Witness.Lt[Symbol]): TransformerInto[From, To, Modifier.relabel[label1.T, label2.T] :: Modifiers] =
      new TransformerInto(source, new Modifier.relabel[label1.T, label2.T] :: modifiers)

    def transform(implicit transformer: DerivedTransformer[From, From, To, Modifiers]): To =
      transformer.transform(source, modifiers)
  }

}
