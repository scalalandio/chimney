package io.scalaland.chimney

import shapeless._
import shapeless.labelled._
import shapeless.ops.record._

trait Transformer[From, To, M <: Modifier] {

  def transform(src: From, modifier: M): To
}

object Transformer {

  type Aux[From, To] = Transformer[From, To, Modifier.empty]


  implicit def identityTransformer[T, M <: Modifier]: Transformer[T, T, M] =
    (obj: T, _: M) => obj

  implicit def hnilCase[FromG, M <: Modifier]: Transformer[FromG, HNil, M] =
    (_: FromG, _: M) => HNil

  implicit def hconsCase[FromG <: HList, Label <: Symbol, FromFieldT, ToFieldT, TailTo <: HList, M <: Modifier]
    (implicit fieldSelector: Selector.Aux[FromG, Label, FromFieldT],
     fieldTransformer: Transformer[FromFieldT, ToFieldT, M],
     tailTransformer: Transformer[FromG, TailTo, M])
  : Transformer[FromG, FieldType[Label, ToFieldT] :: TailTo, M] = {
    (obj: FromG, modifier: M) =>
      field[Label](fieldTransformer.transform(fieldSelector(obj), modifier)) :: tailTransformer.transform(obj, modifier)
  }

  implicit def hconsCaseFieldValueProvided[FromG <: HList, Label <: Symbol, ProvidedT, ToFieldT, TailToG <: HList]
    (implicit fieldTransformer: Transformer[ProvidedT, ToFieldT, Modifier.fieldValue[Label, ProvidedT]],
     tailTransformer: Transformer[FromG, TailToG, Modifier.fieldValue[Label, ProvidedT]])
  : Transformer[FromG, FieldType[Label, ToFieldT] :: TailToG, Modifier.fieldValue[Label, ProvidedT]] = {
    (obj: FromG, modifier: Modifier.fieldValue[Label, ProvidedT]) =>
      field[Label](fieldTransformer.transform(modifier.value, modifier)) :: tailTransformer.transform(obj, modifier)
  }

  implicit def hconsCaseFieldFunctionProvided[FromG <: HList, Label <: Symbol, ProducedT, ToFieldT, TailToG <: HList, From]
    (implicit fieldTransformer: Transformer[ProducedT, ToFieldT, Modifier.fieldFunction[Label, From, ProducedT]],
     tailTransformer: Transformer[FromG, TailToG, Modifier.fieldFunction[Label, From, ProducedT]],
     tLabGen: LabelledGeneric.Aux[From, FromG])
  : Transformer[FromG, FieldType[Label, ToFieldT] :: TailToG, Modifier.fieldFunction[Label, From, ProducedT]] = {
    (obj: FromG, modifier: Modifier.fieldFunction[Label, From, ProducedT]) =>
      field[Label](fieldTransformer.transform(modifier.f(tLabGen.from(obj)), modifier)) :: tailTransformer.transform(obj, modifier)
  }

  implicit def hconsCaseFieldRelabelled[FromG <: HList, LabelFrom <: Symbol, Label <: Symbol, FromFieldT, ToFieldT, TailToG <: HList]
  (implicit fieldSelector: Selector.Aux[FromG, LabelFrom, FromFieldT],
   fieldTransformer: Transformer[FromFieldT, ToFieldT, Modifier.relabel[LabelFrom, Label]],
   tailTransformer: Transformer[FromG, TailToG, Modifier.relabel[LabelFrom, Label]])
  : Transformer[FromG, FieldType[Label, ToFieldT] :: TailToG, Modifier.relabel[LabelFrom, Label]] = {
    (obj: FromG, modifier: Modifier.relabel[LabelFrom, Label]) =>
      field[Label](fieldTransformer.transform(fieldSelector(obj), modifier)) :: tailTransformer.transform(obj, modifier)
  }

  implicit def gen[From, To, FromG, ToG, M <: Modifier]
    (implicit fromLG: LabelledGeneric.Aux[From, FromG],
     toLG: LabelledGeneric.Aux[To, ToG],
     genTransformer: Transformer[FromG, ToG, M]): Transformer[From, To, M] = {
    (src: From, modifier: M) =>
      toLG.from(genTransformer.transform(fromLG.to(src), modifier))
  }
}
