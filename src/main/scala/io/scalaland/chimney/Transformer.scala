package io.scalaland.chimney

import shapeless._
import shapeless.labelled._
import shapeless.ops.record._

trait Transformer[From, To] {

  def transform(src: From): To
}

object Transformer {

  implicit def identityTransformer[T]: Transformer[T, T] =
    (obj: T) => obj

  implicit def hnilCase[FromG]: Transformer[FromG, HNil] =
    (_: FromG) => HNil

  implicit def hconsCase[FromG <: HList, Label <: Symbol, FromFieldT, ToFieldT, TailTo <: HList]
    (implicit fieldSelector: Selector.Aux[FromG, Label, FromFieldT],
     fieldTransformer: Transformer[FromFieldT, ToFieldT],
     labelWit: Witness.Aux[Label],
     tailTransformer: Transformer[FromG, TailTo]): Transformer[FromG, FieldType[Label, ToFieldT] :: TailTo] =
    (obj: FromG) =>
      field[Label](fieldTransformer.transform(fieldSelector(obj))) :: tailTransformer.transform(obj)


  implicit def gen[From, To, FromG, ToG](implicit fromLG: LabelledGeneric.Aux[From, FromG],
                                         toLG: LabelledGeneric.Aux[To, ToG],
                                         genTransformer: Transformer[FromG, ToG]): Transformer[From, To] =
    (src: From) => toLG.from(genTransformer.transform(fromLG.to(src)))
}
