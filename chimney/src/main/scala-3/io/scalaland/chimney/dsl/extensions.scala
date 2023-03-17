package io.scalaland.chimney.dsl

import io.scalaland.chimney.{PartialTransformer, Patcher, Transformer}
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.partial

import scala.util.Try

extension [From](source: From) {

  def into[To]: TransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new TransformerInto(source, new TransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore))

  def transformInto[To](implicit transformer: Transformer[From, To]): To =
    transformer.transform(source)
}

extension [From](source: From) {

  def intoPartial[To]: PartialTransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
    new PartialTransformerInto(
      source,
      new PartialTransformerDefinition(TransformerDefinitionCommons.emptyRuntimeDataStore)
    )

  def transformIntoPartial[To](implicit transformer: PartialTransformer[From, To]): partial.Result[To] =
    transformIntoPartial(failFast = false)

  def transformIntoPartial[To](failFast: Boolean)(implicit
      transformer: PartialTransformer[From, To]
  ): partial.Result[To] =
    transformer.transform(source, failFast)
}

extension [T](obj: T) {

  def using[P](patch: P): PatcherUsing[T, P, PatcherCfg.Empty] =
    new PatcherUsing[T, P, PatcherCfg.Empty](obj, patch)

  def patchUsing[P](patch: P)(implicit patcher: Patcher[T, P]): T =
    patcher.patch(obj, patch)
}

extension [T](option: Option[T]) {

  def toPartialResult: partial.Result[T] =
    partial.Result.fromOption(option)

  def toPartialResultOrString(ifEmpty: => String): partial.Result[T] =
    partial.Result.fromOptionOrString(option, ifEmpty)

}

extension [T](either: Either[String, T]) {

  def toPartialResult: partial.Result[T] =
    partial.Result.fromEitherString(either)
}

extension [T](`try`: Try[T]) {

  def toPartialResult: partial.Result[T] =
    partial.Result.fromTry(`try`)
}
