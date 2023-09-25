package io.scalaland.chimney.protobufs

import io.scalaland.chimney.{partial, PartialTransformer}

// TODO: not yet published
trait ProtobufsPartialTransformerImplicits {
  
  final type IsEmpty = scalapb.GeneratedOneof { type ValueType = Nothing }
  // TODO: not yet published
  implicit def partialTransformerEmptyOneOfInstance[From <: IsEmpty, To]: PartialTransformer[From, To] =
    PartialTransformer(_ => partial.Result.fromEmpty)
}
