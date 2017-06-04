package io.scalaland.chimney

trait Transformer[From, To] {

  def transform(src: From): To
}
