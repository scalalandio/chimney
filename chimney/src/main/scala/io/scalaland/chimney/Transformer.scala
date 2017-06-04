package io.scalaland.chimney

/** User-defined transformer.
  *
  * Used for defining custom transformations using implicit values/definitions.
  *
  * @tparam From original type
  * @tparam To target type
  */
trait Transformer[From, To] {

  /** Transforms original value into a target type.
    *
    * @param src original value
    * @return value transformed into a target type
    */
  def transform(src: From): To
}
