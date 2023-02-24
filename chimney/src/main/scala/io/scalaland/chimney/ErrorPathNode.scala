package io.scalaland.chimney

/** Path segment for [[io.scalaland.chimney.TransformerF]]
  *
  * @see [[io.scalaland.chimney.TransformerFErrorPathSupport]]
  *
  * @since 0.6.1
  */
@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
sealed trait ErrorPathNode {

  /** @since 0.6.1 */
  def show: String

  /** @since 0.6.1 */
  def separator: String
}

/** @since 0.6.1 */
@deprecated("Lifted transformers are deprecated. Consider using PartialTransformer.", since = "Chimney 0.7.0")
object ErrorPathNode {

  /** @since 0.6.1 */
  final case class Accessor(name: String) extends ErrorPathNode {
    def show: String = name

    def separator: String = "."
  }

  /** @since 0.6.1 */
  final case class Index(value: Int) extends ErrorPathNode {
    def show: String = s"($value)"

    def separator: String = ""
  }

  /** @since 0.6.1 */
  final case class MapValue(key: AnyRef) extends ErrorPathNode {
    def show: String = s"($key)"

    def separator: String = ""
  }

  /** @since 0.6.1 */
  final case class MapKey(key: AnyRef) extends ErrorPathNode {
    def show: String = s"keys($key)"

    def separator: String = "."
  }
}
