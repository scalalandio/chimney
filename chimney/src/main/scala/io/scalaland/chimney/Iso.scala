package io.scalaland.chimney

final case class Iso[From, To](from: Transformer[From, To], to: Transformer[To, From]) extends Iso.AutoDerived[From, To]
object Iso {

  def derive[From, To](implicit
      from: Transformer.AutoDerived[From, To],
      to: Transformer.AutoDerived[To, From]
  ): Iso[From, To] = Iso[From, To](from = safeUpcast[From, To], to = safeUpcast[To, From])

  // TODO: define

  private def safeUpcast[From, To](implicit t: Transformer.AutoDerived[From, To]): Transformer[From, To] =
    t match {
      case _: Transformer[?, ?] => t.asInstanceOf[Transformer[From, To]]
      case _                    => (src: From) => t.transform(src)
    }

  trait AutoDerived[From, To] {
    val from: Transformer[From, To]
    val to: Transformer[To, From]
  }
  object AutoDerived {
    
    implicit def derive[From, To](implicit
        from: Transformer.AutoDerived[From, To],
        to: Transformer.AutoDerived[To, From]
    ): Iso.AutoDerived[From, To] = Iso.derive(from, to)
  }
}
