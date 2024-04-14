package io.scalaland.chimney.integrations

trait OptionalValueOf[Optional, Value] {

  def empty: Optional

  def of(value: Value): Optional

  def fold[A](oa: Optional, onNone: => A, onSome: Value => A): A

  def getOrElse(oa: Optional, onNone: => Value): Value = fold(oa, onNone, identity)

  def orElse(oa: Optional, onNone: => Optional): Optional = fold(oa, onNone, _ => oa)
}
object OptionalValueOf {

  sealed trait Experiment[+A] extends Product with Serializable
  object Experiment {
    final case class FullOf[+A](value: A) extends Experiment[A]
    case object Nope extends Experiment[Nothing]
  }

  implicit def experimentOfA[A]: OptionalValueOf[Experiment[A], A] = new OptionalValueOf[Experiment[A], A] {
    override def empty: Experiment[A] = Experiment.Nope
    override def of(value: A): Experiment[A] = Experiment.FullOf(value)
    override def fold[A0](
        oa: Experiment[A],
        onNone: => A0,
        onSome: A => A0
    ): A0 = oa match {
      case Experiment.FullOf(value) => onSome(value)
      case Experiment.Nope          => onNone
    }
  }
}
