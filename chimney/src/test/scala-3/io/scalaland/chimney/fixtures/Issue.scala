package io.scalaland.chimney.fixtures

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.*

object Issue857 {

  trait Entity {
    val id: Int
  }

  case class User(override val id: Int, name: String) extends Entity
  case class CreateUser(name: String)

  case class Message(override val id: Int, message: String, replyTo: Option[String] = None) extends Entity
  case class CreateMessage(message: String)

  trait EntityHydrator[C, E <: Entity] {
    def touch(c: C): E
  }

  object EntityHydrator {
    class EntityHydratorImpl[C, E <: Entity](t: Transformer[C, E]) extends EntityHydrator[C, E] {
      override def touch(c: C): E = t.transform(c)
    }

    inline def make[C, E <: Entity]: EntityHydrator[C, E] = {
      val transformer = Transformer
        .define[C, E]
        .withFieldConst(_.id, 13)
        .enableDefaultValues
        .buildTransformer
      new EntityHydratorImpl[C, E](transformer)
    }
  }
}

object Issue835 {
  abstract class StatusEntity(val status: String)

  abstract class AbstractIdStatusEntity(val id: Long, status: String) extends StatusEntity(status)

  class IdStatusEntity(id: Long, status: String) extends AbstractIdStatusEntity(id, status)

  abstract class AbstractIdStatusGetter(id: Long, status: String) extends StatusEntity(status) {
    def getId: Long = id
  }

  class IdStatusGetter(id: Long, status: String) extends AbstractIdStatusGetter(id, status)

  case class IdStatus(id: Long, status: String)

}
