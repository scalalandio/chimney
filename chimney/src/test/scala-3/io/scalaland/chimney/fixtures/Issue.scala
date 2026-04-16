package io.scalaland.chimney.fixtures

object Issue835 {
  abstract class StatusEntity(val status: String)

  abstract class AbstractIdStatusEntity(val id: Long, status: String) extends StatusEntity(status)

  class IdStatusEntity(id: Long, status: String) extends AbstractIdStatusEntity(id, status)

  abstract class AbstractIdStatusGetter(id: Long, status: String) extends StatusEntity(status) {
    def getId: Long = id
  }

  class IdStatusGetter(id: Long, status: String) extends AbstractIdStatusEntity(id, status)

  case class IdStatus(id: Long, status: String)

}
