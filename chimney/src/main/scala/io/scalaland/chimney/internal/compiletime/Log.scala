package io.scalaland.chimney.internal.compiletime

sealed private[compiletime] trait Log extends Product with Serializable
private[compiletime] object Log {

  /** Single log entry with lazy evaluation (some messages can be expensive to create) */
  final case class Entry(msg: () => String) extends Log {
    lazy val message: String = msg()
  }
  object Entry {
    def defer(msg: => String): Entry = new Entry(msg = () => msg)
  }

  /** Collection of logs (single or nested) */
  final case class Journal(logs: Vector[Log]) {
    def append(msg: => String): Journal = copy(logs = logs :+ Entry.defer(msg))

    def print: String = Log.print(this, "")
  }

  /** Contains a collection of logs computed in a named, nested scope */
  final case class Scope(scopeName: String, journal: Journal) extends Log

  private val singleIndent = "  "

  private def print(log: Log, indent: String): String = log match {
    case Entry(msg)                => s"$indent+ ${msg().replaceAll("\n", s"\n${indent}| ")}\n"
    case Scope(scopeName, journal) => s"$indent+ $scopeName\n${print(journal, indent + singleIndent)}"
  }

  private def print(journal: Journal, indent: String): String =
    journal.logs.map(print(_, indent)).mkString
}
