//> using scala 3.3.3

import java.io.File
import java.nio.file.{Paths, Files}
import scala.util.Using
import scala.sys.process._
import scala.Console.{MAGENTA, RESET}
import java.sql.Savepoint

// config

def resolveVersion(): String = {
  val tag = "git describe --tags".!!
  if (tag.matches(".+-[0-9]+-[0-9a-z]{8}")) tag
  else tag + "-SNAPSHOT"
}

val patterns = Map(
  // keeps in sync with what sbt produces
  "{{ chimney_version() }}" -> "1.0.0-RC1", //resolveVersion(),
  // keep in sync with mkdocs.yml
  "{{ libraries.ducktape }}" -> "0.2.0",
  "{{ libraries.henkan }}" -> "0.6.5",
  "{{ libraries.scala_automapper }}" -> "0.7.0",
  "{{ scala.2_12 }}"  -> "2.12.18",
  "{{ scala.2_13 }}"  -> "2.13.13",
  "{{ scala.3 }}"  -> "3.3.3",
)

val ignored: Set[String] = Set(
  // landing page (check this code manually!!!):
  // abstract examples (code that demonstrates some idea but doesn't work in its own):
)

lazy val tmpDir = Files.createTempDirectory(s"docs-snippets").toFile()

// models

case class Snippet(name: String, hint: String, content: String) {

  lazy val snippetFile: File = File(s"${tmpDir.getPath()}/$name/snippet.sc")
  lazy val snippetDir: String = snippetFile.getParent()

  def isIgnored: Boolean = ignored(name)

  def save(): Unit = {
    snippetFile.getParentFile().mkdirs()
    Files.writeString(snippetFile.toPath(), content)
  }
}
object Snippet {

  def extractAll(markdown: Markdown): List[Snippet] = {
    val name = markdown.name

    case class Example(section: String, ordinal: Int = 1) {

      def next: Example = copy(ordinal = ordinal + 1)

      def toName: String = s"${name}_${section}_${ordinal}".replaceAll(" +", "-").replaceAll("[^A-Za-z0-9_-]+", "")
    }

    enum Mode:
      case Reading(lineNo: Int,indent: Int, contentReverse: List[String]) 
      case Awaiting

    import Mode.*

    val start = "```scala"
    val end   = "```"
    val sectionName = "#+(.+)".r

    def adjustLine(line: String, indent: Int): String = {
      val stripIndent = if (line.length() > indent) line.substring(indent) else line
      patterns.foldLeft(stripIndent) { case (s, (k, v)) =>
        s.replace(k, v)
      }
    }

    def mkSnippet(example: Example, lineNo: Int, contentReverse: List[String]): Snippet = {
      val content0 = contentReverse.reverse.mkString("\n")
      val content = if (content0.startsWith("//> using scala")) content0
                    else "//> using scala 2.13.13\n" + content0
      Snippet(example.toName, s"$name:$lineNo", content)
    }

    def loop(content: List[(String, Int)], example: Example, mode: Mode, reverseResult: List[Snippet]): List[Snippet] = content match {
      case (line, lineNo) :: lines => mode match {
        case Reading(lineNo, indent, contentReverse) =>
          if (line.trim() == end) loop(lines, example, Awaiting, mkSnippet(example, lineNo, contentReverse) :: reverseResult)
          else loop(lines, example, Reading(lineNo, indent, adjustLine(line, indent) :: contentReverse), reverseResult)
        case Awaiting =>
          line.trim() match {
            case `start`              => loop(lines, example.next, Reading(lineNo + 1, line.indexOf(start), Nil), reverseResult)
            case sectionName(section) => loop(lines, Example(section.trim()), Awaiting, reverseResult) 
            case _                    => loop(lines, example, Awaiting, reverseResult)
          }
      }
      case Nil => reverseResult.reverse
    }

    loop(markdown.content.zipWithIndex, Example(""), Awaiting, Nil)
  }
}

case class Markdown(name: String, content: List[String]) {

  def extractAll: List[Snippet] = Snippet.extractAll(this)
}
object Markdown {

  def readAllInDir(dir: File): List[Markdown] =
    for {
      files <- Option(dir.listFiles()).toList
      markdownFile <- files.sortBy(_.getName()) if markdownFile.getAbsolutePath().endsWith(".md")
    } yield 
      Using(io.Source.fromFile(markdownFile)) { src =>
        val name = markdownFile.getName() 
        Markdown(name.substring(0, name.length() - ".md".length()), src.getLines().toList)
      }.get
}

// program

@main // scala-cli run test-examples.scala -- "../docs/docs"
def testExamples(path: String): Unit = {
  extension (s: StringContext)
    def hl(args: Any*): String = s"$MAGENTA${s.s(args*)}$RESET"

  val docsDir = File(path)
  println(hl"Started reading from ${docsDir.getAbsolutePath()}")
  val markdowns = Markdown.readAllInDir(docsDir)
  println(hl"Read files: ${markdowns.map(_.name)}")
  val snippets = markdowns.flatMap(_.extractAll)
  println(hl"Found snippets" + ":\n" + snippets.map(s => hl"${s.hint} (${s.name})").mkString("\n") + "\n")
  val (ignoredSnippets, testedSnippets) = snippets.partition(_.isIgnored)
  println(hl"Ignoring snippets" + ":\n" + ignoredSnippets.map(s => hl"${s.hint} (${s.name})").mkString("\n") + "\n")
  val ignoredNotFound = ignored.filter(i => snippets.exists(_.name == i)).toList.sorted
  if (ignoredNotFound.nonEmpty) {
    println(hl"Some ignored snippets have been moved, their indices changed and cannot be matched" + ":\n" + ignoredNotFound.mkString("\n"))
    sys.exit(1)
  }
  val saved = testedSnippets.foreach(_.save())
  val failed = testedSnippets.flatMap { snippet =>
    import snippet.{hint, name, snippetDir}
    println(hl"Testing: $hint ($name, saved in $snippetDir)" + ":")
    try {
      s"scala-cli run '$snippetDir'".!!
      List.empty[String]
    } catch {
      case _: Throwable => List(s"$hint ($name)")
    }
  }
  if (failed.nonEmpty) {
    println(hl"Failed snippets (${failed.length}/${testedSnippets.length})" + s":\n${failed.mkString("\n")}")
    println(hl"Fix them or add to ignored list (name in parenthesis is less subject to change)")
    sys.exit(1)
  } else {
    println(hl"All snippets (${testedSnippets.length}) run succesfully!")
  }
}
