//> using scala 3.3.3

import java.io.File
import java.nio.file.Files
import scala.Console.{GREEN, MAGENTA, RED, RESET, YELLOW}
import scala.util.matching.Regex
import scala.util.Using
import scala.sys.process.*

// models

case class Markdown(name: String, content: List[String]) {

  def extractAll: List[Snippet] = Snippet.extractAll(this)
}
object Markdown {

  def readAllInDir(dir: File): List[Markdown] =
    for {
      files <- Option(dir.listFiles()).toList
      markdownFile <- files.sortBy(_.getName()) if markdownFile.getAbsolutePath().endsWith(".md")
    } yield Using(io.Source.fromFile(markdownFile)) { src =>
      val name = markdownFile.getName()
      Markdown(name.substring(0, name.length() - ".md".length()), src.getLines().toList)
    }.get
}

case class Snippet(name: String, hint: String, content: String) {

  def save(tmpDir: File): File = {
    val snippetFile: File = File(s"${tmpDir.getPath()}/$name/snippet.sc")
    snippetFile.getParentFile().mkdirs()
    Files.writeString(snippetFile.toPath(), content)
    snippetFile
  }

  def run(tmpDir: File): Unit = {
    val snippetDir = File(s"${tmpDir.getPath()}/$name/snippet.sc").getParent()
    s"scala-cli run '$snippetDir'".!!
  }
}
object Snippet {

  def extractAll(markdown: Markdown): List[Snippet] = {
    val name = markdown.name

    case class Example(section: String, ordinal: Int = 0) {

      def next: Example = copy(ordinal = ordinal + 1)

      def toName: String = s"${name}_${section}_$ordinal".replaceAll(" +", "-").replaceAll("[^A-Za-z0-9_-]+", "")
    }

    enum Mode:
      case Reading(lineNo: Int, indent: Int, contentReverse: List[String])
      case Awaiting

    import Mode.*

    val start = "```scala"
    val end = "```"
    val sectionName = "#+(.+)".r

    def adjustLine(line: String, indent: Int): String =
      if line.length() > indent then line.substring(indent) else line

    def mkSnippet(example: Example, lineNo: Int, contentReverse: List[String]): Snippet =
      Snippet(example.toName, s"$name.md:$lineNo", contentReverse.reverse.mkString("\n"))

    def loop(content: List[(String, Int)], example: Example, mode: Mode, reverseResult: List[Snippet]): List[Snippet] =
      content match {
        case (line, lineNo) :: lines =>
          mode match {
            case Reading(lineNo, indent, contentReverse) =>
              if line.trim() == end then
                loop(lines, example, Awaiting, mkSnippet(example, lineNo, contentReverse) :: reverseResult)
              else
                loop(lines, example, Reading(lineNo, indent, adjustLine(line, indent) :: contentReverse), reverseResult)
            case Awaiting =>
              line.trim() match {
                case `start` => loop(lines, example.next, Reading(lineNo + 1, line.indexOf(start), Nil), reverseResult)
                case sectionName(section) => loop(lines, Example(section.trim()), Awaiting, reverseResult)
                case _                    => loop(lines, example, Awaiting, reverseResult)
              }
          }
        case Nil => reverseResult.reverse
      }

    loop(markdown.content.zipWithIndex, Example(""), Awaiting, Nil)
  }
}

enum SnippetStrategy:
  case ExpectSuccess
  case ExpectErrors(errors: List[String]) // TODO
  case Ignore(cause: String)

trait SnippetExtension:

  extension (snippet: Snippet)
    def adjusted: Snippet
    def howToRun: SnippetStrategy
    def isIgnored: Boolean = howToRun match
      case SnippetStrategy.Ignore(_) => true
      case _                         => false

// program

extension (s: StringContext)
  def hl(args: Any*): String = s"$MAGENTA${s.s(args*)}$RESET"
  def red(args: Any*): String = s"$RED${s.s(args*)}$RESET"
  def green(args: Any*): String = s"$GREEN${s.s(args*)}$RESET"
  def yellow(args: Any*): String = s"$YELLOW${s.s(args*)}$RESET"

def testSnippets(
    docsDir: File,
    tmpDir: File,
    snippetsDrop: Int,
    snippetsTake: Int
)(using SnippetExtension): Unit = {
  println(hl"Testing with docs in $docsDir, snippets extracted to: tmp=$tmpDir")
  println(hl"Started reading from ${docsDir.getAbsolutePath()}")
  println()
  val markdowns = Markdown.readAllInDir(docsDir)
  println(hl"Read files: ${markdowns.map(_.name)}")
  println()
  val snippets = markdowns.flatMap(_.extractAll).drop(snippetsDrop).take(snippetsTake).map(_.adjusted)
  println(
    hl"Found snippets" + ":\n" + snippets.map(s => hl"\n${s.hint} (${s.name})" + ":\n" + s.content).mkString("\n")
  )
  println()
  val (ignoredSnippets, testedSnippets) = snippets.partition(_.isIgnored)
  println(hl"Ignoring snippets" + ":\n" + ignoredSnippets.map(s => hl"${s.hint} (${s.name})").mkString("\n"))
  println()
  /*
  val ignoredNotFound = ignored.filterNot(i => snippets.exists(_.name == i)).toList.sorted
  if ignoredNotFound.nonEmpty && providedSnippetsDrop == -1 && providedSnippetsTake == -1 then {
    println(
      hl"Some ignored snippets have been moved, their indices changed and cannot be matched" + ":\n" + ignoredNotFound
        .mkString("\n")
    )
    sys.exit(1)
  }
   */
  val failed = snippets.flatMap { snippet =>
    println()
    import snippet.{hint, name}
    snippet.howToRun match {
      case SnippetStrategy.ExpectSuccess =>
        val snippetDir = snippet.save(tmpDir)
        println(hl"Snippet: $hint (stable name: $name) saved in $snippetDir, testing" + ":\n" + snippet.content)
        try {
          snippet.run(tmpDir)
          println(green"Snippet: $hint (stable name: $name) succeeded")
          List.empty[String]
        } catch {
          case _: Throwable =>
            println(red"Snippet: $hint (stable name: $name) failed")
            List(s"$hint (stable name: $name)")
        }
      case SnippetStrategy.ExpectErrors(errors) =>
        // TODO
        println(yellow"Snippet $hint (stable name: $name) was ignored - FIXME")
        List.empty[String]
      case SnippetStrategy.Ignore(cause) =>
        println(yellow"Snippet $hint (stable name: $name) was ignored ($cause)")
        List.empty[String]
    }
  }

  println()
  if failed.nonEmpty then {
    println(
      red"Failed snippets (${failed.length}/${testedSnippets.length}, ignored: ${ignoredSnippets.length})" + s":\n${failed
          .mkString("\n")}"
    )
    println(red"Fix them or add to ignored list (name in parenthesis is less subject to change)")
    sys.exit(1)
  } else {
    println(green"All snippets (${testedSnippets.length}, ignored: ${ignoredSnippets.length}) run succesfully!")
  }
}
