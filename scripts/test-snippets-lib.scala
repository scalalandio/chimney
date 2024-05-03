//> using scala 3.3.3

import java.io.File
import java.nio.file.Files
import scala.Console.{GREEN, MAGENTA, RED, RESET, YELLOW}
import scala.util.matching.Regex
import scala.util.Using
import scala.sys.process.*

extension (s: StringContext)
  def hl(args: Any*): String = s"$MAGENTA${s.s(args*)}$RESET"
  def red(args: Any*): String = s"$RED${s.s(args*)}$RESET"
  def green(args: Any*): String = s"$GREEN${s.s(args*)}$RESET"
  def yellow(args: Any*): String = s"$YELLOW${s.s(args*)}$RESET"

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

trait SnippetRunner:

  def docsDir: File
  def tmpDir: File

  extension (snippet: Snippet)
    def save(): File = {
      val snippetFile: File = File(s"${tmpDir.getPath()}/${snippet.name}/snippet.sc")
      snippetFile.getParentFile().mkdirs()
      Files.writeString(snippetFile.toPath(), snippet.content)
      snippetFile
    }

    def run(): Unit = {
      val snippetDir = File(s"${tmpDir.getPath()}/${snippet.name}/snippet.sc").getParent()
      s"scala-cli run '$snippetDir'".!!
    }

    def adjusted: Snippet
    def howToRun: SnippetStrategy
    def isIgnored: Boolean = howToRun match
      case SnippetStrategy.Ignore(_) => true
      case _                         => false

case class Suite(name: String, snippets: List[Snippet]) {

  def run(using SnippetRunner): Suite.Result = {
    println(hl"$name" + ":")
    val (failed, successfulOrIgnored) = snippets.partitionMap { snippet =>
      println()
      import snippet.{hint, name as stableName}
      snippet.howToRun match {
        case SnippetStrategy.ExpectSuccess =>
          val snippetDir = snippet.save()
          println(hl"Snippet: $hint (stable name: $stableName) saved in $snippetDir, testing" + ":\n" + snippet.content)
          try {
            snippet.run()
            println(green"Snippet: $hint (stable name: $stableName) succeeded")
            Right(None)
          } catch {
            case _: Throwable =>
              println(red"Snippet: $hint (stable name: $stableName) failed")
              Left(snippet)
          }
        case SnippetStrategy.ExpectErrors(errors) =>
          // TODO
          println(yellow"Snippet $hint (stable name: $stableName) was ignored - FIXME")
          Right(Some(snippet))
        case SnippetStrategy.Ignore(cause) =>
          println(yellow"Snippet $hint (stable name: $stableName) was ignored ($cause)")
          Right(Some(snippet))
      }
    }
    val ignored = successfulOrIgnored.collect { case Some(snippet) => snippet }
    val succeed = snippets.filterNot(failed.contains).filterNot(ignored.contains)
    if failed.nonEmpty then {
      println(
        red"Results: ${succeed.size} succeed, ${ignored.length} ignored, ${failed.length} failed - some snippets failed:"
      )
      failed.foreach(s => println(red"  ${s.name}"))
      println()
    } else {
      println(green"Results: ${succeed.size} succeed, ${ignored.length} ignored - all snippets passed")
      println()
    }
    Suite.Result(suiteName = name, succeed = succeed, failed = failed, ignored = ignored)
  }
}
object Suite {
  case class Result(suiteName: String, succeed: List[Snippet], failed: List[Snippet], ignored: List[Snippet])
}

// program

def testSnippets()(using SnippetRunner): Unit = {
  println(
    hl"Testing with docs in ${summon[SnippetRunner].docsDir}, snippets extracted to: tmp=${summon[SnippetRunner].tmpDir}"
  )
  println(hl"Started reading from ${summon[SnippetRunner].docsDir.getAbsolutePath()}")
  println()
  val markdowns = Markdown.readAllInDir(summon[SnippetRunner].docsDir)
  println(hl"Read files: ${markdowns.map(_.name)}")
  println()
  val suites = markdowns.map { markdown =>
    Suite(markdown.name, markdown.extractAll.map(_.adjusted))
  }
  val (failed, succeed) = suites.map(_.run).partition(_.failed.nonEmpty)
  println()
  if failed.nonEmpty then {
    println(red"Failed suites:")
    failed.foreach(r => println(red"  ${r.suiteName}"))
    println(red"Fix them or add to ignored list (name in parenthesis is less subject to change)")
    sys.exit(1)
  } else {
    println(green"All suites run succesfully!")
  }
}
