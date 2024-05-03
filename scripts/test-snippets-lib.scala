//> using scala 3.3.3
//> using dep "com.monovore::decline:2.4.1"

import java.io.File
import java.nio.file.{Files, Path}
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

case class Snippet(markdownName: String, lineNo: Int, section: String, ordinal: Int, content: String) {
  lazy val fileName: String =
    s"${markdownName}_${section}_$ordinal".replaceAll(" +", "-").replaceAll("[^A-Za-z0-9_-]+", "")
  lazy val stableName: String = s"$markdownName.md#$section[$ordinal]"
  lazy val hint: String = s"$markdownName.md:$lineNo"
}
object Snippet {

  def extractAll(markdown: Markdown): List[Snippet] = {
    val name = markdown.name

    case class Example(section: String, lineNo: Int = 1, ordinal: Int = 0) {

      def next(lineNo: Int): Example = copy(lineNo = lineNo, ordinal = ordinal + 1)

      def toSnippet(content: String): Snippet = Snippet(
        markdownName = markdown.name,
        lineNo = lineNo,
        section = section,
        ordinal = ordinal,
        content = content
      )
    }

    enum Mode:
      case Reading(indent: Int, content: Vector[String])
      case Awaiting

    import Mode.*

    val start = "```scala"
    val end = "```"
    val sectionName = "#+(.+)".r

    def loop(content: List[(String, Int)], example: Example, mode: Mode, result: Vector[Snippet]): List[Snippet] =
      (content, mode) match {
        // ``` terminates snippet reading
        case ((line, _) :: lines, Reading(indent, content)) if line.trim() == end =>
          loop(
            lines,
            example,
            Awaiting,
            result :+ example.toSnippet(content.mkString("\n"))
          )
        // ``` not reached, we're stil lreading snippet
        case ((line, _) :: lines, Reading(indent, content)) =>
          loop(
            lines,
            example,
            Reading(indent, content :+ (if line.length() > indent then line.substring(indent) else line)),
            result
          )
        // ```scala found, we're reading snippet starting from the next line
        case ((line, lineNo) :: lines, Awaiting) if line.trim() == start =>
          loop(lines, example.next(lineNo + 1), Reading(line.indexOf(start), Vector.empty), result)
        // # section name
        case ((sectionName(section), lineNo) :: lines, Awaiting) =>
          loop(lines, Example(section.trim(), lineNo), Awaiting, result)
        // not reading snippet. skipping over this line
        case ((line, lineNo) :: lines, Awaiting) =>
          loop(lines, example, Awaiting, result)
        // end of document reached, all snippets found
        case (Nil, _) => result.toList
      }

    loop(markdown.content.zipWithIndex, Example(""), Awaiting, Vector.empty)
  }
}

enum SnippetStrategy:
  case ExpectSuccess
  case ExpectErrors(errors: List[String])
  case Ignore(cause: String)

trait SnippetRunner:

  def docsDir: File
  def tmpDir: File

  extension (snippet: Snippet)
    def save(): File = {
      val snippetFile: File = File(s"${tmpDir.getPath()}/${snippet.fileName}/snippet.sc")
      snippetFile.getParentFile().mkdirs()
      Files.writeString(snippetFile.toPath(), snippet.content)
      snippetFile
    }

    def run(): Unit = {
      val snippetDir = File(s"${tmpDir.getPath()}/${snippet.fileName}/snippet.sc").getParent()
      s"scala-cli run '$snippetDir'".!!
    }

    def adjusted: Snippet
    def howToRun: SnippetStrategy
    def isIgnored: Boolean = howToRun match
      case SnippetStrategy.Ignore(_) => true
      case _                         => false

case class Suite(name: String, snippets: List[Snippet]) {

  def run(using SnippetRunner): Suite.Result = {
    val (failed, successfulOrIgnored) = snippets.partitionMap { snippet =>
      println(hl"$name" + ":")
      println()
      import snippet.{hint, stableName}
      snippet.howToRun match {
        case SnippetStrategy.ExpectSuccess =>
          val snippetDir = snippet.save()
          println(hl"Snippet $stableName ($hint) saved in $snippetDir, testing" + ":\n" + snippet.content)
          try {
            snippet.run()
            println(green"Snippet $stableName ($hint) succeeded")
            Right(None)
          } catch {
            case _: Throwable =>
              println(red"Snippet $stableName ($hint) failed")
              Left(snippet)
          }
        case SnippetStrategy.ExpectErrors(errors) =>
          // TODO
          println(yellow"Snippet $stableName ($hint) was ignored - FIXME")
          Right(Some(snippet))
        case SnippetStrategy.Ignore(cause) =>
          println(yellow"Snippet $stableName ($hint) was ignored ($cause)")
          Right(Some(snippet))
      }
    }
    val ignored = successfulOrIgnored.collect { case Some(snippet) => snippet }
    val succeed = snippets.filterNot(failed.contains).filterNot(ignored.contains)
    if failed.nonEmpty then {
      println(
        red"Results: ${succeed.size} succeed, ${ignored.length} ignored, ${failed.length} failed - some snippets failed:"
      )
      failed.foreach(s => println(red"  ${s.stableName} (${s.hint})}"))
      println()
    } else {}
    Suite.Result(suiteName = name, succeed = succeed, failed = failed, ignored = ignored)
  }
}
object Suite {
  case class Result(suiteName: String, succeed: List[Snippet], failed: List[Snippet], ignored: List[Snippet])
}

// program

case class TestConfig(
    docsDir: File,
    tmpDir: File,
    filter: Option[String], // TODO: use it
    extra: Map[String, String]
)
object TestConfig {
  import com.monovore.decline.*

  val defn = Command("test-snippets", "Turn Scala snippets in Mkardown files into test suites", helpFlag = true) {
    import cats.data.{Validated, ValidatedNel}
    import cats.implicits.*

    given Argument[(String, String)] with
      def read(string: String): ValidatedNel[String, (String, String)] =
        string.split("=").toList match {
          case key :: value :: Nil => Validated.valid(key -> value)
          case _                   => Validated.invalidNel(s"Expected pair, got: $string")
        }
      def defaultMetavar: String = "<key>=<value>"

    (
      Opts.argument[Path](metavar = "docs"),
      Opts.argument[Path](metavar = "tmp").orNone,
      Opts.option[String](long = "filter", short = "f", help = "Run only tests matching filter").orNone,
      Opts.options[(String, String)](long = "extra", help = "").orNone
    ).mapN { (docs, tmpOpt, filter, extras) =>
      TestConfig(
        docsDir = docs.toFile,
        tmpDir = tmpOpt.map(_.toFile).getOrElse(Files.createTempDirectory(s"docs-snippets").toFile()),
        filter = filter,
        extra = extras.map(_.toList.toMap).getOrElse(Map.empty)
      )
    }
  }

  def parse(args: Array[String]): Either[Help, TestConfig] = defn.parse(args = args, env = sys.env)
}

val runTestSnippets: SnippetRunner ?=> Unit = {
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
    println(green"All snippets run succesfully!")
  }
}

def testSnippets(args: Array[String])(f: TestConfig => SnippetRunner): Unit =
  TestConfig.parse(args) match {
    case Right(cfg) => runTestSnippets(using f(cfg))
    case Left(help) => println(help); sys.exit(1)
  }
