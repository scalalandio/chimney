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

trait Runner:

  def docsDir: File
  def tmpDir: File
  def filter: Option[String]

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

    def howToRun: Runner.Strategy

    def isIgnored: Boolean = howToRun match
      case Runner.Strategy.Ignore(_) => true
      case _                         => false

object Runner:
  enum Strategy:
    case ExpectSuccess
    case ExpectErrors(errors: List[String])
    case Ignore(cause: String)

  class Default(val docsDir: File, val tmpDir: File, val filter: Option[String]) extends Runner:

    private val filterPattern = filter.map(f => Regex.quote(f).replaceAll("[*]", raw"\\E.*\\Q").r)

    private def extractErrors(content: String): List[String] = {
      enum State:
        case ReadingErrMsg(current: Vector[String])
        case Skipping
      val errorStart = raw"\s*// expected error:\s*".r
      val comment = raw"\s*// (.+)".r
      content
        .split("\n")
        .foldLeft((State.Skipping: State) -> Vector.empty[String]) {
          case ((State.ReadingErrMsg(currentErrorMsg), allErrorMsgs), comment(content)) =>
            State.ReadingErrMsg(currentErrorMsg :+ content) -> allErrorMsgs
          case ((State.ReadingErrMsg(currentErrorMsg), allErrorMsgs), _) =>
            State.Skipping -> (allErrorMsgs :+ currentErrorMsg.mkString("\n"))
          case ((State.Skipping, allErrMsgs), errorStart()) =>
            State.ReadingErrMsg(Vector.empty) -> allErrMsgs
          case ((State.Skipping, allErrMsgs), _) =>
            State.Skipping -> allErrMsgs
        }
        .match {
          case (State.ReadingErrMsg(currentErrorMsg), allErrMsgs) => allErrMsgs :+ (currentErrorMsg.mkString("\n"))
          case (State.Skipping, allErrorMsgs)                     => allErrorMsgs
        }
        .toList
    }

    extension (snippet: Snippet)
      def adjusted: Snippet = snippet
      def howToRun: Strategy =
        // simple filtering
        if filterPattern.exists(p => !p.matches(snippet.stableName)) then Strategy.Ignore("filtered out")
        // for simplicity: we're assuming that each actual example should have //> using dep with some library
        else if !snippet.content.contains("//> using dep") then Strategy.Ignore("pseudocode")
        // for simplicity: we're assuming that only sbt examples have libraryDependencies
        else if snippet.content.contains("libraryDependencies") then Strategy.Ignore("sbt example")
        // for simplicity: we're assuming that errors are defined in inline comments starting with '// expected error:'
        else if snippet.content.contains("// expected error:") then
          Strategy.ExpectErrors(extractErrors(snippet.content))
        else Strategy.ExpectSuccess

case class Suite(name: String, snippets: List[Snippet]) {

  def run(using Runner): Suite.Result = {
    val (failed, successfulOrIgnored) = snippets.partitionMap { snippet =>
      println(hl"$name" + ":")
      println()
      import snippet.{hint, stableName}
      snippet.howToRun match {
        case Runner.Strategy.ExpectSuccess =>
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
        case Runner.Strategy.ExpectErrors(errors) =>
          // TODO
          println(yellow"Snippet $stableName ($hint) was ignored - FIXME")
          Right(Some(snippet))
        case Runner.Strategy.Ignore(cause) =>
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
    filter: Option[String],
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

val runTestSnippets: Runner ?=> Unit = {
  println(
    hl"Testing with docs in ${summon[Runner].docsDir}, snippets extracted to: tmp=${summon[Runner].tmpDir}"
  )
  println(hl"Started reading from ${summon[Runner].docsDir.getAbsolutePath()}")
  println()
  val markdowns = Markdown.readAllInDir(summon[Runner].docsDir)
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

def testSnippets(args: Array[String])(f: TestConfig => Runner): Unit =
  TestConfig.parse(args) match {
    case Right(cfg) => runTestSnippets(using f(cfg))
    case Left(help) => println(help); sys.exit(1)
  }
