//> using scala 3.3.3
//> using dep "com.monovore::decline:2.4.1"

import java.io.File
import java.nio.file.{Files, Path}
import scala.Console.{GREEN, MAGENTA, RED, RESET, YELLOW}
import scala.util.Using
import scala.util.matching.Regex

import java.io.OutputStream

extension (s: StringContext)
  def hl(args: Any*): String = s"$MAGENTA${s.s(args*)}$RESET"
  def red(args: Any*): String = s"$RED${s.s(args*)}$RESET"
  def green(args: Any*): String = s"$GREEN${s.s(args*)}$RESET"
  def yellow(args: Any*): String = s"$YELLOW${s.s(args*)}$RESET"

case class RunResult(
    exitCode: Int,
    out: String,
    err: String,
    outErr: String
)
object RunResult {

  import java.io.{ByteArrayOutputStream, FilterOutputStream, OutputStream}
  import scala.sys.process.*

  class Broadcast(out1: OutputStream, out2: OutputStream) extends OutputStream {

    def write(b: Int): Unit = {
      out1.write(b)
      out2.write(b)
    }
  }

  class FilterANSIConsole(out: OutputStream) extends FilterOutputStream(out) {
    private enum State:
      case NoAnsi
      case Esc
      case LeftBracket
      case Digit

    private var state = State.NoAnsi

    override def write(b: Int): Unit = state match {
      case State.NoAnsi if b == 0x1b             => state = State.Esc
      case State.NoAnsi                          => super.write(b)
      case State.Esc if b.toChar == '['          => state = State.LeftBracket
      case State.LeftBracket if b.toChar.isDigit => state = State.Digit
      case State.Digit if b.toChar.isDigit       => ()
      case State.Digit if b.toChar == 'm'        => state = State.NoAnsi
      case _                                     => throw new AssertionError(s"Unexpected character: $b (${b.toChar})")
    }
  }

  extension (os: OutputStream)
    def &&(os2: OutputStream): OutputStream = Broadcast(os, os2)
    def noAnsiConsole: OutputStream = FilterANSIConsole(os)

  def apply(str: String*): RunResult =
    Using.Manager { use =>
      val sb = new StringBuffer
      val out = use(ByteArrayOutputStream())
      val err = use(ByteArrayOutputStream())
      val outErr = use(ByteArrayOutputStream())
      val exitValue: Int = str
        .run(
          BasicIO(withIn = false, buffer = sb, log = None)
            .withOutput(in => in.transferTo((out && outErr).noAnsiConsole && sys.process.stdout))
            .withError(in => in.transferTo((err && outErr).noAnsiConsole && sys.process.stderr))
        )
        .exitValue()
      RunResult(exitValue.toInt, out.toString, err.toString, outErr.toString)
    }.get
}

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
    (if section.isEmpty then s"${markdownName}_$ordinal" else s"${markdownName}_${section}_$ordinal")
      .replaceAll(" +", "-")
      .replaceAll("[^A-Za-z0-9_-]+", "")
  lazy val stableName: String =
    if section.isEmpty then s"$markdownName.md[$ordinal]" else s"$markdownName.md#$section[$ordinal]"
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

  private lazy val filterPattern = filter.map(f => Regex.quote(f).replaceAll("[*]", raw"\\E.*\\Q").r)

  extension (snippet: Snippet)
    def save(): File = {
      val snippetFile: File = File(s"${tmpDir.getPath()}/${snippet.fileName}/snippet.sc")
      snippetFile.getParentFile().mkdirs()
      Files.writeString(snippetFile.toPath(), snippet.content)
      snippetFile
    }

    def run(): RunResult =
      RunResult("scala-cli", "run", File(s"${tmpDir.getPath()}/${snippet.fileName}/snippet.sc").getParent())

    def adjusted: Snippet

    def howToRun: Runner.Strategy

    def isIgnored: Boolean = howToRun match
      case Runner.Strategy.Ignore(_) => true
      case _                         => false

    def isTested: Boolean = filterPattern.forall(_.matches(snippet.stableName))

object Runner:
  enum Strategy:
    case ExpectSuccess
    case ExpectErrors(errors: List[String])
    case Ignore(cause: String)

  class Default(val docsDir: File, val tmpDir: File, val filter: Option[String]) extends Runner:

    def this(cfg: TestConfig) = this(cfg.docsDir, cfg.tmpDir, cfg.filter)

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
        // for simplicity: we're assuming that each actual example should have //> using dep with some library
        if !snippet.content.contains("//> using dep") then Strategy.Ignore("pseudocode")
        // for simplicity: we're assuming that only sbt examples have libraryDependencies
        else if snippet.content.contains("libraryDependencies") then Strategy.Ignore("sbt example")
        // for simplicity: we're assuming that errors are defined in inline comments starting with '// expected error:'
        else if snippet.content.contains("// expected error:") then
          Strategy.ExpectErrors(extractErrors(snippet.content))
        else Strategy.ExpectSuccess

case class Suite(name: String, snippets: List[Snippet]) {

  def run(using Runner): Suite.Result =
    if snippets.exists(_.isTested) then {
      println(hl"$name" + ":")
      val (failed, successfulOrIgnored) = snippets.filter(_.isTested).partitionMap { snippet =>
        println()
        import snippet.{hint, stableName}
        snippet.howToRun match {
          case Runner.Strategy.ExpectSuccess =>
            val snippetDir = snippet.save()
            println(hl"Snippet $stableName ($hint) saved in $snippetDir, testing" + ":\n" + snippet.content)
            val RunResult(exitCode, out, err, outErr) = snippet.run()
            if exitCode == 0 then
              println(green"Snippet $stableName ($hint) succeeded")
              Right(None)
            else
              println(red"Snippet $stableName ($hint) failed")
              Left(snippet)
          case Runner.Strategy.ExpectErrors(errors) =>
            val snippetDir = snippet.save()
            println(hl"Snippet $stableName ($hint) saved in $snippetDir, testing" + ":\n" + snippet.content)
            val RunResult(exitCode, out, err, outErr) = snippet.run()
            val sanitized =
              err.replaceAll(raw"snippet\.this\.", "").replaceAll(raw"snippet\.", "").replaceAll(raw"\[error\] ", "")
            lazy val unmatched = errors.filterNot(error => sanitized.contains(error.trim))
            if exitCode == 0 then
              println(red"Snippet $stableName ($hint) should have produced error(s)")
              Left(snippet)
            else if errors.nonEmpty && unmatched.nonEmpty then
              println(red"Snippet $stableName ($hint) shoule have produced errors:" + "\n" + unmatched.mkString("\n"))
              println(red"got:" + "\n" + sanitized)
              Left(snippet)
            else
              println(green"Snippet $stableName ($hint) failed as expected")
              Right(None)
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
      } else {
        println(
          green"Results: ${succeed.size} succeed, ${ignored.length} ignored, all snippets succeeded"
        )
        println()
      }
      Suite.Result(suiteName = name, succeed = succeed, failed = failed, ignored = ignored)
    } else Suite.Result(suiteName = name, succeed = List.empty, failed = List.empty, ignored = List.empty)
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

  val defn = Command("test-snippets", "Turn Scala snippets in Markdown files into test suites", helpFlag = true) {
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
      Opts.option[String](long = "test-only", short = "f", help = "Run only tests matching filter").orNone,
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
    println(red"Fix them or add to ignored list")
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
