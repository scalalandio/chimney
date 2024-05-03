//> using dep org.virtuslab::scala-yaml:0.0.8

import java.io.File
import java.nio.file.Files
import scala.collection.immutable.ListMap
import scala.util.{Try, Using}
import scala.util.chaining.*

// Chimney-specific configuration

case class MkDocsConfig(extra: Map[String, String])
object MkDocsConfig {

  def parse(cfgFile: File): Either[String, MkDocsConfig] = {
    import org.virtuslab.yaml.*
    def decode(any: Any): Map[String, String] = any match {
      case map: Map[?, ?] =>
        map.flatMap {
          case (k, v: Map[?, ?]) => decode(v).map { case (k2, v2) => s"$k.$k2" -> v2 }
          case (k, v: List[?])   => decode(v).map { case (k2, v2) => s"$k.$k2" -> v2 }
          case (k, v)            => Map(k.toString -> v.toString)
        }.toMap
      case list: List[?] =>
        list.zipWithIndex.flatMap {
          case (i: Map[?, ?], idx) => decode(i).map { case (k, v) => s"[$idx].$k" -> v }
          case (i: List[?], idx)   => decode(i).map { case (k, v) => s"[$idx].$k" -> v }
          case (i, idx)            => Map(s"[$idx]" -> i.toString)
        }.toMap
      case _ => throw new IllegalArgumentException(s"$any is not an expected YAML")
    }
    for {
      cfgStr <- Using(io.Source.fromFile(cfgFile))(_.getLines().toList.mkString("\n")).toEither.left
        .map(_.getMessage())
      cfgRaw <- cfgStr.as[Any].left.map(_.toString)
      extra <- Try(decode(cfgRaw.asInstanceOf[Map[Any, Any]].apply("extra"))).toEither.left.map(_.getMessage)
    } yield MkDocsConfig(extra)
  }
}

enum SpecialHandling:
  case NeedManual(reason: String)

val specialHandling: ListMap[String, SpecialHandling] = ListMap(
  "index__2" -> SpecialHandling.NeedManual("landing page"),
  "index__3" -> SpecialHandling.NeedManual("landing page"),
  "index__4" -> SpecialHandling.NeedManual("landing page"),
  "index__5" -> SpecialHandling.NeedManual("landing page"),
  "index__6" -> SpecialHandling.NeedManual("landing page"),
  "supported-transformations_Between-sealedenums_2" -> SpecialHandling.NeedManual(
    "snippet fails!!! investigate later"
  ), // FIXME
  "supported-transformations_Between-sealedenums_3" -> SpecialHandling.NeedManual(
    "snippet throws exception!!! investigate later"
  ), // FIXME
  "supported-transformations_Between-sealedenums_4" -> SpecialHandling.NeedManual(
    "snippet throws exception!!! investigate later"
  ), // FIXME
  "supported-transformations_Javas-enums_1" -> SpecialHandling.NeedManual("requires previous snipper with Java code"),
  "supported-transformations_Javas-enums_2" -> SpecialHandling.NeedManual("requires previous snipper with Java code"),
  "supported-transformations_Handling-a-specific-sealed-subtype-with-a-computed-value_3" -> SpecialHandling
    .NeedManual(
      "snippet throws exception!!! investigate later"
    ), // FIXME
  "supported-transformations_Handling-a-specific-sealed-subtype-with-a-computed-value_4" -> SpecialHandling
    .NeedManual(
      "requires previous snipper with Java code"
    ),
  "supported-transformations_Handling-a-specific-sealed-subtype-with-a-computed-value_5" -> SpecialHandling
    .NeedManual(
      "requires previous snipper with Java code"
    ),
  "supported-transformations_Handling-a-specific-sealed-subtype-with-a-computed-value_6" -> SpecialHandling
    .NeedManual(
      "requires previous snipper with Java code"
    ),
  "supported-transformations_Types-with-manually-provided-constructors_3" -> SpecialHandling.NeedManual(
    "example split into multiple files"
  ),
  "supported-transformations_Types-with-manually-provided-constructors_4" -> SpecialHandling.NeedManual(
    "contunuation from the previous snippet"
  ),
  "supported-transformations_Types-with-manually-provided-constructors_5" -> SpecialHandling.NeedManual(
    "example split into multiple files"
  ),
  "supported-transformations_Types-with-manually-provided-constructors_6" -> SpecialHandling.NeedManual(
    "contunuation from the previous snippet"
  ),
  "supported-transformations_Defining-custom-name-matching-predicate_1" -> SpecialHandling.NeedManual(
    "example split into multiple files"
  ),
  "supported-transformations_Defining-custom-name-matching-predicate_2" -> SpecialHandling.NeedManual(
    "contunuation from the previous snippet"
  ),
  "troubleshooting_Ducktape_2" -> SpecialHandling.NeedManual(
    "snippet throws exception!!! investigate later"
  ), // FIXME
  "troubleshooting_Ducktape_4" -> SpecialHandling.NeedManual(
    "snippet throws exception!!! investigate later"
  ), // FIXME
  "troubleshooting_Ducktape_8" -> SpecialHandling.NeedManual(
    "snippet throws exception!!! investigate later"
  ), // FIXME
  "troubleshooting_Ducktape_10" -> SpecialHandling.NeedManual(
    "snippet throws exception!!! investigate later"
  ) // FIXME
)

class ChimneySpecific(
    chimneyVersion: String,
    mkDocsCfg: MkDocsConfig,
    val docsDir: File,
    val tmpDir: File
) extends SnippetRunner {

  private val defaultScalaVersion = "2.13.13"

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

  private val replacePatterns = (mkDocsCfg.extra + (raw"chimney_version\(\)" -> chimneyVersion)).map { case (k, v) =>
    (raw"\{\{\s*" + k + raw"\s*\}\}") -> v
  }

  extension (snippet: Snippet)
    def adjusted: Snippet =
      snippet.copy(content =
        replacePatterns.foldLeft(
          if snippet.content.contains("//> using scala") then snippet.content
          else s"//> using scala $defaultScalaVersion\n${snippet.content}"
        ) { case (s, (k, v)) => s.replaceAll(k, v) }
      )

    def howToRun: SnippetStrategy = specialHandling.get(snippet.fileName) match
      case None =>
        // for simplicity: we're assuming that each actual example should have //> using dep with some library
        if !snippet.content.contains("//> using dep") then SnippetStrategy.Ignore("pseudocode")
        // for simplicity: we're assuming that only sbt examples have libraryDependencies
        else if snippet.content.contains("libraryDependencies") then SnippetStrategy.Ignore("sbt example")
        // for simplicity: we're assuming that errors are defined in inline comments starting with '// expected error:'
        else if snippet.content.contains("// expected error:") then
          SnippetStrategy.ExpectErrors(extractErrors(snippet.content))
        else SnippetStrategy.ExpectSuccess
      case Some(SpecialHandling.NeedManual(reason)) => SnippetStrategy.Ignore(reason)
}

/** Usage:
  *
  * From the project root (if called from other directory, adapt path after PWD accordingly):
  *
  * on CI:
  * {{{
  * # run all tests, use artifacts published locally from current tag
  * scala-cli run scripts/test-snippets.scala scripts/test-snippets-lib.scala -- --extra "chimney-version=$(sbt -batch -error 'print chimney/version')" "$PWD/docs/docs"
  * }}}
  *
  * during development:
  * {{{
  * # fix: version to use, tmp directory
  * scala-cli run scripts/test-snippets.scala scripts/test-snippets-lib.scala -- --extra "chimney-version=1.0.0-RC1" --filter "Supported Transformations" "$PWD/docs/docs" "/var/folders/m_/sm90t09d5591cgz5h242bkm80000gn/T/docs-snippets13141962741435068727"
  * }}}
  */
@main def testChimneySnippets(args: String*): Unit = testSnippets(args.toArray) { cfg =>
  val chimneyVersion = cfg
    .extra("chimney-version")
    .trim
    .pipe("\u001b\\[([0-9]+)m".r.replaceAllIn(_, "")) // remove possible console coloring from sbt
    .pipe(raw"(?U)\s".r.replaceAllIn(_, "")) // remove possible ESC characters
    .replaceAll("\u001B\\[0J", "") // replace this one offending thing

  val cfgFile = File(s"${cfg.docsDir}/../mkdocs.yml").getAbsoluteFile()
  println(hl"Reading MkDocs specific config: $cfgFile")
  val mkDocsCfg = MkDocsConfig.parse(cfgFile).right.get

  new ChimneySpecific(
    chimneyVersion = chimneyVersion,
    mkDocsCfg = mkDocsCfg,
    docsDir = cfg.docsDir,
    tmpDir = cfg.tmpDir
  )
}
