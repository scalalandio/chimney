//> using scala 3.3.3
//> using dep com.kubuszok::scala-cli-md-spec:0.1.0
//> using dep org.virtuslab::scala-yaml:0.0.8

import com.kubuszok.scalaclimdspec.*
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

class ChimneyExtendedRunner(runner: Runner)(
    chimneyVersion: String,
    mkDocsCfg: MkDocsConfig
) extends Runner {

  private val defaultScalaVersion = "2.13.13"

  private val replacePatterns = (mkDocsCfg.extra + (raw"chimney_version\(\)" -> chimneyVersion)).map { case (k, v) =>
    (raw"\{\{\s*" + k + raw"\s*\}\}") -> v
  }

  private val manuallyIgnored = ListMap(
    "supported-transformations.md#Between `sealed`/`enum`s[2]" -> "snippet fails!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "supported-transformations.md#Between `sealed`/`enum`s[3]" -> "snippet throws exception!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "supported-transformations.md#Between `sealed`/`enum`s[4]" -> "snippet throws exception!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "supported-transformations.md#Java's `enum`s[1]" -> "requires previous snippet with Java code",
    "supported-transformations.md#Java's `enum`s[2]" -> "requires previous snippet with Java code",
    "supported-transformations.md#Handling a specific `sealed` subtype with a computed value[3]" -> "snippet throws exception!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "supported-transformations.md#Handling a specific `sealed` subtype with a computed value[4]" -> "requires previous snippet with Java code",
    "supported-transformations.md#Handling a specific `sealed` subtype with a computed value[5]" -> "requires previous snippet with Java code",
    "supported-transformations.md#Handling a specific `sealed` subtype with a computed value[6]" -> "requires previous snippet with Java code",
    "supported-transformations.md#Defining custom name matching predicate[1]" -> "example split into multiple files",
    "supported-transformations.md#Defining custom name matching predicate[2]" -> "contunuation from the previous snippet",
    "troubleshooting.md#Ducktape[2]" -> "snippet throws exception!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "troubleshooting.md#Ducktape[4]" -> "snippet throws exception!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "troubleshooting.md#Ducktape[8]" -> "snippet throws exception!!! investigate later", // FIXME: https://github.com/scala/scala3/issues/20349
    "troubleshooting.md#Ducktape[10]" -> "snippet throws exception!!! investigate later" // FIXME: https://github.com/scala/scala3/issues/20349
  )

  val addDefaultScala: Snippet.Content => Snippet.Content = {
    case Snippet.Content.Single(content) =>
      Snippet.Content.Single(
        if content.contains("//> using scala") then content
        else s"//> using scala $defaultScalaVersion\n$content"
      )
    case Snippet.Content.Multiple(files) =>
      Snippet.Content.Multiple(
        if files.values.exists(_.content.contains("//> using scala")) then files
        else {
          val (fileName, Snippet.Content.Single(content0)) = files.head
          val content1: Snippet.Content.Single =
            Snippet.Content.Single(s"//> using scala $defaultScalaVersion\n$content0")
          ListMap(fileName -> content1) ++ files.tail
        }
      )
  }

  val interpolateTemplates: Snippet.Content => Snippet.Content = {
    case Snippet.Content.Single(content) =>
      Snippet.Content.Single(replacePatterns.foldLeft(content) { case (s, (k, v)) => s.replaceAll(k, v) })
    case Snippet.Content.Multiple(files) =>
      Snippet.Content.Multiple(
        ListMap.from(files.mapValues(interpolateTemplates(_).asInstanceOf[Snippet.Content.Single]))
      )
  }

  export runner.{docsDir, filter, tmpDir}

  extension (snippet: Snippet)
    def adjusted: Snippet =
      // interpolate templates before Default adjustments
      runner.adjusted(snippet.copy(content = interpolateTemplates(snippet.content)))

    def howToRun: Runner.Strategy = manuallyIgnored.get(snippet.stableName) match
      case None         => runner.howToRun(snippet)
      case Some(reason) => Runner.Strategy.Ignore(reason)

  extension (snippets: List[Snippet])
    def adjusted: List[Snippet] = runner.adjusted(snippets).map { snippet =>
      howToRun(snippet) match
        case Runner.Strategy.Ignore(_) => snippet
        case _                         => snippet.copy(content = addDefaultScala(snippet.content))
    }
}

/** Usage:
  *
  * From the project root (if called from other directory, adapt path after PWD accordingly):
  *
  * on CI:
  * {{{
  * # run all tests, use artifacts published locally from current tag
  * scala-cli run scripts/test-snippets.scala -- --extra "chimney-version=$(sbt -batch -error 'print chimney/version')" "$PWD/docs/docs"
  * }}}
  *
  * during development:
  * {{{
  * # fix: version to use, tmp directory
  * scala-cli run scripts/test-snippets.scala -- --extra "chimney-version=1.0.0-RC1" --test-only "supported-transformations.md*" "$PWD/docs/docs" "/var/folders/m_/sm90t09d5591cgz5h242bkm80000gn/T/docs-snippets13141962741435068727"
  * }}}
  */
@main def testChimneySnippets(args: String*): Unit = testSnippets(args.toArray) { cfg =>
  new ChimneyExtendedRunner(new Runner.Default(cfg))(
    chimneyVersion = cfg
      .extra("chimney-version")
      .trim
      .pipe("\u001b\\[([0-9]+)m".r.replaceAllIn(_, "")) // remove possible console coloring from sbt
      .pipe(raw"(?U)\s".r.replaceAllIn(_, "")) // remove possible ESC characters
      .replaceAll("\u001B\\[0J", ""), // replace this one offending thing
    mkDocsCfg = MkDocsConfig
      .parse(File(s"${cfg.docsDir}/../mkdocs.yml").getAbsoluteFile())
      .fold(s => throw Exception(s), identity)
  )
}
