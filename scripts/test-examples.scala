//> using scala 3.3.3

import java.io.File
import java.nio.file.{Paths, Files}
import scala.util.Using
import scala.sys.process._
import scala.Console.{MAGENTA, RESET}

val patterns = Map(
  "{{ chimney_version() }}" -> "1.0.0-RC1",
  "{{ libraries.ducktape }}" -> "0.2.0",
  "{{ libraries.henkan }}" -> "0.6.5",
  "{{ libraries.scala_automapper }}" -> "0.7.0",
  "{{ scala.2_12 }}"  -> "2.12.18",
  "{{ scala.2_13 }}"  -> "2.13.13",
  "{{ scala.3 }}"  -> "3.3.3",
)

case class Markdown(name: String, content: List[String])

case class Snippet(name: String, line: Int, content: String) {
  def id = s"$name:$line"
}

case class SavedSnippet(id: String, directory: String)

def readMarkdowns(dir: File): List[Markdown] =
  for {
    files <- Option(dir.listFiles()).toList
    markdownFile <- files.sortBy(_.getName()) if markdownFile.getAbsolutePath().endsWith(".md")
  } yield 
    Using(io.Source.fromFile(markdownFile)) { src =>
      val name = markdownFile.getName() 
      Markdown(name.substring(0, name.length() - ".md".length()), src.getLines().toList)
    }.get

  
def extractSnippets(markdown: Markdown): List[Snippet] = {
  val name = markdown.name

  enum Mode:
    case Reading(lineNo: Int, indent: Int, contentReverse: List[String]) 
    case Awaiting

  import Mode.*

  val start = "```scala"
  val end   = "```"

  def adjustLine(line: String, indent: Int): String = {
    val stripIndent = if (line.length() > indent) line.substring(indent) else line
    patterns.foldLeft(stripIndent) { case (s, (k, v)) =>
      s.replace(k, v)
    }
  }

  def mkSnippet(lineNo: Int, contentReverse: List[String]): Snippet = {
    val content0 = contentReverse.reverse.mkString("\n")
    val content = if (content0.startsWith("//> using scala")) content0
                  else "//> using scala 2.13.13\n" + content0
    Snippet(name, lineNo + 1, content)
  }

  def loop(content: List[(String, Int)], mode: Mode, reverseResult: List[Snippet]): List[Snippet] = content match {
    case (line, lineNo) :: lines => mode match {
      case Reading(lineNo, indent, contentReverse) =>
        if (line.trim() == end) loop(lines, Awaiting, mkSnippet(lineNo, contentReverse) :: reverseResult)
        else loop(lines, Reading(lineNo, indent, adjustLine(line, indent) :: contentReverse), reverseResult)
      case Awaiting =>
        if (line.trim() == start) loop(lines, Reading(lineNo, line.indexOf(start), Nil), reverseResult)
        else loop(lines, Awaiting, reverseResult)
    }
    case Nil => reverseResult.reverse
  }

  loop(markdown.content.zipWithIndex, Awaiting, Nil)
}

def saveSnippet(snippet: Snippet, tmpDir: File): SavedSnippet = {
  val snippetFile = File(s"${tmpDir.getPath()}/${snippet.name}_${snippet.line}/snippet.sc")
  snippetFile.getParentFile().mkdirs()
  Files.writeString(snippetFile.toPath(), snippet.content)
  SavedSnippet(s"${snippet.id}", snippetFile.getParent())
}

val ignored: Set[String] = Set(
  "cookbook:55",
)

@main // scala-cli run test-examples.scala -- "../docs/docs"
def testExamples(path: String): Unit = {
  val docsDir = File(path)
  println(s"${MAGENTA}Started reading from ${docsDir.getAbsolutePath()}${RESET}")
  val markdowns = readMarkdowns(docsDir)
  println(s"${MAGENTA}Read files: ${markdowns.map(_.name)}${RESET}")
  val snippets = markdowns.flatMap(extractSnippets)
  println(s"${MAGENTA}Found snippets:${RESET}\n\n${snippets.map(s => s"${MAGENTA}${s.id}${RESET}\n${s.content}").mkString("\n\n")}\n\n")
  val (ignoredSnippets, testedSnippets) = snippets.partition(s => ignored(s.id))
  println(s"${MAGENTA}Ignoring snippets:${RESET}\n\n${ignoredSnippets.map(s => s"${MAGENTA}${s.id}${RESET}").mkString("\n")}\n\n")
  val tmpDir = Files.createTempDirectory(s"docs-snippets").toFile()
  val saved = testedSnippets.map(saveSnippet(_, tmpDir))
  val failed = saved.flatMap { case SavedSnippet(id, snippetDir) =>
    println(s"${MAGENTA}Testing: $id ($snippetDir)${RESET}:")
    try {
      s"scala-cli run '$snippetDir'".!!
      List.empty[String]
    } catch {
      case _: Throwable => List(id)
    }
  }
  if (failed.nonEmpty) {
    println(s"${MAGENTA}Failed snippets${RESET}:\n${failed.mkString("\n")}")
    println(s"Add them to ignored list")
    sys.exit(1)
  } else {
    println("All snippets run succesfully!")
  }
}
