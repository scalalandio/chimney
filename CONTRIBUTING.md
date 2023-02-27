# Contributing to Chimney

Thank you for being interested in making Chimney a better library. We appreciate all feedback and contributing effort!

## Reporting bugs and suggesting features

Please, use provided templates to describe the issue. It makes it easier for us to understand what troubles you and what
we would have to do improve things. While it may look a bit verbose, knowing: the versions, what you did, what error you
got, really helps us understand the core of the problem faster.

## Contributing code

We are open to all contributors. Before we'll start reviewing the PRs we would ask however for a few things:

* linking the PR to the issue describing what it fixes/improves
* making sure that all tests pass, code is formatted, coverage haven't dropped, docs still compile
* if you want to do some big, serious refactors which affects the whole architecture, it might be a good idea to
  start a discussion first, since some things that may look like a nice hack today, might get in the way of releasing
  planned improvements in the future

Additionally, we would like to:

* add bug fixes test cases to `IssuesSpec`
* for new flags:
    * test them on the default setting (including failing compilation)
    * testing them flipped in in-place derivation (e.g. `foo.into[Bar].enableFlag.transform`)
    * testing them flipped in implicit `TransformerConfig` (
      e.g. `implicit val cfg = TransformerConfig.default.enableFlag`)
    * testing them overriding in in-place derivation the implicit config (
      e.g. `implicit val cfg = TransformerConfig.default.enableFlag` AND `foo.into[Bar].disbleFlag.transform`)
* for new modifiers:
    * test them on the default setting (including failing compilation)
    * testing them flipped in in-place derivation (e.g. `foo.into[Bar].modifier(_.bar, something).transform`)
* for any new feature we need a documentation:
    * Scaladoc documenting: what is does, what are its type parameters, what are its value parameters
    * Sphinx documentation describing new functionality
    * linking Scaladoc entries to corresponding Sphinx documentation (https://scalalandio.github.io/chimney/ subpage)
    * it might be good to discuss whether put this information into a new page or as a section of an existing page

### How to start working on Chimney

The first 2 things you need are JVM and sbt installed. There are many ways to manage their installations (jEnv, Sdkman,
package managers) and we didn't impose any of them since, as a library, Chimney should work with any supported JDK,
and sbt runner should fetch the right versions for the project on its own.

Then you need to set up project in an IDE. We are using Intellij, and are using these two settings to control which
version we are working on currently:

```scala
val versions = new {
  // ...

  // Which version should be used in IntelliJ
  val ideScala = scala213
  val idePlatform = VirtualAxis.jvm
}
```

Since almost all the sources are shared between different platforms and Scala versions, this imports only 1 version
into the IDE, which makes IDE happy, while sbt can compile/test any version/platform without reloading settings thanks
to sbt-projectmatrix.

Some details of this setup along with useful commands you would be able to see in the welcome prompt when you start sbt
shell.
