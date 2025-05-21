# Contributing to Chimney

Thank you for being interested in making Chimney a better library. We appreciate all feedback and contributing effort!

## Reporting bugs and suggesting features

Please, use provided templates to describe the issue. It makes it easier for us to understand what troubles you and what
we would have to do improve things. While it may look a bit verbose, knowing: the versions, what you did, what error you
got, really helps us understand the core of the problem faster.

## Contributing to the documentation

Documentation is hosted on [Read the Docs](https://docs.readthedocs.io/) using their versioning system. It uses
[MkDocs](https://www.mkdocs.org/) with [Material](https://squidfunk.github.io/mkdocs-material/) and 
[Macros](https://mkdocs-macros-plugin.readthedocs.io/) plugins.

To develop locally it is recommended to install [Just](https://github.com/casey/just) and Docker:

 * `cd docs`
 * `just serve`
 * open http://0.0.0.0:8000/

Site will reload and update as you edit the markdown files in `docs/docs` directory.

### Testing snippets

Testing snippets requires [Scala CLI](https://scala-cli.virtuslab.org/) installed. They are implemented with
[ScalaCLI.md Spec](https://github.com/MateuszKubuszok/scala-cli-md-spec).

To run all tests you should publish the newest Chimney artifacts locally:

```bash
sbt publish-local-for-tests
```

and look what version name was generated for them. Then you can run (from the root of the project):

```bash
scala-cli run scripts/test-snippets.scala -- --extra "chimney-version=[version from the artifacts]" "$PWD/docs/docs"
# or
scala-cli run scripts/test-snippets.scala -- --extra "chimney-version=[version from the artifacts]" --test-only '*' "$PWD/docs/docs"
```

to test all snippets. To test only selected ones you can run:

```bash
# test all snippets from supported-transformations.md
scala-cli run scripts/test-snippets.scala -- --extra "chimney-version=[version from the artifacts]" --test-only 'supported-transformations.md#*' "$PWD/docs/docs"
# test all snippets from supported-transformations.md in "Total `Transformer`s vs `PartialTransformer`s" section
scala-cli run scripts/test-snippets.scala -- --extra "chimney-version=[version from the artifacts]" --test-only 'supported-transformations.md#Total `Transformer`s vs `PartialTransformer`s*' "$PWD/docs/docs"
```

## Contributing code

We are open to all contributors. And Chimney is a complicated library. There are many things that you could
implement/fix on your own, but also many which might look easy at the first glance, but get harder as you learn more.
If you find yourself overwhelmed or unsure where to go next, don't hesitate to reach out!

Before we'll start reviewing the PRs we would ask however for a few things:

* linking the PR to the issue describing what it fixes/improves
* making sure that all tests pass, code is formatted, coverage haven't dropped, docs still compile
* if you want to do some big, serious refactors which affects the whole architecture, it might be a good idea to
  start a discussion first, since some things that may look like a nice hack today, might get in the way of releasing
  planned improvements in the future

Additionally, we would like to:

* add bug fixes' test cases to `IssuesSpec`
* for new flags:
    * test them on the default setting (including failing compilation)
    * testing them flipped in in-place derivation (e.g. `foo.into[Bar].enableFlag.transform`)
    * testing them flipped in implicit `TransformerConfig` (
      e.g. `implicit val cfg = TransformerConfiguration.default.enableFlag`)
    * testing them overriding in in-place derivation the implicit config (
      e.g. `implicit val cfg = TransformerConfiguration.default.enableFlag` AND `foo.into[Bar].disableFlag.transform`)
* for new modifiers:
    * test them on the default setting (including failing compilation)
    * testing them flipped in in-place derivation (e.g. `foo.into[Bar].modifier(_.bar, something).transform`)
* for any new feature we need a documentation:
    * Scaladoc documenting: what is does, what are its type parameters, what are its value parameters
    * MkDocs documentation describing new functionality
    * linking Scaladoc entries to corresponding MkDocs documentation (https://chimney.readthedocs.io/ subpage)
    * it might be good to discuss whether put this information into a new page or as a section of an existing page

### How to start working on Chimney

The first 2 things you need are JVM and sbt installed. There are many ways to manage their installations (jEnv, Sdkman,
package managers) and we didn't impose any of them since, as a library, Chimney should work with any supported JDK,
and sbt runner should fetch the right versions for the project on its own.

Then you need to set up project in an IDE. We are using Intellij, and are using these two settings (`dev.properties`)
to control which version we are working on currently:

```conf
# Do not commit changes to this file!
# Change it locally, reload build in IDE, and develop.

# Allowed: 2.12, 2.13, 3
ide.scala = 2.13
# Allowed: jvm, js, native
ide.platform = jvm
```

Since almost all the sources are shared between different platforms and Scala versions, this imports only 1 version
into the IDE, which makes IDE happy, while sbt can compile/test any version/platform without reloading settings thanks
to sbt-projectmatrix.

Some details of this setup along with useful commands you would be able to see in the welcome prompt when you start sbt
shell.

Information about Scala macros can be found on:

 * after loading project with Scala 2, using _Go to Symbol_ on `Context` values
   * alternatively opening [it in a browser](https://github.com/scala/scala/tree/2.13.x/src/reflect/scala/reflect/api)
     API directory and keeping the tab opened (might be a good idea since in my IDE sources are NOT imported so I see no
     documentation)
 * after loading project with Scala 3, opening `Quotes.scala` source
   * alternatively opening [it in a browser](https://github.com/scala/scala3/blob/3.3.3/library/src/scala/quoted/Quotes.scala)
     and keeping the tab opened 
 * [Scala 2 macros documentation](https://docs.scala-lang.org/overviews/macros/overview.html)
 * [Scala 3 macros documentation](https://docs.scala-lang.org/scala3/guides/macros/macros.html)
 * [EPFL papers](https://infoscience.epfl.ch/search?ln=en&as=1&m1=p&p1=macros&f1=keyword&op1=a&m2=p&p2=scala&f2=&op2=a&m3=a&p3=&f3=&dt=&d1d=&d1m=&d1y=&d2d=&d2m=&d2y=&rm=&action_search=Search&sf=title&so=a&rg=10&c=Infoscience&of=hb),
   in particular:
   * [**Scala Macros, a Technical Report**](https://infoscience.epfl.ch/record/183862?ln=en) (Scala 2 macros)
   * [**Quasiquotes for Scala**](https://infoscience.epfl.ch/record/185242?ln=en) (Scala 2 macros)
   * [**Scalable Metaprogramming in Scala 3**](https://infoscience.epfl.ch/record/299370?ln=en) (Scala 3 macros)
 * [**Scala 2 vs Scala 3 macros** presentation](https://kubuszok.com/presentations/#scala-2-vs-scala-3-macros-presentation)
   made by one of library's coauthors
 * [**Slow Auto, Inconvenient Semi - escaping false dichotomy with sanely-automatic derivation** presentation](https://mateuszkubuszok.github.io/SlowAutoInconvenientSemi/#/)
   discussing the mechanism used for (sanely) automatic derivation used in Chimney: why it exists and how it works
   * [**Scala.io** recording](https://www.youtube.com/watch?v=h9NdXLTZkGk) - 2nd time it was shown
   * [**Art of Scala** recording](https://www.youtube.com/watch?v=scWvlO_fb78) - 1st time it was shown
   * [**Scala Space** podcast discussing **Derive and conquer** recording](https://www.youtube.com/watch?v=FUL4Ou1SDx4) - followup to the presentation

Additionally, [**Polymorphic Embedding of DSLs** paper](https://www.informatik.uni-marburg.de/~rendel/hofer08polymorphic.pdf)
is useful to understanding how compatibility layer between Scala 2 macros and Scala 3 macros was created.

Very basic introduction can be found in [design doc](DESIGN.md) and in the
[Under the hood](https://chimney.readthedocs.io/under-the-hood/) section of the documentation.
From then on we suggest looking at tests, and using`.enableMacrosLogging` to see how some branches are triggered.
If still at doubt, you can ask us on GH discussions.

## Testing snapshots

Every commit merged on `master` which was not tagged, will be released as a snapshot to Sonatype snapshot repository.

You can check its name in:

 * **Sonatype Release / release-tag** GitHub Action [log](https://github.com/scalalandio/chimney/actions/workflows/release.yml)
 * newest commit will be available on https://chimney.readthedocs.io/ as the [latest version](https://chimney.readthedocs.io/en/latest/)
 * sbt after starting it for selected commit and checking which version will be printed by welcome prompt
 * you may need to add https://central.sonatype.com/repository/maven-snapshots to the list of repositories

If you want to test changes that were not merged to `master` you can publish them you for yourself locally. For that
open sbt and run `publishLocal` task for every artifact that you want to publish as a snapshot. Keep in mind that:

 * `chimney-macro-commons` is required for `chimney` to work
 * `chimney-cats`, `chimney-java-collections` and `chimney-protobufs` rely on `chimney`
 * `chimneyMacroCommons`, `chimney`, `chimneyCats`, etc. are versions with Scala 2.13 on JVM
 * versions for Scala 2.12, Scala 3, Scala.js and Scala Native are NOT published by prepending `++` but by
   using a different suffix (`2_12`, `3`, `JS`, `Native`, see `projects` task to get a full list of projects) 

## Release checklist

To ensure that there are no silly, easily avoidable issues found right after the release, there is a list of steps we
would follow to publish a new version of the library:

1. Pre-release checks 
  - [ ] verify that all task in the milestone are finished (if a milestone for the release exists)
  - [ ] verify that all Scala Steward PRs are merged or manually replaced
  - [ ] verify that chimney-macro-commons is **not** on RC version
  - [ ] wait for the `master` to build and pass all tests, ensure that they are all green
  - [ ] search `TODO`s in the code and verify that they are not problematic (no missing documentation links for instance)
  - [ ] verify that docs from the latest build are rendering correctly (on RTD or `cd docs && just serve`)
  - [ ] close the milestone (if it exists)
2. Release
  - [ ] create tag (no `v` in the tag name, no `-a`)
  - [ ] approve running benchmarks on the tagged commit
3. Post-release actions
  - [ ] verify in https://oss.sonatype.org/ that the release was successful
  - [ ] open https://chimney.readthedocs.io/ and make sure that the version got published (-Mn, -RCn versions might require manual activation!)
  - [ ] draft a (pre)release on GitHub (don't publish it until Maven lists the new version!)
  - [ ] wait until the version is available on Maven Central
  - [ ] verify that Scaladex sees it
  - [ ] force download of Scaladoc (open Scaladoc dor each Scala version, change "latest" to the new version to force download) 
  - [ ] run https://github.com/sbts/github-badge-cache-buster to flush GH badge cache (`./github-badge-cache-buster.sh https://github.com/scalalandio/chimney`)
  - [ ] publish the (pre)release on GitHub
  - [ ] publish announcements on Twitter/Mastodon/Reddit/etc
  - [ ] add published versions to `mimaPreviousArtifacts` value in `build.sbt`  

While building, testing and deployment are automated, some things have to be verified manually, because they are not
immediate (library can become visible on Maven Central 15 minutes after publishing or a few hours later), or because
human needs to tell if things are in good shape (documentation's content, whether all that should go into the release
was done, etc).
