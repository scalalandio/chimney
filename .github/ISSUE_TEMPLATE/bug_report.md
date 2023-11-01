---
name: Bug report
about: An existing functionality has doesn't behave as expected
title: ''
labels: bug
assignees: ''

---

**Checklist**
- [ ] I read the documentation at https://chimney.readthedocs.io/ and checked that the functionality exists
- [ ] I verified that the behavior for my use case doesn't match the documentation
- [ ] I checked the https://github.com/scalalandio/chimney/issues and haven't found the issue reported
- [ ] I confirmed that the bug is not related to functionality that was deprecated: lifted transformers (`TransformerF`s) or `unsafeOption` flags
- [ ] I believe that this is actual bug, rather than a question how to use existing features

**Describe the bug**
A clear and concise description of what the bug is.

**Reproduction**
A snippet of code that triggers erroneous behavior. Ideally, a self-contained
[Scala CLI scripts](https://scala-cli.virtuslab.org/docs/guides/scripts) with all 
[the directives](https://scala-cli.virtuslab.org/docs/reference/directives/#using-directives) necessary to reproduce
the issue, e.g.:

```scala
//> using jvm [JVM version name according to Coursier]
//> using scala [Scala version]
//> using options [scalac options]
//> using dep io.scalaland::chimney::[Chimney version]
import io.scalaland.chimney.dsl._

// data models

// transformation example
```

**Expected behavior**
A clear and concise description of what you expected to happen.

**Actual behavior**
A description showing what happened: ideally a copy of compiler error, example of wrong value generated, stack trace and message of thrown exception.

**Which Chimney version do you use**
Version you used for confirming bug with a snippet.

**Which platform do you use**
- [ ] JVM
- [ ] Scala.js
- [ ] Scala Native

**If you checked JVM**
Which JVM distribution do you use (you can provide `java -version` output).

**Additional context**
Add any other context about the problem here.
