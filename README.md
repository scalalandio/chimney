# Chimney <img src="chimney.png" alt="Chimney logo" width="64" />

[![chimney Scala version support](https://index.scala-lang.org/scalalandio/chimney/chimney/latest.svg)](https://index.scala-lang.org/scalalandio/chimney/chimney)

![CI build](https://github.com/scalalandio/chimney/workflows/CI%20build/badge.svg)
[![codecov.io](http://codecov.io/github/scalalandio/chimney/coverage.svg?branch=master)](http://codecov.io/github/scalalandio/chimney?branch=master)
[![License](http://img.shields.io/:license-Apache%202-green.svg)](http://www.apache.org/licenses/LICENSE-2.0.txt) [![Join the discussions at https://github.com/scalalandio/chimney/discussions](https://img.shields.io/github/discussions/scalalandio/chimney
)](https://github.com/scalalandio/chimney/discussions)

[![Documentation Status](https://readthedocs.org/projects/chimney/badge/?version=latest)](https://chimney.readthedocs.io/en/latest/?badge=latest)
[![Scaladoc 2.11](https://javadoc.io/badge2/io.scalaland/chimney_2.11/scaladoc%202.11.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.11)
[![Scaladoc 2.12](https://javadoc.io/badge2/io.scalaland/chimney_2.12/scaladoc%202.12.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.12)
[![Scaladoc 2.13](https://javadoc.io/badge2/io.scalaland/chimney_2.13/scaladoc%202.13.svg)](https://javadoc.io/doc/io.scalaland/chimney_2.13)
[![Scaladoc 3](https://javadoc.io/badge2/io.scalaland/chimney_3/scaladoc%203.svg)](https://javadoc.io/doc/io.scalaland/chimney_3)

Battle tested Scala library for boilerplate-free data transformations.

What does it mean?

Imagine you have strict domain definition of a `User` and much less strict
API definition of a `UserAPI`:

```scala
case class User(name: User.Name, surname: User.Surname)
object User {
  case class Name(value: String) extends AnyVal
  case class Surname(value: String) extends AnyVal
}

case class UserAPI(name: Option[String], surname: Option[String])
```

Converting the strict representation to less strict is obvious and boring:

```scala
val user = User(User.Name("John"), User.Surname("Smith"))

// encoding domain to API by hand
UserAPI(Some(user.name.value), Some(user.surname.value))
```

Converting the less strict representation to strict is also obvious and boring,
and additionally long:

```scala
val userApi = UserAPI(Some(user.name.value), Some(user.surname.value))

// decoding API to domain by hand
for {
  name <- user.name.map(User.Name)
  surname <- user.surname.map(User.Surname)
} yield User(name, surname)
```

You know how this code would look like to the letter. There is nothing new you
would learn from reading it if someone else wrote it. And you need to write it
and update any time your case classes change.

The good news is that this obvious and boring code could be generated for you:

```scala
import io.scalaland.chimney.dsl._

user.transformInto[UserAPI]
// UserAPI(John, Smith)

userApi.transformIntoPartial[User].asOption
// Some(User(Name(John, Surname(Smith))))
```

Short, simple, easy! When you update the classes, it would update the generated
code for you. Additionally, you need not worry that you forgot to change something
as you copy-pasted pieces of the transformation ad nauseam!

It can also be generated for you when you works with sealed hierarchies
(including Scala 3's `enum`!):

```scala
sealed trait UserStatusAPI
object UserStatusAPI {
  case object Active extends UserStatusAPI
  case class Inactive(cause: String) extends UserStatusAPI
}

enum UserStatus:
  case Active
  case Inactive(cause: String)

val userStatusAPI: UserStatusAPI = UserStatusAPI.Active
val userStatus: UserStatus = UserStatus.Inactive("banned")
```
  
```scala
import io.scalaland.chimney.dsl._

userStatusApi.transformInto[UserStatus]
// UserStatus.Active

userStatus.transformInto[UserStatusAPI]
// UserStatusAPI.Inactive(banned)
```

and Java Beans:

```scala
class UserBean {
private var name: String = _
private var surname: String = _

  def getName: String = name
  def setName(name: String): Unit = this.name = name

  def getSurname: String = surname
  def setSurname(surname: String): Unit = this.surname = surname

  override def toString(): String = s"UserBean($name, $surname)"
}

val userBean = new UserBean()
userBean.setName("John")
userBean.setSurname("Smith")
```

```scala
import io.scalaland.chimney.dsl._

user.into[UserBean].enableBeanSetters.transform
// UserBean(John, Smith)

userBean.into[User].enableBeanGetters.transform
// User(John, Smith)
```

With Chimney, this and much more can be safely generated for you by the compiler -
the more repetitive transformation you have the more boilerplate you shrug off!

The library uses Scala macros underneath, to give you:
- type-safety at compile-time
- efficient generated code, almost equivalent to hand-written version
- excellent compilation error messages
- minimal overhead on compilation time

## Getting started

To include Chimney to your SBT project, add the following line to your `build.sbt`:

```scala
// if you use Scala on JVM-only
libraryDependencies += "io.scalaland" %% "chimney" % chimneyVersion
// if you cross-compile to Scala.js and/or Scala Native
libraryDependencies += "io.scalaland" %%% "chimney" % chimneyVersion
```

where the latest versions available on Maven for each platform are

[![Chimney JVM versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=jvm)](https://search.maven.org/artifact/io.scalaland/chimney_2.13) <br>
[![Chimney Scala.js 1.x versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs1)](https://search.maven.org/artifact/io.scalaland/chimney_sjs1_2.13) <br>
[![Chimney Scala.js 0.6 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=sjs0.6)](https://search.maven.org/artifact/io.scalaland/chimney_sjs0.6_2.13) <br>
[![Chimney Scala Native 0.4 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.4)](https://search.maven.org/artifact/io.scalaland/chimney_native0.4_2.13) <br>
[![Chimney Scala Native 0.3 versions](https://index.scala-lang.org/scalalandio/chimney/chimney/latest-by-scala-version.svg?platform=native0.3)](https://search.maven.org/artifact/io.scalaland/chimney_native0.3_2.11) <br>

Library is currently supported for Scala 2.12.x, 2.13.x and 3.x on JVM, SJS 1.x, SN 0.4. Other versions should be considered EOL.

Due to some [compiler bugs](https://issues.scala-lang.org/browse/SI-7046),
it's recommended to use at least Scala 2.12.1.

### Trying out with Scala CLI/Ammonite

If you are using Scala CLI you can try out Chimney by adding it with `using` clause:
```scala
//> using scala "2.13.11"
//> using dep "io.scalaland::chimney:0.8.0-RC1"
import io.scalaland.chimney.dsl._

case class Foo(x: String, y: Int, z: Boolean = true)
case class Bar(x: String, y: Int)

object Main extends App {
  println(Foo("abc", 10).transformInto[Bar])
  println(Bar("abc", 10).into[Foo].enableDefaultValues.transform)
}
```

or run the Ammonite REPL:

```scala
scala-cli --power repl --ammonite --scala "2.13.11" --dependency "io.scalaland::chimney:0.8.0-RC1"
Loading...
Welcome to the Ammonite Repl 2.5.9 (Scala 2.13.11 Java 17.0.3)
@ case class Foo(x: String, y: Int, z: Boolean = true)
defined class Foo

@ case class Bar(x: String, y: Int)
defined class Bar

@ import io.scalaland.chimney.dsl._
import io.scalaland.chimney.dsl._

@ Foo("abc", 10).transformInto[Bar]
res3: Bar = Bar(x = "abc", y = 10)

@ Bar("abc", 10).into[Foo].enableDefaultValues.transform
res4: Foo = Foo(x = "abc", y = 10, z = true)
```

If you don't have Scala CLI installed you can use this quick script that downloads
[coursier](https://github.com/alexarchambault/coursier) and uses it to fetch
[Ammonite](https://github.com/lihaoyi/Ammonite) REPL with the latest version
of Chimney. It drops you immediately into a REPL session.

```
curl -s https://raw.githubusercontent.com/scalalandio/chimney/master/try-chimney.sh | bash
```

## Documentation

Chimney documentation is available at https://chimney.readthedocs.io.

#### Building documentation locally

For building documentation locally you can use Docker:

```bash
docker run --rm -v "$PWD/docs:/docs" sphinxdoc/sphinx:5.3.0 bash -c "pip install sphinx-rtd-theme && make html"
```

It will build the docs in the `./docs/build/html/` directory.

## Thanks

Thanks to [JProfiler (Java profiler)](https://www.ej-technologies.com/products/jprofiler/overview.html)
for helping us develop the library and allowing us to use it during development.

Thanks to [SwissBorg](https://swissborg.com) for sponsoring the development time for this project.
