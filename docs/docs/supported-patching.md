# Supported Patching

Besides transforming Chimney is also able to perform patching - take a "patched" value, a "patch" value, and compute
the updated version of the patched value.

## Updating `case class`

Currently, the only supported case is updating one `case class` with another: 

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Email(address: String) extends AnyVal
    case class Phone(number: Long) extends AnyVal

    case class User(id: Int, email: Email, phone: Phone)
    case class UserUpdateForm(email: String, phone: Long)

    val user = User(10, Email("abc@@domain.com"), Phone(1234567890L))
    val updateForm = UserUpdateForm("xyz@@domain.com", 123123123L)

    pprint.pprintln(
      user.patchUsing(updateForm)
    )
    // expected output:
    // User(id = 10, email = Email(address = "xyz@@domain.com"), phone = Phone(number = 123123123L))
    ```

As we see the values from the "patch" aren't always of the same type as the values they are supposed to update.
In such case, macros use `Transformer`s logic under the hood to convert a patch into a patched value.

### Updating field with a provided value

When we want to not only update one object with fields from another object but also set some fields manually,
we can do it using `withFieldConst` (just like with `Transformer`s):

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Email(address: String) extends AnyVal
    case class Phone(number: Long) extends AnyVal

    case class User(id: Int, email: Email, phone: Phone)
    case class UserUpdateForm(email: String, phone: Long)

    val user = User(10, Email("abc@@domain.com"), Phone(1234567890L))
    val updateForm = UserUpdateForm("xyz@@domain.com", 123123123L)

    pprint.pprintln(
      user.using(updateForm)
        .withFieldConst(_.id, 20)
        .patch
    )
    // expected output:
    // User(id = 20, email = Email(address = "xyz@@domain.com"), phone = Phone(number = 123123123L))
    ```

### Updating field with a computed value

When we want to not only update one object with fields from another object but also set some fields to computed value,
we can do it using `withFieldComputed` (just like with `Transformer`s) or `withFieldComputedFrom`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Email(address: String) extends AnyVal
    case class Phone(number: Long) extends AnyVal

    case class User(id: Int, email: Email, phone: Phone)
    case class UserUpdateForm(email: String, phone: Long)

    case class Wrapper[A](value: A)

    val wrappedUser = Wrapper(User(10, Email("abc@@domain.com"), Phone(1234567890L)))
    val updateForm = Wrapper(UserUpdateForm("xyz@@domain.com", 123123123L))

    pprint.pprintln(
      wrappedUser.using(updateForm)
        .withFieldComputed(_.value.id, patch => patch.value.phone.toInt)
        .patch
    )
    // expected output:
    // Wrapper(
    //   value = User(
    //     id = 123123123,
    //     email = Email(address = "xyz@@domain.com"),
    //     phone = Phone(number = 123123123L)
    //   )
    // )

    pprint.pprintln(
      wrappedUser.using(updateForm)
        .withFieldComputedFrom(_.value.phone)(_.value.id, phone => phone.toInt)
        .patch
    )
    // expected output:
    // Wrapper(
    //   value = User(
    //     id = 123123123,
    //     email = Email(address = "xyz@@domain.com"),
    //     phone = Phone(number = 123123123L)
    //   )
    // )
    ```

### Ignoring fields in patches

When the patch `case class` contains a field that does not exist in patched object, Chimney will not be able to generate
`Patcher`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class User(id: Int, email: String, phone: Long)
    case class UserUpdateForm(email: String, phone: Long, address: String)

    val user = User(10, "abc@@domain.com", 1234567890L)

    user.patchUsing(UserUpdateForm("xyz@@domain.com", 123123123L, "some address"))
    // expected error:
    // Chimney can't derive patching for User with patch type UserUpdateForm
    //
    // Field named 'address' not found in target patching type snippet.User!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

This default behavior is intentional to prevent silent oversight of typos in patcher field names.

But there is a way to ignore redundant patcher fields explicitly with `.ignoreRedundantPatcherFields` operation.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(id: Int, email: String, phone: Long)
    case class UserUpdateForm(email: String, phone: Long, address: String)

    val user = User(10, "abc@@domain.com", 1234567890L)

    pprint.pprintln(
      user
        .using(UserUpdateForm("xyz@@domain.com", 123123123L, "some address"))
        .ignoreRedundantPatcherFields
        .patch
    )
    // expected output:
    // User(id = 10, email = "xyz@@domain.com", phone = 123123123L)

    locally {
      // All patching derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = PatcherConfiguration.default.ignoreRedundantPatcherFields

      pprint.pprintln(
        user.patchUsing(UserUpdateForm("xyz@@domain.com", 123123123L, "some address"))
      )
      // expected output:
      // User(id = 10, email = "xyz@@domain.com", phone = 123123123L)
    }
    ```

Patching succeeded using only relevant fields that appear in the patched object and ignoring address: `String` field 
from the patch.

If the flag was enabled in the implicit config it can be disabled with `.failRedundantPatcherFields`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class User(id: Int, email: String, phone: Long)
    case class UserUpdateForm(email: String, phone: Long, address: String)

    val user = User(10, "abc@@domain.com", 1234567890L)

    // All patching derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
    implicit val cfg = PatcherConfiguration.default.ignoreRedundantPatcherFields

    user
      .using(UserUpdateForm("xyz@@domain.com", 123123123L, "some address"))
      .failRedundantPatcherFields
      .patch
    // expected error:
    // Chimney can't derive patching for User with patch type UserUpdateForm
    //
    // Field named 'address' not found in target patching type User!
    //
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

Alternatively you can explicitly point to the fields that you want to ignore:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(id: Int, email: String, phone: Long)
    case class UserUpdateForm(email: String, phone: Long, address: String)

    val user = User(10, "abc@@domain.com", 1234567890L)

    pprint.pprintln(
      user
        .using(UserUpdateForm("xyz@@domain.com", 123123123L, "some address"))
        .withFieldIgnored(_.address)
        .patch
    )
    // expected output:
    // User(id = 10, email = "xyz@@domain.com", phone = 123123123L)
    ```

## Updating `AnyVal` with `AnyVal`

It is possible to update values containing `AnyVal`s:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo[A](value: A)
    case class Bar[A](value: A)
    case class Wrapper(str: String) extends AnyVal

    pprint.pprintln(
      Foo("aaa").patchUsing(Bar(Wrapper("bbb")))
    )
    // expected output:
    // Foo(value = "bbb")
    pprint.pprintln(
      Foo(Wrapper("aaa")).patchUsing(Bar("bbb"))
    )
    // expected output:
    // Foo(value = Wrapper(str = "bbb"))
    pprint.pprintln(
      Foo(Wrapper("aaa")).patchUsing(Bar(Wrapper("bbb")))
    )
    // expected output:
    // Foo(value = Wrapper(str = "bbb"))
    ```

## Updating value with `Option`

It is possible to patch using optional values of type `Option[A]` as long as the `Transformer` is available (or derivable)
for `A`. If the value is present (`Some`), it’s used for patching a field in the target object; otherwise (`None`) it’s ignored
and the field value is copied from the original object.

Let’s consider the following patch:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(id: Int, email: String, phone: Long)
    case class UserPatch(email: Option[String], phone: Option[Long])

    val user = User(10, "abc@@domain.com", 1234567890L)
    val update = UserPatch(email = Some("updated@@example.com"), phone = None)

    pprint.pprintln(
      user.patchUsing(update)
    )
    // expected output:
    // User(id = 10, email = "updated@@example.com", phone = 1234567890L)
    ```

The field `phone` remained the same as in the original `user`, while the optional e-mail string got updated from
a patch object.

!!! tip

    There are special rules which makes sure that we can not only update `A` with `Option[B]`, but also

     * `Option[A]` with `Option[Option[B]]`
     * `Either[A, B]` with `Option[Either[C, D]]`
     * `Collection1[A]` with `Option[Collection2[B]]`

### Treating `None` as no-update instead of "set to `None`"

An interesting case appears when both the patch `case class` and the patched object define fields `f: Option[A]`.
Depending on the values of `f` in the patched object and patch, we would like to apply the following semantic table:

| `patchedObject.f` | `patch.f`      | patching result |
|-------------------|----------------|-----------------|
| `None`            | `Some(value)`  | `Some(value)`   |
| `Some(value1)`    | `Some(value2)` | `Some(value2)`  |
| `None`            | `None`         | `None`          |
| `Some(value)`     | `None`         | **???**         |

When a `patch.f` contains some value, it’s immediately used for replacing a field in the target object (rows 1 and 2), 
regardless of the original object field value. When both field are `None`, the patching result is also `None` (row 3).

But if the original object contains a some value, but the patch comes with a `None`, we can do two things:

  - clear value in target object (replace it with `None`)
  - or ignore updating this particular field (as in the previous section)

Both choices may make perfect sense, depending on the context. By default, Chimney does the former (clears the value),
but it also gives a simple way to always ignore `None` from the patch with `.ignoreNoneInPatch` operation.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Option[String], age: Option[Int])
    case class UserPatch(name: Option[String], age: Option[Int])

    val user = User(Some("John"), Some(30))
    val userPatch = UserPatch(None, None)
     
    pprint.pprintln(
      user.patchUsing(userPatch)
    )
    // clears both fields:
    // expected output:
    // User(name = None, age = None)

    pprint.pprintln(
      user
        .using(userPatch)
        .ignoreNoneInPatch
        .patch
    )
    // ignores updating both fields:
    // expected output:
    // User(name = Some(value = "John"), age = Some(value = 30))

    locally {
      // All patching derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

      pprint.pprintln(
        user.patchUsing(userPatch)
      )
      // ignores updating both fields:
      // expected output:
      // User(name = Some(value = "John"), age = Some(value = 30))
    }
    ```

If the flag was enabled in the implicit config it can be disabled with `.clearOnNoneInPatch`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Option[String], age: Option[Int])
    case class UserPatch(name: Option[String], age: Option[Int])

    val user = User(Some("John"), Some(30))
    val userPatch = UserPatch(None, None)

    // all patching derived in this scope will see these new flags
    implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

    pprint.pprintln(
      user
        .using(userPatch)
        .clearOnNoneInPatch
        .patch
    )
    // clears both fields:
    // expected output:
    // User(name = None, age = None)
    ```
 
### Unambiguous `Option` update

The flag is not required when updating `Option[A]` with `Option[Option[B]]`, as then it is always
unambiguous what to do:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Option[String], age: Option[Int])
    case class UserPatch(name: Option[Option[String]], age: Option[Option[Int]])

    val user = User(Some("John"), Some(30))
    val userPatch1 = UserPatch(None, None)
     
    pprint.pprintln(
      user.patchUsing(userPatch1)
    )
    // clears both fields:
    // expected output:
    // User(name = Some(value = "John"), age = Some(value = 30))

    val userPatch2 = UserPatch(Some(Some("Jane")), Some(Some(25)))

    pprint.pprintln(
      user.patchUsing(userPatch2)
    )
    // ignores updating both fields:
    // expected output:
    // User(name = Some(value = "Jane"), age = Some(value = 25))
    ```

## Updating value with `Either`

By default patch always just replaces the old value with a new one:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Either[String, String], age: Either[String, Int])
    case class UserPatch(name: Either[String, String], age: Either[String, Int])

    val user = User(Right("John"), Right(30))
    val userPatch = UserPatch(Left("nope"), Left("nope"))

    pprint.pprintln(
      user.patchUsing(userPatch)
    )
    // expected output:
    // User(name = Left(value = "nope"), age = Left(value = "nope"))
    ```

### Treating `Left` as no-update instead of "set to `Left`"

Depending on the values of `f` in the patched object and patch, we could like to apply the following semantic table:

| `patchedObject.f` | `patch.f`       | patching result |
|-------------------|-----------------|-----------------|
| `Left(err)`       | `Right(value)`  | `Right(value)`  |
| `Right(value1)`   | `Right(value2)` | `Right(value2)` |
| `Left(err1)`      | `Left(err2)`    | `Left(err2)`    |
| `Right(value)`    | `Left(err)`     | **???**         |

When a `patch.f` contains `Right` value, it’s immediately used for replacing a field in the target object (rows 1 and 2), 
regardless of the original object field value. When both field are `Left`, the patching result is also `Left`
(with newer error) (row 3).

But if the original object contains a `Right` value, but the patch comes with a `Left`, we can do two things:

  - fail value in target object (replace it with `Left`)
  - or ignore updating this particular field (as in the previous section)

Both choices may make perfect sense, depending on the context. By default, Chimney does the former (fails the value),
but it also gives a simple way to always ignore `Left` from the patch with `.ignoreLeftInPatch` operation.

The latter would assume that `Either` is `Right`-biased.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Either[String, String], age: Either[String, Int])
    case class UserPatch(name: Either[String, String], age: Either[String, Int])

    val user = User(Right("John"), Right(30))
    val userPatch = UserPatch(Left("nope"), Left("nope"))
     
    pprint.pprintln(
      user.patchUsing(userPatch)
    )
    // fails both fields:
    // expected output:
    // User(name = Left(value = "nope"), age = Left(value = "nope"))

    pprint.pprintln(
      user
        .using(userPatch)
        .ignoreLeftInPatch
        .patch
    )
    // ignores updating both fields:
    // expected output:
    // User(name = Right(value = "John"), age = Right(value = 30))

    locally {
      // All patching derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = PatcherConfiguration.default.ignoreLeftInPatch

      pprint.pprintln(
        user.patchUsing(userPatch)
      )
      // ignores updating both fields:
      // expected output:
      // User(name = Right(value = "John"), age = Right(value = 30))
    }
    ```

If the flag was enabled in the implicit config it can be disabled with `.useLeftOnLeftInPatch`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Either[String, String], age: Either[String, Int])
    case class UserPatch(name: Either[String, String], age: Either[String, Int])

    val user = User(Right("John"), Right(30))
    val userPatch = UserPatch(Left("nope"), Left("nope"))

    // all patching derived in this scope will see these new flags
    implicit val cfg = PatcherConfiguration.default.ignoreLeftInPatch

    pprint.pprintln(
      user
        .using(userPatch)
        .useLeftOnLeftInPatch
        .patch
    )
    // clears both fields:
    // expected output:
    // User(name = Left(value = "nope"), age = Left(value = "nope"))
    ```
 
### Unambiguous `Either` update

The flag is not required when updating `Either[K, V]` with `Option[Either[K2, V2]]`, as then it is always
unambiguous what to do:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class User(name: Either[String, String], age: Either[String, Int])
    case class UserPatch(name: Option[Either[String, String]], age: Option[Either[String, Int]])

    val user = User(Right("John"), Right(30))
    val userPatch1 = UserPatch(None, None)
     
    pprint.pprintln(
      user.patchUsing(userPatch1)
    )
    // clears both fields:
    // expected output:
    // User(name = Right(value = "John"), age = Right(value = 30))

    val userPatch2 = UserPatch(Some(Right("Jane")), Some(Right(25)))

    pprint.pprintln(
      user.patchUsing(userPatch2)
    )
    // ignores updating both fields:
    // expected output:
    // User(name = Right(value = "Jane"), age = Right(value = 25))
    ```

## Updating value with collection

By default patch always just replaces the old value with a new one:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class UserStats(names: List[String], ages: List[Int])
    case class UserStatsPatch(names: Vector[String], ages: Vector[Int])

    val user = UserStats(List("John"), List(30))
    val userPatch = UserStatsPatch(Vector("Jane"), Vector(25))

    pprint.pprintln(
      user.patchUsing(userPatch)
    )
    // expected output:
    // UserStats(names = List("Jane"), ages = List(25))
    ```

### Appending to collection instead of replacing it

If the original object contains a collection, and the patch comes with another one`, we can do two things:

  - replace the old one with a new one
  - or append the value from patch to the existing value

Both choices may make perfect sense, depending on the context. By default, Chimney does the former (replaces the value),
but it also gives a simple way to append collection to the old value.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class UserStats(names: List[String], ages: List[Int])
    case class UserStatsPatch(names: Vector[String], ages: Vector[Int])

    val user = UserStats(List("John"), List(30))
    val userPatch = UserStatsPatch(Vector("Jane"), Vector(25))
     
    pprint.pprintln(
      user.patchUsing(userPatch)
    )
    // fails both fields:
    // expected output:
    // UserStats(names = List("Jane"), ages = List(25))

    pprint.pprintln(
      user
        .using(userPatch)
        .appendCollectionInPatch
        .patch
    )
    // ignores updating both fields:
    // expected output:
    // UserStats(names = List("John", "Jane"), ages = List(30, 25))

    locally {
      // All patching derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3)
      implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

      pprint.pprintln(
        user.patchUsing(userPatch)
      )
      // ignores updating both fields:
      // expected output:
      // UserStats(names = List("John", "Jane"), ages = List(30, 25))
    }
    ```

If the flag was enabled in the implicit config it can be disabled with `.overrideCollectionInPatch`.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class UserStats(names: List[String], ages: List[Int])
    case class UserStatsPatch(names: Vector[String], ages: Vector[Int])

    val user = UserStats(List("John"), List(30))
    val userPatch = UserStatsPatch(Vector("Jane"), Vector(25))

    // all patching derived in this scope will see these new flags
    implicit val cfg = PatcherConfiguration.default.appendCollectionInPatch

    pprint.pprintln(
      user
        .using(userPatch)
        .overrideCollectionInPatch
        .patch
    )
    // clears both fields:
    // expected output:
    // UserStats(names = List("Jane"), ages = List(25))
    ```
 
### Unambiguous collection update

The flag is not required when updating collection with `Option[collection]`, as then it is always
unambiguous what to do (leave unchanged or replace):

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class UserStats(names: List[String], ages: List[Int])
    case class UserStatsPatch(names: Option[Vector[String]], ages: Option[Vector[Int]])

    val user = UserStats(List("John"), List(30))
    val userPatch1 = UserStatsPatch(None, None)
     
    pprint.pprintln(
      user.patchUsing(userPatch1)
    )
    // clears both fields:
    // expected output:
    // UserStats(names = List("John"), ages = List(30))

    val userPatch2 = UserStatsPatch(Some(Vector("Jane")), Some(Vector(25)))

    pprint.pprintln(
      user.patchUsing(userPatch2)
    )
    // ignores updating both fields:
    // expected output:
    // UserStats(names = List("Jane"), ages = List(25))
    ```
