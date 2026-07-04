package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.javafixtures.*
import io.scalaland.chimney.partial.syntax.*
import io.scalaland.chimney.utils.OptionUtils.*

/** Proves that Hearth's JVM-only built-in std providers serve `java.*` types through the engine's extension-fallback
  * layer WITHOUT any import and WITHOUT any flag:
  *   - the 8 Java boxed primitives via `IsValueTypeProviderForJava*` (surfaced through the ungated `ValueClassType` -
  *     this replaces chimney-java-collections' `JavaPrimitivesImplicits`),
  *   - `java.util.EnumSet`/`java.util.EnumMap` via the java-collection providers (served directly by Hearth since the
  *     hearth#321/#322/#323/#324 fixes - the engine's `JavaCollectionsPlatformCompat` replacement layer is gone),
  *   - `java.util.Map` targets, whose provider emits JDK 9+ `java.util.Map.entry` - only compilable on Scala 2.13 since
  *     the `-release 11` baseline bump.
  */
class HearthStdJavaTypesSpec extends ChimneySpec {

  import HearthStdJavaTypesSpec.*

  group("Java boxed primitives via Hearth IsValueType built-ins (no imports, no flags)") {

    test("transform primitives into their boxed forms and back (total)") {
      true.transformInto[java.lang.Boolean] ==> java.lang.Boolean.TRUE
      java.lang.Boolean.TRUE.transformInto[Boolean] ==> true
      (1: Byte).transformInto[java.lang.Byte] ==> java.lang.Byte.valueOf(1: Byte)
      java.lang.Byte.valueOf(1: Byte).transformInto[Byte] ==> (1: Byte)
      'a'.transformInto[java.lang.Character] ==> java.lang.Character.valueOf('a')
      java.lang.Character.valueOf('a').transformInto[Char] ==> 'a'
      (2: Short).transformInto[java.lang.Short] ==> java.lang.Short.valueOf(2: Short)
      java.lang.Short.valueOf(2: Short).transformInto[Short] ==> (2: Short)
      3.transformInto[java.lang.Integer] ==> java.lang.Integer.valueOf(3)
      java.lang.Integer.valueOf(3).transformInto[Int] ==> 3
      4L.transformInto[java.lang.Long] ==> java.lang.Long.valueOf(4L)
      java.lang.Long.valueOf(4L).transformInto[Long] ==> 4L
      5.0f.transformInto[java.lang.Float] ==> java.lang.Float.valueOf(5.0f)
      java.lang.Float.valueOf(5.0f).transformInto[Float] ==> 5.0f
      6.0.transformInto[java.lang.Double] ==> java.lang.Double.valueOf(6.0)
      java.lang.Double.valueOf(6.0).transformInto[Double] ==> 6.0
    }

    test("transform primitives into their boxed forms and back (partial)") {
      true.transformIntoPartial[java.lang.Boolean].asOption ==> Some(java.lang.Boolean.TRUE)
      java.lang.Boolean.TRUE.transformIntoPartial[Boolean].asOption ==> Some(true)
      (1: Byte).transformIntoPartial[java.lang.Byte].asOption ==> Some(java.lang.Byte.valueOf(1: Byte))
      java.lang.Byte.valueOf(1: Byte).transformIntoPartial[Byte].asOption ==> Some(1: Byte)
      'a'.transformIntoPartial[java.lang.Character].asOption ==> Some(java.lang.Character.valueOf('a'))
      java.lang.Character.valueOf('a').transformIntoPartial[Char].asOption ==> Some('a')
      (2: Short).transformIntoPartial[java.lang.Short].asOption ==> Some(java.lang.Short.valueOf(2: Short))
      java.lang.Short.valueOf(2: Short).transformIntoPartial[Short].asOption ==> Some(2: Short)
      3.transformIntoPartial[java.lang.Integer].asOption ==> Some(java.lang.Integer.valueOf(3))
      java.lang.Integer.valueOf(3).transformIntoPartial[Int].asOption ==> Some(3)
      4L.transformIntoPartial[java.lang.Long].asOption ==> Some(java.lang.Long.valueOf(4L))
      java.lang.Long.valueOf(4L).transformIntoPartial[Long].asOption ==> Some(4L)
      5.0f.transformIntoPartial[java.lang.Float].asOption ==> Some(java.lang.Float.valueOf(5.0f))
      java.lang.Float.valueOf(5.0f).transformIntoPartial[Float].asOption ==> Some(5.0f)
      6.0.transformIntoPartial[java.lang.Double].asOption ==> Some(java.lang.Double.valueOf(6.0))
      java.lang.Double.valueOf(6.0).transformIntoPartial[Double].asOption ==> Some(6.0)
    }

    test("transform boxed primitives inside products (total + partial, both directions)") {
      val primitives = Primitives(b = true, bt = 1, c = 'a', s = 2, i = 3, l = 4L, f = 5.0f, d = 6.0)
      val boxed = Boxed(
        b = java.lang.Boolean.TRUE,
        bt = java.lang.Byte.valueOf(1: Byte),
        c = java.lang.Character.valueOf('a'),
        s = java.lang.Short.valueOf(2: Short),
        i = java.lang.Integer.valueOf(3),
        l = java.lang.Long.valueOf(4L),
        f = java.lang.Float.valueOf(5.0f),
        d = java.lang.Double.valueOf(6.0)
      )

      primitives.transformInto[Boxed] ==> boxed
      boxed.transformInto[Primitives] ==> primitives
      primitives.transformIntoPartial[Boxed].asOption ==> Some(boxed)
      boxed.transformIntoPartial[Primitives].asOption ==> Some(primitives)
    }

    test("user-provided implicit Transformer overrides the built-in boxed-primitive support") {
      implicit val marker: Transformer[Int, java.lang.Integer] = (src: Int) => java.lang.Integer.valueOf(src + 100)

      1.transformInto[java.lang.Integer] ==> java.lang.Integer.valueOf(101)
    }
  }

  group("java.util.EnumSet/EnumMap via Hearth IsCollection built-ins") {

    test("transform into and from java.util.EnumSet") {
      Set(jcolors1.Color.Red, jcolors1.Color.Blue)
        .transformInto[java.util.EnumSet[jcolors1.Color]] ==> java.util.EnumSet
        .of(jcolors1.Color.Red, jcolors1.Color.Blue)
      List(jcolors1.Color.Green).transformInto[java.util.EnumSet[jcolors1.Color]] ==> java.util.EnumSet.of(
        jcolors1.Color.Green
      )
      java.util.EnumSet.of(jcolors1.Color.Red, jcolors1.Color.Blue).transformInto[Set[jcolors1.Color]] ==> Set(
        jcolors1.Color.Red,
        jcolors1.Color.Blue
      )
      java.util.EnumSet.of(jcolors1.Color.Red).transformIntoPartial[List[jcolors1.Color]].asOption ==> Some(
        List(jcolors1.Color.Red)
      )
      Set(jcolors1.Color.Red).transformIntoPartial[java.util.EnumSet[jcolors1.Color]].asOption ==> Some(
        java.util.EnumSet.of(jcolors1.Color.Red)
      )
    }

    test("transform into and from java.util.EnumMap (pre-compiled enum)") {
      import java.util.concurrent.TimeUnit

      val expected = new java.util.EnumMap[TimeUnit, Int](classOf[TimeUnit])
      expected.put(TimeUnit.SECONDS, 1)
      expected.put(TimeUnit.HOURS, 2)

      Map(TimeUnit.SECONDS -> 1, TimeUnit.HOURS -> 2).transformInto[java.util.EnumMap[TimeUnit, Int]] ==> expected
      expected.transformInto[Map[TimeUnit, Int]] ==> Map(TimeUnit.SECONDS -> 1, TimeUnit.HOURS -> 2)
      Map(TimeUnit.SECONDS -> 1, TimeUnit.HOURS -> 2)
        .transformIntoPartial[java.util.EnumMap[TimeUnit, Int]]
        .asOption ==> Some(expected)
    }

    test("java.util.EnumSet also works with pre-compiled enums") {
      import java.util.concurrent.TimeUnit

      Set(TimeUnit.SECONDS, TimeUnit.HOURS).transformInto[java.util.EnumSet[TimeUnit]] ==> java.util.EnumSet.of(
        TimeUnit.SECONDS,
        TimeUnit.HOURS
      )
      java.util.EnumSet.of(TimeUnit.SECONDS).transformInto[List[TimeUnit]] ==> List(TimeUnit.SECONDS)
    }

    test("java.util.EnumMap of a SAME-COMPILATION-UNIT enum works (hearth#323 fixed: symbolic enum detection)") {
      // Used to be a pinned Hearth 0.4.0 limitation: the EnumMap provider branch gated on macro-time `Class.forName`
      // of the enum, which never matched enums compiled in the same run (jcolors1.Color here). Since hearth#323 the
      // detection is symbolic, so same-unit enums work like pre-compiled ones.
      val expected = new java.util.EnumMap[jcolors1.Color, Int](classOf[jcolors1.Color])
      expected.put(jcolors1.Color.Red, 1)

      Map(jcolors1.Color.Red -> 1).transformInto[java.util.EnumMap[jcolors1.Color, Int]] ==> expected
      Map(jcolors1.Color.Red -> 1)
        .transformIntoPartial[java.util.EnumMap[jcolors1.Color, Int]]
        .asOption ==> Some(expected)
    }
  }

  group("java.util.Map targets (JDK 9+ Map.entry emission - the -release 11 baseline)") {

    test("transform scala.collection.Map into java.util.Map (total + partial incl. error paths)") {
      val expected: java.util.Map[String, Int] = {
        val map = new java.util.HashMap[String, Int]()
        map.put("a", 1)
        map.put("b", 2)
        map
      }

      Map("a" -> 1, "b" -> 2).transformInto[java.util.Map[String, Int]] ==> expected
      Map("a" -> 1, "b" -> 2).transformIntoPartial[java.util.Map[String, Int]].asOption ==> Some(expected)

      implicit val intParserOpt: PartialTransformer[String, Int] = PartialTransformer(_.parseInt.asResult)
      Map("a" -> "1", "b" -> "2").transformIntoPartial[java.util.Map[String, Int]].asOption ==> Some(expected)
      // failure is reported at the failing key
      Map("a" -> "x").transformIntoPartial[java.util.Map[String, Int]].asErrorPathMessageStrings ==> Iterable(
        "(a)" -> "empty value"
      )
    }
  }
}
object HearthStdJavaTypesSpec {

  case class Primitives(b: Boolean, bt: Byte, c: Char, s: Short, i: Int, l: Long, f: Float, d: Double)
  case class Boxed(
      b: java.lang.Boolean,
      bt: java.lang.Byte,
      c: java.lang.Character,
      s: java.lang.Short,
      i: java.lang.Integer,
      l: java.lang.Long,
      f: java.lang.Float,
      d: java.lang.Double
  )
}
