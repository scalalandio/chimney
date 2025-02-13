package io.scalaland.chimney.benchmarks

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.*

object fixtures {

  case class Simple(a: Int, b: Double, c: String, d: Option[String])
  case class SimpleOutput(a: Int, b: Double, c: String, d: Option[String])

  // format: off
  case class Large(
    a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int,
    k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int,
    u: Int, v: Int
  )

  case class LargeOutput(
    a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int,
    k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int,
    u: Int, v: Int
  )
  case class LargeRenamedOutput(
    a$: Int, b$: Int, c$: Int, d$: Int, e$: Int, f$: Int, g$: Int, h$: Int, i$: Int, j$: Int,
    k$: Int, l$: Int, m$: Int, n$: Int, o$: Int, p$: Int, q$: Int, r$: Int, s$: Int, t$: Int,
    u$: Int, v$: Int
  )

  case class Huge(
    a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int,
    k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int,
    u: Int, v: Int, w: Int, x: Int, y: Int, z: Int, aa: Int, ab: Int, ac: Int, ad: Int,
    ae: Int, af: Int, ag: Int, ah: Int, ai: Int, aj: Int, ak: Int, al: Int, am: Int, an: Int,
    ao: Int, ap: Int, aq: Int, ar: Int, as: Int, at: Int, au: Int, av: Int, aw: Int, ax: Int,
    ay: Int, az: Int, ba: Int, bb: Int, bc: Int, bd: Int, be: Int, bf: Int, bg: Int, bh: Int,
    bi: Int, bj: Int, bk: Int, bl: Int, bm: Int, bn: Int, bo: Int, bp: Int, bq: Int, br: Int,
    bs: Int, bt: Int, bu: Int, bv: Int, bw: Int, bx: Int, by: Int, bz: Int, ca: Int, cb: Int,
    cc: Int, cd: Int, ce: Int, cf: Int, cg: Int, ch: Int, ci: Int, cj: Int, ck: Int, cl: Int,
    cm: Int, cn: Int, co: Int, cp: Int, cq: Int, cr: Int, cs: Int, ct: Int, cu: Int, cv: Int
  )

  case class HugeOutput(
    a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int,
    k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int,
    u: Int, v: Int, w: Int, x: Int, y: Int, z: Int, aa: Int, ab: Int, ac: Int, ad: Int,
    ae: Int, af: Int, ag: Int, ah: Int, ai: Int, aj: Int, ak: Int, al: Int, am: Int, an: Int,
    ao: Int, ap: Int, aq: Int, ar: Int, as: Int, at: Int, au: Int, av: Int, aw: Int, ax: Int,
    ay: Int, az: Int, ba: Int, bb: Int, bc: Int, bd: Int, be: Int, bf: Int, bg: Int, bh: Int,
    bi: Int, bj: Int, bk: Int, bl: Int, bm: Int, bn: Int, bo: Int, bp: Int, bq: Int, br: Int,
    bs: Int, bt: Int, bu: Int, bv: Int, bw: Int, bx: Int, by: Int, bz: Int, ca: Int, cb: Int,
    cc: Int, cd: Int, ce: Int, cf: Int, cg: Int, ch: Int, ci: Int, cj: Int, ck: Int, cl: Int,
    cm: Int, cn: Int, co: Int, cp: Int, cq: Int, cr: Int, cs: Int, ct: Int, cu: Int, cv: Int
  )

  case class Complex(a: String, b: Int, c: Nested)
  case class Nested(x: Double, y: Double, z: NestedDeeper)
  case class NestedDeeper(a: String, b: String)

  object samples {
    final val simpleSample = Simple(23, 23d, "23", None)
    final val simpleSampleArray: Array[Simple] = Array.fill(100)(samples.simpleSample)
    final val simpleSampleVector: Vector[Simple] = Vector.fill(100)(samples.simpleSample)
    final val simpleSampleMapOfStrings: Map[String, Simple] = (1 to 100).map(i => i.toString -> samples.simpleSample).toMap
    final val simpleSampleRight: Either[String, Simple] = Right(samples.simpleSample)

    final val largeSample = Large(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 22
    )
    final val largeNestedSample = Array.tabulate(200) { i =>
      Large(
        i + 1, i + 2, i + 3, i + 4, i + 5, i + 6, i + 7, i + 8, i + 9, i + 10,
        i + 11, i + 12, i + 13, i + 14, i + 15, i + 16, i + 17, i + 18, i + 19, i + 20,
        i + 21, i + 22
      )
    }
    final val hugeSample = Huge(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
      21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
      31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
      41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
      51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
      61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
      71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
      81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
      91, 92, 93, 94, 95, 96, 97, 98, 99, 100
    )

    final val complexSample = Complex("whatever", 23, Nested(0, 0, NestedDeeper("not", "used")))

    object rich {
      import fixtures.rich._
      final val person = Person(PersonId(10), PersonName("Bill"), 30)
    }

    object validation {
      object nonFhappy {
        def validateA(a: Int): Int = a
        def validateB(b: Double): Double = b
        def validateC(c: String): String = c
        def validateD(d: Option[String]): Option[String] = d
      }

      object nonFunhappy {
        def validateA(a: Int): Int = throw new Exception("a not nice")
        def validateB(b: Double): Double = b
        def validateC(c: String): String = throw new Exception("c not pretty")
        def validateD(d: Option[String]): Option[String] = throw new Exception("I don't like this d")
      }

      object happy {
        def validateA(a: Int): Either[String, Int] = Right(a)
        def validateB(b: Double): Either[String, Double] = Right(b)
        def validateC(c: String): Either[String, String] = Right(c)
        def validateD(d: Option[String]): Either[String, Option[String]] = Right(d)
        def squareInt(a: Int): Either[String, Int] = Right(a * a)
      }

      object unhappy {
        def validateA(a: Int): Either[String, Int] = Left("a not nice")
        def validateB(b: Double): Either[String, Double] = Right(b)
        def validateC(c: String): Either[String, String] = Left("c not pretty")
        def validateD(d: Option[String]): Either[String, Option[String]] = Left("I don't like this d")
        def squareIntWhenOdd(a: Int): Either[String, Int] = if(a % 2 == 1) Right(a * a) else Left(s"$a is not an odd number")
      }
    }
  }

  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
  }

  sealed trait Channel
  object Channel {
    case object Alpha extends Channel
    case object Blue extends Channel
    case object Green extends Channel
    case object Red extends Channel
  }

  object rich {
    case class PersonId(id: Int) extends AnyVal
    case class PersonName(name: String) extends AnyVal
    case class Person(personId: PersonId, personName: PersonName, age: Int)
  }
  object plain {
    case class Person(personId: Int, personName: String, age: Int)
  }

  final def doSimpleByHand(sample: Simple): SimpleOutput =
    new SimpleOutput(sample.a, sample.b, sample.c, sample.d)

  final def doLargeByHand(sample: Large): LargeOutput =
    new LargeOutput(
      sample.a, sample.b, sample.c, sample.d, sample.e, sample.f, sample.g, sample.h, sample.i, sample.j,
      sample.k, sample.l, sample.m, sample.n, sample.o, sample.p, sample.q, sample.r, sample.s, sample.t,
      sample.u, sample.v
    )

  final def doLargeRenameByHand(sample: Large): LargeRenamedOutput =
    new LargeRenamedOutput(
      sample.a, sample.b, sample.c, sample.d, sample.e, sample.f, sample.g, sample.h, sample.i, sample.j,
      sample.k, sample.l, sample.m, sample.n, sample.o, sample.p, sample.q, sample.r, sample.s, sample.t,
      sample.u, sample.v
    )

  final def doLargeByHandComputed(sample: Large): LargeOutput =
    new LargeOutput(
      sample.a * 2, sample.b * 2, sample.c * 2, sample.d * 2, sample.e * 2, sample.f * 2, sample.g * 2, sample.h * 2, sample.i * 2, sample.j * 2,
      sample.k * 2, sample.l * 2, sample.m * 2, sample.n * 2, sample.o * 2, sample.p * 2, sample.q * 2, sample.r * 2, sample.s * 2, sample.t * 2,
      sample.u * 2, sample.v * 2
    )

  final def doLargeByHandConst(sample: Large): LargeOutput =
    new LargeOutput(
      834, 834, 834, 834, 834, 834, 834, 834, 834, 834,
      834, 834, 834, 834, 834, 834, 834, 834, 834, 834,
      834, 834
    )

  final def doHugeByHand(sample: Huge): HugeOutput =
    new HugeOutput(
      sample.a, sample.b, sample.c, sample.d, sample.e, sample.f, sample.g, sample.h, sample.i, sample.j,
      sample.k, sample.l, sample.m, sample.n, sample.o, sample.p, sample.q, sample.r, sample.s, sample.t,
      sample.u, sample.v, sample.w, sample.x, sample.y, sample.z, sample.aa, sample.ab, sample.ac, sample.ad,
      sample.ae, sample.af, sample.ag, sample.ah, sample.ai, sample.aj, sample.ak, sample.al, sample.am, sample.an,
      sample.ao, sample.ap, sample.aq, sample.ar, sample.as, sample.at, sample.au, sample.av, sample.aw, sample.ax,
      sample.ay, sample.az, sample.ba, sample.bb, sample.bc, sample.bd, sample.be, sample.bf, sample.bg, sample.bh,
      sample.bi, sample.bj, sample.bk, sample.bl, sample.bm, sample.bn, sample.bo, sample.bp, sample.bq, sample.br,
      sample.bs, sample.bt, sample.bu, sample.bv, sample.bw, sample.bx, sample.by, sample.bz, sample.ca, sample.cb,
      sample.cc, sample.cd, sample.ce, sample.cf, sample.cg, sample.ch, sample.ci, sample.cj, sample.ck, sample.cl,
      sample.cm, sample.cn, sample.co, sample.cp, sample.cq, sample.cr, sample.cs, sample.ct, sample.cu, sample.cv
    )

  // format: on

  final def color2Channel(color: Color): Channel = color match {
    case Color.Blue  => Channel.Blue
    case Color.Green => Channel.Green
    case Color.Red   => Channel.Red
  }

  final def channel2Color(channel: Channel): Color = channel match {
    case Channel.Alpha => Color.Blue
    case Channel.Blue  => Color.Blue
    case Channel.Green => Color.Green
    case Channel.Red   => Color.Red
  }

  final def plainToRich(person: plain.Person): rich.Person =
    rich.Person(rich.PersonId(person.personId), rich.PersonName(person.personName), person.age)

  final def richToPlain(person: rich.Person): plain.Person =
    plain.Person(person.personId.id, person.personName.name, person.age)

  type M[+A] = Either[Vector[partial.Error], A]

  final def simpleByHandErrorAccEitherSwap(
      simple: Simple,
      fa: Int => M[Int],
      fb: Double => M[Double],
      fc: String => M[String],
      fd: Option[String] => M[Option[String]]
  ): M[SimpleOutput] = {
    val valA = fa(simple.a)
    val valB = fb(simple.b)
    val valC = fc(simple.c)
    val valD = fd(simple.d)

    if (valA.isRight && valB.isRight && valC.isRight && valD.isRight) {
      Right(SimpleOutput(valA.toOption.get, valB.toOption.get, valC.toOption.get, valD.toOption.get))
    } else {
      val errsB = Vector.newBuilder[partial.Error]
      errsB ++= valA.swap.getOrElse(Vector.empty)
      errsB ++= valB.swap.getOrElse(Vector.empty)
      errsB ++= valC.swap.getOrElse(Vector.empty)
      errsB ++= valD.swap.getOrElse(Vector.empty)
      Left(errsB.result())
    }
  }

  final def simpleByHandErrorAccCrazyNesting(
      simple: Simple,
      fa: Int => M[Int],
      fb: Double => M[Double],
      fc: String => M[String],
      fd: Option[String] => M[Option[String]]
  ): M[SimpleOutput] =
    fa(simple.a) match {
      case Right(a) =>
        fb(simple.b) match {
          case Right(b) =>
            fc(simple.c) match {
              case Right(c) =>
                fd(simple.d) match {
                  case Right(d)         => Right(SimpleOutput(a, b, c, d))
                  case retVal @ Left(_) => retVal.asInstanceOf[M[SimpleOutput]]
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs3)
                  case Left(errs4) => Left(errs3 ++ errs4)
                }
            }
          case Left(errs2) =>
            fc(simple.c) match {
              case Right(_) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs2)
                  case Left(errs4) => Left(errs2 ++ errs4)
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs2 ++ errs3)
                  case Left(errs4) => Left(errs2 ++ errs3 ++ errs4)
                }
            }
        }
      case Left(errs1) =>
        fb(simple.b) match {
          case Right(_) =>
            fc(simple.c) match {
              case Right(_) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs1)
                  case Left(errs4) => Left(errs1 ++ errs4)
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs3)
                  case Left(errs4) => Left(errs1 ++ errs3 ++ errs4)
                }
            }
          case Left(errs2) =>
            fc(simple.c) match {
              case Right(_) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs1 ++ errs2)
                  case Left(errs4) => Left(errs1 ++ errs2 ++ errs4)
                }
              case Left(errs3) =>
                fd(simple.d) match {
                  case Right(_)    => Left(errs1 ++ errs2 ++ errs3)
                  case Left(errs4) => Left(errs1 ++ errs2 ++ errs3 ++ errs4)
                }
            }
        }
    }

  object transformers {

    final val simpleTransformerPartialHappy: PartialTransformer[Simple, SimpleOutput] = {
      import samples.validation.*
      PartialTransformer
        .define[Simple, SimpleOutput]
        .withFieldComputedPartial(_.a, s => happy.validateA(s.a).asResult)
        .withFieldComputedPartial(_.b, s => happy.validateB(s.b).asResult)
        .withFieldComputedPartial(_.c, s => happy.validateC(s.c).asResult)
        .withFieldComputedPartial(_.d, s => happy.validateD(s.d).asResult)
        .buildTransformer
    }

    final val simpleTransformerPartialUnhappy: PartialTransformer[Simple, SimpleOutput] = {
      import samples.validation.*
      PartialTransformer
        .define[Simple, SimpleOutput]
        .withFieldComputedPartial(_.a, s => unhappy.validateA(s.a).asResult)
        .withFieldComputedPartial(_.b, s => unhappy.validateB(s.b).asResult)
        .withFieldComputedPartial(_.c, s => unhappy.validateC(s.c).asResult)
        .withFieldComputedPartial(_.d, s => unhappy.validateD(s.d).asResult)
        .buildTransformer
    }

    final val largeTransformerPartialHappy: PartialTransformer[Large, LargeOutput] = {
      import samples.validation.*
      PartialTransformer
        .define[Large, LargeOutput]
        .withFieldComputedPartial(_.a, s => happy.squareInt(s.a).asResult)
        .withFieldComputedPartial(_.b, s => happy.squareInt(s.b).asResult)
        .withFieldComputedPartial(_.c, s => happy.squareInt(s.c).asResult)
        .withFieldComputedPartial(_.d, s => happy.squareInt(s.d).asResult)
        .withFieldComputedPartial(_.e, s => happy.squareInt(s.e).asResult)
        .withFieldComputedPartial(_.f, s => happy.squareInt(s.f).asResult)
        .withFieldComputedPartial(_.g, s => happy.squareInt(s.g).asResult)
        .withFieldComputedPartial(_.h, s => happy.squareInt(s.h).asResult)
        .withFieldComputedPartial(_.i, s => happy.squareInt(s.i).asResult)
        .withFieldComputedPartial(_.j, s => happy.squareInt(s.j).asResult)
        .withFieldComputedPartial(_.k, s => happy.squareInt(s.k).asResult)
        .withFieldComputedPartial(_.l, s => happy.squareInt(s.l).asResult)
        .withFieldComputedPartial(_.m, s => happy.squareInt(s.m).asResult)
        .withFieldComputedPartial(_.n, s => happy.squareInt(s.n).asResult)
        .withFieldComputedPartial(_.o, s => happy.squareInt(s.o).asResult)
        .withFieldComputedPartial(_.p, s => happy.squareInt(s.p).asResult)
        .withFieldComputedPartial(_.q, s => happy.squareInt(s.q).asResult)
        .withFieldComputedPartial(_.r, s => happy.squareInt(s.r).asResult)
        .withFieldComputedPartial(_.s, s => happy.squareInt(s.s).asResult)
        .withFieldComputedPartial(_.t, s => happy.squareInt(s.t).asResult)
        .withFieldComputedPartial(_.u, s => happy.squareInt(s.u).asResult)
        .withFieldComputedPartial(_.v, s => happy.squareInt(s.v).asResult)
        .buildTransformer
    }

    final val largeTransformerPartialUnhappy: PartialTransformer[Large, LargeOutput] = {
      import samples.validation.*
      PartialTransformer
        .define[Large, LargeOutput]
        .withFieldComputedPartial(_.a, s => unhappy.squareIntWhenOdd(s.a).asResult)
        .withFieldComputedPartial(_.b, s => unhappy.squareIntWhenOdd(s.b).asResult)
        .withFieldComputedPartial(_.c, s => unhappy.squareIntWhenOdd(s.c).asResult)
        .withFieldComputedPartial(_.d, s => unhappy.squareIntWhenOdd(s.d).asResult)
        .withFieldComputedPartial(_.e, s => unhappy.squareIntWhenOdd(s.e).asResult)
        .withFieldComputedPartial(_.f, s => unhappy.squareIntWhenOdd(s.f).asResult)
        .withFieldComputedPartial(_.g, s => unhappy.squareIntWhenOdd(s.g).asResult)
        .withFieldComputedPartial(_.h, s => unhappy.squareIntWhenOdd(s.h).asResult)
        .withFieldComputedPartial(_.i, s => unhappy.squareIntWhenOdd(s.i).asResult)
        .withFieldComputedPartial(_.j, s => unhappy.squareIntWhenOdd(s.j).asResult)
        .withFieldComputedPartial(_.k, s => unhappy.squareIntWhenOdd(s.k).asResult)
        .withFieldComputedPartial(_.l, s => unhappy.squareIntWhenOdd(s.l).asResult)
        .withFieldComputedPartial(_.m, s => unhappy.squareIntWhenOdd(s.m).asResult)
        .withFieldComputedPartial(_.n, s => unhappy.squareIntWhenOdd(s.n).asResult)
        .withFieldComputedPartial(_.o, s => unhappy.squareIntWhenOdd(s.o).asResult)
        .withFieldComputedPartial(_.p, s => unhappy.squareIntWhenOdd(s.p).asResult)
        .withFieldComputedPartial(_.q, s => unhappy.squareIntWhenOdd(s.q).asResult)
        .withFieldComputedPartial(_.r, s => unhappy.squareIntWhenOdd(s.r).asResult)
        .withFieldComputedPartial(_.s, s => unhappy.squareIntWhenOdd(s.s).asResult)
        .withFieldComputedPartial(_.t, s => unhappy.squareIntWhenOdd(s.t).asResult)
        .withFieldComputedPartial(_.u, s => unhappy.squareIntWhenOdd(s.u).asResult)
        .withFieldComputedPartial(_.v, s => unhappy.squareIntWhenOdd(s.v).asResult)
        .buildTransformer
    }
  }
}
