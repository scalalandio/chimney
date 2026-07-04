package io.scalaland.chimney.chimneyextensiontest

import hearth.MacroCommons
import hearth.fp.effect.MIO
import io.scalaland.chimney.integrations.ChimneyMacroExtension
import io.scalaland.chimney.internal.compiletime.derivation.transformer.ChimneyEngineExtensionApi

/** Test-only Chimney `ChimneyMacroExtension` (Chimney's OWN engine-aware SPI, registered via
  * `META-INF/services/io.scalaland.chimney.integrations.ChimneyMacroExtension`) proving, from a SEPARATELY-COMPILED
  * artifact loaded through `ServiceLoader`, that:
  *
  *   - a special-cased PAIR derives in both a total and a partial context (`Int -> TestSpecialLeaf` total,
  *     `String -> TestSpecialLeaf` partial),
  *   - a handler can build the OUTER layer while DEFERRING N (= 2) inner values to the engine via `deriveInner`
  *     (`TestBox2[A, B] -> TestSpecialBox2[C, D]`, whose inner `A -> C` may itself re-hit this very rule recursively),
  *   - the SPI's precedence holds: a user implicit `Transformer` for the same pair BEATS the handler (asserted in the
  *     spec - the special-cased rule sits below the implicit rules).
  *
  * Implementation mirrors `EngineTestMacroExtension`: cross-quotes live in helper methods with REGULAR type parameters
  * (path-dependent types from `Type.Ctor2.unapply` inside `Expr.quote` break Scala 2's reifier), generic type
  * constructors are matched with `Type.Ctor2.fromUntyped` (safe across compilation-unit boundaries). `Expr`/`Type` are
  * used only through `Expr.quote`/`Expr.splice`/`Type.of` (never in type-annotation position, where `import ctx.*`
  * would shadow them), and `scala.Option` is fully qualified for the same reason.
  */
final class TestChimneyMacroExtension extends ChimneyMacroExtension { loader =>

  override def extend(ctx: MacroCommons & ChimneyEngineExtensionApi): Unit = {
    import ctx.*

    val IntType = Type.of[Int]
    val StringType = Type.of[String]
    val TestSpecialLeafType = Type.of[TestSpecialLeaf]
    val TestBox2Ctor = Type.Ctor2.fromUntyped[TestBox2](Type.Ctor2.of[TestBox2].asUntyped)
    val TestSpecialBox2Ctor = Type.Ctor2.fromUntyped[TestSpecialBox2](Type.Ctor2.of[TestSpecialBox2].asUntyped)

    // Int -> TestSpecialLeaf (total): leaf, no inner derivation.
    def intToLeaf: SpecialCasedTransformation[Int, TestSpecialLeaf] =
      new SpecialCasedTransformation[Int, TestSpecialLeaf] {
        override def specialCase(implicit
            context: SpecialCaseContext[Int, TestSpecialLeaf]
        ): MIO[scala.Option[DerivedExpr[TestSpecialLeaf]]] =
          specialCasedTotal(
            Expr.quote(
              io.scalaland.chimney.chimneyextensiontest.TestSpecialLeaf.wrap(Expr.splice(sourceOf(context)))
            )
          )
      }

    // String -> TestSpecialLeaf (partial): parse, may fail - proves a handler can produce a partial result.
    def stringToLeaf: SpecialCasedTransformation[String, TestSpecialLeaf] =
      new SpecialCasedTransformation[String, TestSpecialLeaf] {
        override def specialCase(implicit
            context: SpecialCaseContext[String, TestSpecialLeaf]
        ): MIO[scala.Option[DerivedExpr[TestSpecialLeaf]]] =
          specialCasedPartial(Expr.quote {
            val s = Expr.splice(sourceOf(context))
            scala.util.Try(s.trim.toInt).toOption match {
              case scala.Some(i) =>
                io.scalaland.chimney.partial.Result.fromValue(
                  io.scalaland.chimney.chimneyextensiontest.TestSpecialLeaf.wrap(i)
                )
              case scala.None => io.scalaland.chimney.partial.Result.fromErrorString("not a number: " + s)
            }
          })
      }

    // TestBox2[A, B] -> TestSpecialBox2[C, D]: build the outer while deferring BOTH inner values (N = 2) to the engine.
    def box2Support[A, B, C, D](implicit
        A: Type[A],
        B: Type[B],
        C: Type[C],
        D: Type[D]
    ): SpecialCasedTransformation[TestBox2[A, B], TestSpecialBox2[C, D]] = {
      implicit val Box: Type[TestSpecialBox2[C, D]] = TestSpecialBox2Ctor[C, D]
      new SpecialCasedTransformation[TestBox2[A, B], TestSpecialBox2[C, D]] {
        override def specialCase(implicit
            context: SpecialCaseContext[TestBox2[A, B], TestSpecialBox2[C, D]]
        ): MIO[scala.Option[DerivedExpr[TestSpecialBox2[C, D]]]] = {
          val src = sourceOf(context)
          val firstExpr = Expr.quote(Expr.splice(src).first)
          val secondExpr = Expr.quote(Expr.splice(src).second)
          // Defer both inner values recursively, then compose (TransformationExpr.flatMap/map thread total/partial).
          deriveInner[A, C](firstExpr).flatMap { (firstDerived: DerivedExpr[C]) =>
            deriveInner[B, D](secondExpr).flatMap { (secondDerived: DerivedExpr[D]) =>
              val combined: DerivedExpr[TestSpecialBox2[C, D]] =
                firstDerived.flatMap[TestSpecialBox2[C, D]] { (cExpr: Expr[C]) =>
                  secondDerived.map[TestSpecialBox2[C, D]] { (dExpr: Expr[D]) =>
                    Expr.quote(
                      io.scalaland.chimney.chimneyextensiontest.TestSpecialBox2
                        .of[C, D](Expr.splice(cExpr), Expr.splice(dExpr))
                    )
                  }
                }
              specialCasedExpr(combined)
            }
          }
        }
      }
    }

    registerSpecialCase(new SpecialCaseHandler {
      override def apply[From, To](implicit
          From: Type[From],
          To: Type[To]
      ): scala.Option[SpecialCasedTransformation[From, To]] = {
        def some[F, T](t: SpecialCasedTransformation[F, T]): scala.Option[SpecialCasedTransformation[From, To]] =
          scala.Some(t.asInstanceOf[SpecialCasedTransformation[From, To]])

        (From, To) match {
          case (TestBox2Ctor(a, b), TestSpecialBox2Ctor(c, d)) =>
            import a.Underlying as A, b.Underlying as B, c.Underlying as C, d.Underlying as D
            some(box2Support[A, B, C, D])
          case _ =>
            if (From =:= IntType && To =:= TestSpecialLeafType) some(intToLeaf)
            else if (From =:= StringType && To =:= TestSpecialLeafType) some(stringToLeaf)
            else scala.None
        }
      }
    })
  }
}
