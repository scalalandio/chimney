package io.scalaland.chimney.example.internal

import io.scalaland.chimney.example.MyTypeClass
import io.scalaland.chimney.internal.compiletime.DerivationEngine

trait MyTypeClassDerivation extends DerivationEngine {

  // example of platform-independent type definition
  protected val MyTypes: MyTypesModule
  protected trait MyTypesModule { this: MyTypes.type =>

    // Provides
    //   - MyTypeClass.apply[From, To]: Type[MyTypeClass[From, To]]
    //   - MyTypeClass.unapply(tpe: Type[Any]): Option[(??, ??)] // existential types
    val MyTypeClass: MyTypeClassModule
    trait MyTypeClassModule extends Type.Ctor2[MyTypeClass] { this: MyTypeClass.type => }

    // use in platform-independent code (it cannot generate Type instances, as opposed to Scala 2/Scala 3 macros)
    object Implicits {

      implicit def MyTypeClassType[From: Type, To: Type]: Type[MyTypeClass[From, To]] = MyTypeClass[From, To]
    }
  }

  // example of platform-independent expr utility
  protected val MyExprs: MyExprsModule
  protected trait MyExprsModule { this: MyExprs.type =>

    import MyTypes.Implicits.*

    def callMyTypeClass[From: Type, To: Type](tc: Expr[MyTypeClass[From, To]], from: Expr[From]): Expr[To]

    def createTypeClass[From: Type, To: Type](body: Expr[From] => Expr[To]): Expr[MyTypeClass[From, To]]

    def summonMyTypeClass[From: Type, To: Type]: Option[Expr[MyTypeClass[From, To]]] =
      Expr.summonImplicit[MyTypeClass[From, To]]

    // use in platform-independent code (since it does not have quotes nor quasiquotes)
    object Implicits {

      implicit class MyTypeClassOps[From: Type, To: Type](private val tc: Expr[MyTypeClass[From, To]]) {

        def convert(from: Expr[From]): Expr[To] = callMyTypeClass(tc, from)
      }
    }
  }

  import MyExprs.Implicits.*

  // example of a platform-independent Rule
  object MyTypeClassImplicitRule extends Rule("MyTypeClassImplicit") {

    override def expand[From, To](implicit
        ctx: TransformationContext[From, To]
    ): DerivationResult[Rule.ExpansionResult[To]] =
      MyExprs.summonMyTypeClass[From, To] match {
        case Some(myTypeClass) => DerivationResult.expandedTotal(myTypeClass.convert(ctx.src))
        case None              => DerivationResult.attemptNextRule
      }
  }

  def myTypeClassDerivation[From: Type, To: Type]: Expr[MyTypeClass[From, To]] =
    MyExprs.createTypeClass[From, To] { (from: Expr[From]) =>
      val cfg = TransformerConfiguration() // customize, read config with DSL etc
      val context = TransformationContext.ForTotal.create[From, To](from, cfg)

      deriveFinalTransformationResultExpr(context).toEither.fold(
        derivationErrors => reportError(derivationErrors.toString), // customize
        identity
      )
    }
}
