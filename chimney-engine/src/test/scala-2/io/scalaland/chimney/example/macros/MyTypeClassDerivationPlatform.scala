package io.scalaland.chimney.example.internal

import io.scalaland.chimney.example.MyTypeClass
import io.scalaland.chimney.example.internal.MyTypeClassDerivation
import io.scalaland.chimney.internal.compiletime.{DerivationEnginePlatform, StandardRules}

/** Scala-2-specifc implementations */
trait MyTypeClassDerivationPlatform extends DerivationEnginePlatform with MyTypeClassDerivation with StandardRules {

  // in Scala-2-specific code, remember to import content of the universe
  import c.universe.*

  protected object MyTypes extends MyTypesModule {

    import Type.platformSpecific.*

    object MyTypeClass extends MyTypeClassModule {
      def apply[From: Type, To: Type]: Type[MyTypeClass[From, To]] = weakTypeTag[MyTypeClass[From, To]]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        A.asCtor[MyTypeClass[?, ?]].map(A0 => A0.param(0) -> A0.param(1)) // utility from Type.platformSpecific.*
    }
  }

  protected object MyExprs extends MyExprsModule {

    def callMyTypeClass[From: Type, To: Type](tc: Expr[MyTypeClass[From, To]], from: Expr[From]): Expr[To] =
      c.Expr[To](q"""$tc.convert($from)""")

    def createTypeClass[From: Type, To: Type](body: Expr[From] => Expr[To]): Expr[MyTypeClass[From, To]] = {
      val name = ExprPromise.platformSpecific.freshTermName("from")
      // remember to use full qualified names in Scala 2 macros!!!
      c.Expr[MyTypeClass[From, To]](
        q"""
        new _root_.io.scalaland.chimney.example.MyTypeClass[${Type[From]}, ${Type[To]}] {
          def convert($name: ${Type[From]}): ${Type[To]} = ${body(c.Expr[From](q"$name"))}   
        }
        """
      )
    }
  }

  final override protected val rulesAvailableForPlatform: List[Rule] = List(
    MyTypeClassImplicitRule, // replacing TransformImplicitRule
    TransformSubtypesRule,
    TransformToSingletonRule,
    TransformOptionToOptionRule,
    TransformPartialOptionToNonOptionRule,
    TransformToOptionRule,
    TransformValueClassToValueClassRule,
    TransformValueClassToTypeRule,
    TransformTypeToValueClassRule,
    TransformEitherToEitherRule,
    TransformMapToMapRule,
    TransformIterableToIterableRule,
    TransformProductToProductRule,
    TransformSealedHierarchyToSealedHierarchyRule
  )
}
