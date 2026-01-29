package io.scalaland.chimney.example.internal

import io.scalaland.chimney.example.MyTypeClass
import io.scalaland.chimney.internal.compiletime.{DerivationEnginePlatform, StandardRules}

/** Scala-3-specifc implementations */
abstract class MyTypeClassDerivationPlatform(q: scala.quoted.Quotes)
    extends DerivationEnginePlatform(q)
    with MyTypeClassDerivation
    with StandardRules {

  // in Scala-3-specific code, remember to import content of the quotes and quotes.reflect
  import q.*, q.reflect.*

  protected object MyTypes extends MyTypesModule {

    object MyTypeClass extends MyTypeClassModule {
      def apply[From: Type, To: Type]: Type[MyTypeClass[From, To]] = scala.quoted.Type.of[MyTypeClass[From, To]]
      def unapply[A](A: Type[A]): Option[(??, ??)] = A match {
        case '[MyTypeClass[from, to]] => Some((Type[from].as_??, Type[to].as_??))
        case _                        => None
      }
    }
  }

  protected object MyExprs extends MyExprsModule {

    def callMyTypeClass[From: Type, To: Type](tc: Expr[MyTypeClass[From, To]], from: Expr[From]): Expr[To] =
      '{ $tc.convert($from) }

    def createTypeClass[From: Type, To: Type](body: Expr[From] => Expr[To]): Expr[MyTypeClass[From, To]] =
      '{ new MyTypeClass[From, To] { def convert(from: From): To = ${ body('from) } } }
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
