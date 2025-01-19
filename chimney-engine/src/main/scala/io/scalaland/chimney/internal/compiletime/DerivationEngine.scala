package io.scalaland.chimney.internal.compiletime

/** Chimney derivation engine as API.
  *
  * Intended usage:
  *
  *   1. Implement your own derivation rules in a mixins based on implementation-agnostic API:
  *
  * {{{
  * trait MyOwnImplicitRuleModule { this: DerivationEngine =>
  *
  *   protected object MyOwnImplicitRule extends Rule("MyOwnImplicit") {
  *
  *     def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionRule[To] =
  *       ...
  *   }
  * }
  * }}}
  *
  * (Do the same when you need more types and utility methods shared between 2 different macro implementations.)
  *
  *   2. Mix-in with the implementation for Scala 2/Scala 3 macros:
  *
  * {{{
  * // Scala 2
  * trait MyMacrosImpl
  *     extends DerivationEngineImpl
  *     with StandardRules
  *     with MyOwnImplicitRuleModule {
  *
  *   final override protected val rulesAvailableForPlatform: List[Rule] = List(
  *     MyOwnImplicitRule,
  *     TransformSubtypesRule,
  *     TransformToSingletonRule,
  *     TransformOptionToOptionRule,
  *     TransformPartialOptionToNonOptionRule,
  *     TransformToOptionRule,
  *     TransformValueClassToValueClassRule,
  *     TransformValueClassToTypeRule,
  *     TransformTypeToValueClassRule,
  *     TransformEitherToEitherRule,
  *     TransformMapToMapRule,
  *     TransformIterableToIterableRule,
  *     TransformProductToProductRule,
  *     TransformSealedHierarchyToSealedHierarchyRule
  *   )
  * }
  * }}}
  *
  * {{{
  * // Scala 3
  * abstract class MyMacrosImpl(q: scala.quoted.Quotes)
  *     extends DerivationEngineImpl(q)
  *     with StandardRules
  *     with MyOwnImplicitRuleModule {
  *
  *   final override protected val rulesAvailableForPlatform: List[Rule] = List(
  *     MyOwnImplicitRule,
  *     TransformSubtypesRule,
  *     TransformToSingletonRule,
  *     TransformOptionToOptionRule,
  *     TransformPartialOptionToNonOptionRule,
  *     TransformToOptionRule,
  *     TransformValueClassToValueClassRule,
  *     TransformValueClassToTypeRule,
  *     TransformTypeToValueClassRule,
  *     TransformEitherToEitherRule,
  *     TransformMapToMapRule,
  *     TransformIterableToIterableRule,
  *     TransformProductToProductRule,
  *     TransformSealedHierarchyToSealedHierarchyRule
  *   )
  * }}}
  *
  * (You can provide platform-specific implementations of your types/utilities the same way).
  *
  *   3. Use `deriveFinalTransformationResultExpr` in your macros:
  *
  * {{{
  * // Scala 2
  * import c.universe._
  *
  * def deriveMyTypeClass[
  *     From: c.WeakTypeTag,
  *     To: c.WeakTypeTag
  * ]: c.Expr[MyTypeClass[From, To]] = {
  *
  *   def deriveBody(src: c.Expr[From]): c.Expr[To] = {
  *     val cfg = TransformerConfiguration() // customize, read config with DSL etc
  *     val context = TransformationContext.ForTotal.create(src, cfg)
  *
  *     deriveFinalTransformationResultExpr(context).toEither.fold(
  *       derivationErrors => reportError(derivationErrors.toString), // customize
  *       identity
  *     )
  *   }
  *
  *   val inputName = freshTermName(...)
  *   c.Expr[MyTypeClass[From, To](
  *     q"""
  *     new MyTypeClass[${Type[From]}, ${Type[To]}] {
  *       def encode($inputName: ${Type[From]}): ${Type[To]} = ${deriveBody(c.Expr[From](q"$inputName"))}
  *     }
  *     """
  *   )
  * )
  * }
  * }}}
  *
  * {{{
  * // Scala 3
  * import q.*, q.reflect.*
  *
  * def deriveMyTypeClass[
  *     From: Type,
  *     To: Type
  * ]: Expr[MyTypeClass[From, To]] = {
  *
  *   def deriveBody(src: Expr[From]): Expr[To] = {
  *     val cfg = TransformerConfiguration() // customize, read config with DSL etc
  *     val context = TransformationContext.ForTotal.create(src, cfg)
  *
  *     deriveFinalTransformationResultExpr(context).toEither.fold(
  *       derivationErrors => reportError(derivationErrors.toString), // customize
  *       identity
  *     )
  *   }
  *
  *   '{
  *     new MyTypeClass[From, To] {
  *       def convert(src: From): To = ${deriveBody('toExpr)}
  *     }
  *   }
  * )
  * }
  * }}}
  */
trait DerivationEngine
    extends derivation.transformer.Derivation
    with derivation.transformer.Configurations
    with derivation.transformer.Contexts
    with derivation.transformer.ImplicitSummoning
    with derivation.transformer.ResultOps
    with datatypes.IterableOrArrays
    with datatypes.ProductTypes
    with datatypes.SealedHierarchies
    with datatypes.SingletonTypes
    with datatypes.ValueClasses
    with derivation.transformer.integrations.OptionalValues
    with derivation.transformer.integrations.PartiallyBuildIterables
    with derivation.transformer.integrations.TotallyBuildIterables
    with derivation.transformer.integrations.TotallyOrPartiallyBuildIterables
    with derivation.transformer.rules.TransformationRules {

  type DerivationResult[+A] = io.scalaland.chimney.internal.compiletime.DerivationResult[A]
  val DerivationResult = io.scalaland.chimney.internal.compiletime.DerivationResult

  /** Adapts TransformationExpr[To] to expected type of transformation */
  def deriveFinalTransformationResultExpr[From, To](implicit
      ctx: TransformationContext[From, To]
  ): DerivationResult[Expr[ctx.Target]] =
    DerivationResult.log(s"Start derivation with context: $ctx") >>
      deriveTransformationResultExpr[From, To]
        .map { transformationExpr =>
          ctx.fold(_ => transformationExpr.ensureTotal.asInstanceOf[Expr[ctx.Target]])(_ =>
            transformationExpr.ensurePartial.asInstanceOf[Expr[ctx.Target]]
          )
        }
}
