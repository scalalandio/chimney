package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Syntax.*
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.partial
import io.scalaland.chimney.partial.Result

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ProductType(Product(fromExtractors, _)), ProductType(Product(_, toConstructors))) =>
          def resolveSource[ToElement: Type](
              name: String,
              defaultValue: Option[Expr[ToElement]]
          ): DerivationResult[TransformationExpr[ToElement]] =
            // TODO:
            // - if override (field computed/const [partial]) exist, use it
            // - else if rename exists, derive recursively for it
            // - else if getter with corresponding name exists, derive recursively for it
            //   - if Java Beans getters are disabled, filter them out from test
            //   - if Java Beans getters are enabled, special case their name comparison
            //   - if accessors are disabled, filter them out from test
            // - else if default value exists and is enabled, use it
            // - else fail
            //   - if there is Java Bean getter that couldn't be used because flag is off, inform about it
            //   - if there is accessor that couldn't be used because flag is off, inform about it
            //   - if default value exists that couldn't be used because flag is off, inform about it
            DerivationResult.notYetImplemented("field value resolution")

          toConstructors match {
            case Product.Constructor.JavaBean(defaultConstructor, setters) =>
              // TODO: if setters empty OR config.settersEnabled continue, otherwise fail with message
              Traverse[List]
                .traverse[DerivationResult, Existential[Product.Setter[To, *]], PartiallyAppliedSetter[To]](setters) {
                  (setter: Existential[Product.Setter[To, *]]) =>
                    Existential.use(setter) { implicit SetterType: Type[setter.Underlying] =>
                      { case Product.Setter(toName, set) =>
                        resolveSource[setter.Underlying](toName, None).map {
                          value: TransformationExpr[setter.Underlying] =>
                            PartiallyAppliedSetter(set, value)
                        }
                      }
                    }
                }
                .flatMap { (resolvedSetters: List[PartiallyAppliedSetter[To]]) =>
                  val (
                    totalSetters: List[Expr[To] => Expr[Unit]],
                    partialSetters: List[Expr[To] => Expr[partial.Result[Unit]]]
                  ) = resolvedSetters.partitionMap {
                    case PartiallyAppliedSetter.Total(set)   => Left(set)
                    case PartiallyAppliedSetter.Partial(set) => Right(set)
                  }

                  // TODO:
                  // - if 0 partials - call setters directly
                  // - else if 1 partial - result.map { /* default constructor then setters */ }
                  // - else if 2 partials - resul1.map2(result2) { /* default constructor then setters */ }
                  // - else basically if-else with mutable array on full, and flatMap on fail fast
                  DerivationResult.notYetImplemented("Java Bean creation")
                }
            case Product.Constructor.CaseClass(constructor, parameters) =>
              Traverse[List]
                .traverse[DerivationResult, Existential[Product.Parameter], Existential[TransformationExpr]](
                  parameters
                ) { (parameter: Existential[Product.Parameter]) =>
                  Existential.use(parameter) { implicit ParameterType: Type[parameter.Underlying] =>
                    { case Product.Parameter(toName, defaultValue) =>
                      resolveSource[parameter.Underlying](toName, defaultValue).map {
                        (arg: TransformationExpr[parameter.Underlying]) =>
                          Existential[TransformationArgument, parameter.Underlying](
                            TransformationArgument(toName, arg)
                          )
                      }
                    }
                  }
                }
                .flatMap { (resolvedArguments: List[Existential[TransformationArgument]]) =>
                  val (
                    totalArguments: List[Existential[Product.Argument]],
                    partialArguments: List[Nothing] // TODO: I have not idea ATM what to do with it
                  ) =
                    resolvedArguments.partitionMap { (argument: Existential[TransformationArgument]) =>
                      Existential.use(argument) { implicit Argument: Type[argument.Underlying] =>
                        {
                          case TransformationArgument.Total(name, arg) =>
                            Left(Existential[Product.Argument, argument.Underlying](Product.Argument(name, arg)))
                          case TransformationArgument.Partial(name, arg) =>
                            Right(???)
                        }
                      }
                    }

                  // TODO:
                  // - if 0 partials - call constructor directly
                  // - else if 1 partial - result.map { /* create with values */ }
                  // - else if 2 partials - resul1.map2(result2) { /* create with values */ }
                  // - else basically if-else with mutable array on full, and flatMap on fail fast
                  DerivationResult.notYetImplemented("case class creation")
                }
          }
        case _ => DerivationResult.attemptNextRule
      }

    sealed private trait PartiallyAppliedSetter[To] extends scala.Product with Serializable
    private object PartiallyAppliedSetter {
      final case class Total[To](set: Expr[To] => Expr[Unit]) extends PartiallyAppliedSetter[To]
      final case class Partial[To](set: Expr[To] => Expr[partial.Result[Unit]]) extends PartiallyAppliedSetter[To]

      def apply[To: Type, A: Type](
          set: (Expr[To], Expr[A]) => Expr[Unit],
          value: TransformationExpr[A]
      ): PartiallyAppliedSetter[To] =
        value.fold[PartiallyAppliedSetter[To]] { (value: Expr[A]) =>
          Total((to: Expr[To]) => set(to, value))
        } { (value: Expr[partial.Result[A]]) =>
          Partial((to: Expr[To]) => value.map(Expr.Function1.instance[A, Unit](set(to, _))))
        }
    }

    sealed private trait TransformationArgument[A] extends scala.Product with Serializable
    private object TransformationArgument {
      final case class Total[A](name: String, value: Expr[A]) extends TransformationArgument[A]
      final case class Partial[A](name: String, value: Expr[partial.Result[A]]) extends TransformationArgument[A]

      def apply[A](name: String, value: TransformationExpr[A]): TransformationArgument[A] =
        value.fold[TransformationArgument[A]] { (arg: Expr[A]) =>
          Total(name, arg)
        } { (arg: Expr[Result[A]]) =>
          Partial(name, arg)
        }
    }
  }
}
