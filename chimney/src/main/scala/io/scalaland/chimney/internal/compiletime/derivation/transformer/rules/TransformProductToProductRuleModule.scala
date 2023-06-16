package io.scalaland.chimney.internal.compiletime.derivation.transformer.rules

import io.scalaland.chimney.internal.compiletime.DerivationResult
import io.scalaland.chimney.internal.compiletime.derivation.transformer.Derivation
import io.scalaland.chimney.internal.compiletime.fp.Syntax.*
import io.scalaland.chimney.internal.compiletime.fp.Traverse
import io.scalaland.chimney.partial

private[compiletime] trait TransformProductToProductRuleModule { this: Derivation =>

  import TypeImplicits.*, ChimneyTypeImplicits.*

  protected object TransformProductToProductRule extends Rule("ProductToProduct") {

    def expand[From, To](implicit ctx: TransformationContext[From, To]): DerivationResult[Rule.ExpansionResult[To]] =
      (Type[From], Type[To]) match {
        case (ProductType(Product(fromExtractors, _)), ProductType(Product(_, toConstructors))) =>
          lazy val allowedGetters = fromExtractors.filter { getter =>
            getter.value.sourceType match {
              case Product.Getter.SourceType.ConstructorVal => true
              case Product.Getter.SourceType.AccessorMethod => ctx.config.flags.methodAccessors
              case Product.Getter.SourceType.JavaBeanGetter => ctx.config.flags.beanGetters
            }
          }
          def resolveSource[ToElement: Type](
              toName: String,
              defaultValue: Option[Expr[ToElement]]
          ): DerivationResult[TransformationExpr[ToElement]] =
            // TODO: append errorPath to these!
            ctx.config.fieldOverrides
              .get(toName)
              .map {
                case RuntimeFieldOverride.Const(runtimeDataIdx) =>
                  // We're constructing:
                  // '{ ${ runtimeDataStore }(idx).asInstanceOf[$ToElement] }
                  DerivationResult.expandedTotal(
                    ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[ToElement]
                  )
                case RuntimeFieldOverride.ConstPartial(runtimeDataIdx) =>
                  // We're constructing:
                  // '{ ${ runtimeDataStore }(idx).asInstanceOf[partial.Result[$ToElement]] }
                  DerivationResult.expandedPartial(
                    ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[partial.Result[ToElement]]
                  )
                case RuntimeFieldOverride.Computed(runtimeDataIdx) =>
                  // We're constructing:
                  // '{ ${ runtimeDataStore }(idx).asInstanceOf[$From => $ToElement](${ src }) }
                  DerivationResult.expandedTotal(
                    ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[From => ToElement](ctx.src)
                  )
                case RuntimeFieldOverride.ComputedPartial(runtimeDataIdx) =>
                  // We're constructing:
                  // '{ ${ runtimeDataStore }(idx).asInstanceOf[$From => partial.Result[$ToElement]](${ src }) }
                  DerivationResult.expandedPartial(
                    ctx.runtimeDataStore(runtimeDataIdx).asInstanceOfExpr[From => partial.Result[ToElement]](ctx.src)
                  )
                case RuntimeFieldOverride.RenamedFrom(sourceName) =>
                  fromExtractors
                    .collectFirst {
                      case getter if getter.value.name == sourceName =>
                        Existential.use(getter) { implicit Getter: Type[getter.Underlying] =>
                          { case Product.Getter(name, _, get) =>
                            // We're constructing:
                            // '{ ${ derivedToElement } } // using ${ src.$name }
                            deriveRecursiveTransformationExpr[getter.Underlying, ToElement](get(ctx.src))
                          }
                        }
                    }
                    .getOrElse {
                      DerivationResult.assertionError(
                        s"Assumed that field $fieldName is a part of ${Type[From]}, but wasn't found"
                      )
                    }
              }
              .orElse(allowedGetters.collectFirst {
                case getter if areNamesMatching(getter.value.name, toName) =>
                  Existential.use(getter) { implicit Getter: Type[getter.Underlying] =>
                    { case Product.Getter(name, _, get) =>
                      // We're constructing:
                      // '{ ${ derivedToElement } } // using ${ src.$name }
                      deriveRecursiveTransformationExpr[getter.Underlying, ToElement](get(ctx.src))
                    }
                  }
              })
              .orElse(defaultValue.map { (value: Expr[ToElement]) =>
                // We're constructing:
                // '{ ${ defaultValue } }
                DerivationResult.expandedTotal(value)
              })
              .getOrElse {
                // TODO:
                //   - if there is Java Bean getter that couldn't be used because flag is off, inform about it
                //   - if there is accessor that couldn't be used because flag is off, inform about it
                //   - if default value exists that couldn't be used because flag is off, inform about it
                DerivationResult.notYetImplemented("Proper error message")
              }

          toConstructors match {
            case Product.Constructor.JavaBean(defaultConstructor, setters) =>
              // TODO: if setters empty OR config.settersEnabled continue, otherwise fail with message
              Traverse[List]
                .traverse[DerivationResult, Existential[Product.Setter[To, *]], Existential[SetterWithValue[To, *]]](
                  setters
                ) { (setter: Existential[Product.Setter[To, *]]) =>
                  Existential.use(setter) { implicit SetterType: Type[setter.Underlying] =>
                    { case Product.Setter(toName, set) =>
                      resolveSource[setter.Underlying](toName, None).map {
                        value: TransformationExpr[setter.Underlying] =>
                          Existential[SetterWithValue[To, *], setter.Underlying](set -> value)
                      }
                    }
                  }
                }
                .flatMap { (resolvedSetters: List[Existential[SetterWithValue[To, *]]]) =>
                  val (
                    totalSetters: List[Existential[SetterWithTotalValue[To, *]]],
                    partialSetters: List[Existential[SetterWithPartialValue[To, *]]]
                  ) = resolvedSetters.partitionMap { setterWithValue =>
                    Existential.use(setterWithValue) { implicit SetterWithValue: Type[setterWithValue.Underlying] =>
                      {
                        case (set, TransformationExpr.TotalExpr(expr)) =>
                          Left(Existential[SetterWithTotalValue, setterWithValue.Underlying](set -> expr))
                        case (set, TransformationExpr.PartialExpr(expr)) =>
                          Right(Existential[SetterWithPartialValue, setterWithValue.Underlying](set -> expr))
                      }
                    }
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
                          Existential[TransformationArgument, parameter.Underlying](toName -> arg)
                      }
                    }
                  }
                }
                .flatMap { (resolvedArguments: List[Existential[TransformationArgument]]) =>
                  val (
                    totalArguments: List[Existential[Product.Argument]],
                    partialArguments: List[Existential[PromisedArgument]]
                  ) =
                    resolvedArguments.partitionMap { (argument: Existential[TransformationArgument]) =>
                      Existential.use(argument) { implicit Argument: Type[argument.Underlying] =>
                        {
                          case (name, TransformationExpr.Total(arg)) =>
                            Left(Existential[Product.Argument, argument.Underlying](Product.Argument(name, arg)))
                          case (name, TransformationExpr.Partial(arg)) =>
                            Right(
                              Existential[PromisedArgument, argument.Underlying](
                                ExprPromise
                                  .promise[argument.Underlying](ExprPromise.NameGenerationStrategy.FromType)
                                  .map((arg: Expr[argument.Underlying]) => Product.Argument(name, arg))
                              )
                            )
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

    private type SetterWithValue[To, A] = ((Expr[To], Expr[A]) => Expr[Unit], TransformationExpr[A])
    private type SetterWithTotalValue[To, A] = ((Expr[To], Expr[A]) => Expr[Unit], Expr[A])
    private type SetterWithPartialValue[To, A] = ((Expr[To], Expr[A]) => Expr[Unit], Expr[partial.Result[A]])

    private type TransformationArgument[A] = (String, TransformationExpr[A])
    private type PromisedArgument[A] = ExprPromise[A, Product.Argument[A]]

    private def areNamesMatching(fromName: String, toName: String): Boolean = ??? // TODO
  }
}
