package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.dsl.{PreferPartialTransformer, PreferTotalTransformer}
import io.scalaland.chimney.internal.*
import io.scalaland.chimney.internal.utils.EitherUtils

import scala.reflect.macros.blackbox

trait TransformerMacros extends MappingMacros with TargetConstructorMacros with EitherUtils {

  val c: blackbox.Context

  import c.universe.*

  def buildDefinedTransformer[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](derivationTarget: DerivationTarget): Tree = {
    val config = readConfig[C, InstanceFlags, ScopeFlags]
      .withDefinitionScope(weakTypeOf[From], weakTypeOf[To])
      .withDerivationTarget(derivationTarget)

    if (!config.valueLevelAccessNeeded) {
      genTransformer[From, To](config)
    } else {
      val tdName = freshTermName("td")
      val derivedTransformer = genTransformer[From, To](config.withTransformerDefinitionPrefix(q"$tdName"))

      q"""
        final val $tdName = ${c.prefix.tree}
        $derivedTransformer
      """
    }
  }

  def deriveWithTarget[From: WeakTypeTag, To: WeakTypeTag, ResultTpe](
      derivationTarget: DerivationTarget
  ): c.Expr[ResultTpe] = {
    val tcTree = findLocalTransformerConfigurationFlags
    val flags = captureFromTransformerConfigurationTree(tcTree)
    val config = TransformerConfig(flags = flags)
      .withDefinitionScope(weakTypeOf[From], weakTypeOf[To])
      .withDerivationTarget(derivationTarget)
    val transformerTree = genTransformer[From, To](config)
    c.Expr[ResultTpe] {
      q"""
        {
          val _ = $tcTree // hack to avoid unused warnings
          $transformerTree
        }
      """
    }
  }

  def expandTransform[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](derivationTarget: DerivationTarget)(callTransform: (Tree, Tree) => Tree): Tree = {
    val tiName = freshTermName("ti")

    val config = readConfig[C, InstanceFlags, ScopeFlags]
      .withTransformerDefinitionPrefix(q"$tiName.td")
      .withDerivationTarget(derivationTarget)

    val derivedTransformerTree = genTransformer[From, To](config)

    q"""
       final val $tiName = ${c.prefix.tree}
       ${callTransform(derivedTransformerTree, q"$tiName.source")}
    """
  }

  def genTransformer[From: WeakTypeTag, To: WeakTypeTag](
      config: TransformerConfig
  ): Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName = freshTermName(From)
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    genTransformerTree(config.withSrcPrefixTree(srcPrefixTree))(From, To) match {

      case Right(transformerTree) =>
        config.derivationTarget match {
          case DerivationTarget.TotalTransformer =>
            q"""
               new ${Trees.Transformer.tpe(From, To)} {
                 final def transform($srcName: $From): $To = {
                   $transformerTree
                 }
               }
            """
          case pt: DerivationTarget.PartialTransformer =>
            q"""
               new ${Trees.PartialTransformer.tpe(From, To)} {
                 final def transform($srcName: $From, ${pt.failFastTermName}: Boolean): ${pt.targetType(To)} = {
                   $transformerTree
                 }
               }
            """
          case DerivationTarget.LiftedTransformer(f, _, _) =>
            q"""
               new ${Trees.LiftedTransformer.tpe(f, From, To)} {
                 final def transform($srcName: $From): ${f.applyTypeArg(To)} = {
                   $transformerTree
                 }
               }
            """
        }

      case Left(derivationErrors) =>
        val errorMessage =
          s"""Chimney can't derive transformation from $From to $To
             |
             |${TransformerDerivationError.printErrors(derivationErrors)}
             |Consult $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        c.abort(c.enclosingPosition, errorMessage)
    }
  }

  def genTransformerTree(
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
    resolveTransformerBody(config)(From, To).map { derivedTree =>
      mkDerivedBodyTree(config.derivationTarget)(derivedTree).tree
    }
  }

  def expandTransformerTree(
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], DerivedTree] = {

    resolveImplicitTransformer(config)(From, To)
      .map { localImplicitDerivedTree =>
        Right(localImplicitDerivedTree.mapTree(_.callTransform(config.srcPrefixTree)))
      }
      .getOrElse {
        deriveTransformerTree(config)(From, To)
      }
  }

  def deriveTransformerTree(
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], DerivedTree] = {

    expandSubtypes(config)(From, To)
      .orElse(expandValueClassToValueClass(config)(From, To))
      .orElse(expandValueClassToType(config)(From, To))
      .orElse(expandTypeToValueClass(config)(From, To))
      .orElse(expandOptions(config)(From, To))
      .orElse(expandPartialFromOptionToNonOption(config)(From, To))
      .orElse(expandTargetWrappedInOption(config)(From, To))
      .orElse(expandSourceWrappedInOption(config)(From, To))
      .orElse(expandEithers(config)(From, To))
      .orElse(expandFromMap(config)(From, To))
      .orElse(expandIterableOrArray(config)(From, To))
      .orElse(expandDestinationTuple(config)(From, To))
      .orElse(expandDestinationCaseClass(config)(From, To))
      .orElse(expandDestinationJavaBean(config)(From, To))
      .orElse(expandSealedClasses(config)(From, To))
      .getOrElse(notSupportedDerivation(config.srcPrefixTree, From, To))
  }

  def expandPartialFromOptionToNonOption(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(
      config.derivationTarget.isPartial && !config.flags.unsafeOption && fromOptionToNonOption(From, To)
    ) {
      val fn = Ident(freshTermName("value"))
      resolveRecursiveTransformerBody(config.withSrcPrefixTree(q"$fn"))(From.typeArgs.head, To)
        .map { innerDerived =>
          val liftedTree =
            if (innerDerived.isPartialTarget) innerDerived.tree
            else mkTransformerBodyTree0(config.derivationTarget)(innerDerived.tree)

          val tree =
            q"""
              ${config.srcPrefixTree}
                .map(($fn: ${From.typeArgs.head}) => $liftedTree)
                .getOrElse(${Trees.PartialResult.empty})
           """

          DerivedTree(tree, config.derivationTarget)
        }
    }
  }

  def expandSubtypes(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(isSubtype(From, To)) {
      Right(DerivedTree.fromTotalTree(config.srcPrefixTree))
    }
  }

  def expandValueClassToType(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(fromValueClass(From, To)) {
      val fromValueClassMember = From.valueClassMember.toRight(
        // $COVERAGE-OFF$
        Seq(CantFindValueClassMember(From.typeSymbol.name.toString, To.typeSymbol.name.toString))
        // $COVERAGE-ON$
      )

      for {
        fromValueClassMember <- fromValueClassMember
        fromValueClassMemberType = fromValueClassMember.returnType
        fromMemberAccessTree = q"${config.srcPrefixTree}.${fromValueClassMember.name}"
        derivedTree <- resolveRecursiveTransformerBody(config.withSrcPrefixTree(fromMemberAccessTree))(
          fromValueClassMemberType,
          To
        )
      } yield derivedTree
    }
  }

  def expandTypeToValueClass(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(toValueClass(From, To)) {
      val toValueClassMember = To.valueClassMember.toRight(
        // $COVERAGE-OFF$
        Seq(CantFindValueClassMember(To.typeSymbol.name.toString, From.typeSymbol.name.toString))
        // $COVERAGE-ON$
      )

      for {
        toValueClassMethodSymbol <- toValueClassMember
        toValueClassMemberType <- toValueClassMember.map(_.returnType)
        transformerBodyTree <- resolveRecursiveTransformerBody(config)(From, toValueClassMemberType)
      } yield mkTransformerBodyTree1(
        To,
        Target.fromField(toValueClassMethodSymbol, toValueClassMemberType),
        transformerBodyTree,
        config.derivationTarget
      )(innerTree => q"new $To($innerTree)")
    }
  }

  def expandTargetWrappedInOption(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(isOption(To) && !To.typeArgs.headOption.exists(_.isSealedClass)) { // TODO: check for None?
      if (To <:< noneTpe) {
        notSupportedDerivation(config.srcPrefixTree, From, To)
      } else {
        val optFrom = c.typecheck(Trees.Option.tpe(From), c.TYPEmode).tpe
        expandOptions(config.withSrcPrefixTree(Trees.Option.option(From, config.srcPrefixTree)))(
          optFrom,
          To
        ).get // TODO: better support for calling other rules
      }
    }
  }

  def expandValueClassToValueClass(config: TransformerConfig)(
      From: Type,
      To: Type
  ): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(bothValueClasses(From, To)) {
      val fromValueClassMember = From.valueClassMember.toRight(
        // $COVERAGE-OFF$
        Seq(CantFindValueClassMember(From.typeSymbol.name.toString, To.typeSymbol.name.toString))
        // $COVERAGE-ON$
      )

      val toValueClassMember = To.valueClassMember.toRight(
        // $COVERAGE-OFF$
        Seq(CantFindValueClassMember(To.typeSymbol.name.toString, From.typeSymbol.name.toString))
        // $COVERAGE-ON$
      )

      for {
        fromValueClassMemberSymbol <- fromValueClassMember
        fromValueClassMemberType = fromValueClassMemberSymbol.returnType
        toValueClassMethodSymbol <- toValueClassMember
        toValueClassMemberType <- toValueClassMember.map(_.returnType)
        fromMemberAccessTree = q"${config.srcPrefixTree}.${fromValueClassMemberSymbol.name}"
        transformerBodyTree <- resolveRecursiveTransformerBody(config.withSrcPrefixTree(fromMemberAccessTree))(
          fromValueClassMemberType,
          toValueClassMemberType
        )
      } yield mkTransformerBodyTree1(
        To,
        Target.fromField(toValueClassMethodSymbol, toValueClassMemberType),
        transformerBodyTree,
        config.derivationTarget
      )(innerTree => q"new $To($innerTree)")
    }
  }

  def expandSourceWrappedInOption(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(config.flags.unsafeOption && isOption(From)) {
      if (From <:< noneTpe || config.derivationTarget.isPartial) {
        notSupportedDerivation(config.srcPrefixTree, From, To)
      } else {
        val fromInnerT = From.typeArgs.head
        val innerSrcPrefix = q"${config.srcPrefixTree}.get"
        resolveRecursiveTransformerBody(config.withSrcPrefixTree(innerSrcPrefix))(fromInnerT, To)
          .map { innerTransformerBody =>
            val fn = freshTermName(innerSrcPrefix).toString
            mkTransformerBodyTree1(To, Target(fn, To), innerTransformerBody, config.derivationTarget) { tree =>
              q"($tree)"
            }
          }
      }
    }
  }

  def expandOptions(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(bothOptions(From, To)) {
      def fromInnerT = From.typeArgs.head

      def toInnerT = To.typeArgs.head

      if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
        notSupportedDerivation(config.srcPrefixTree, From, To)
      } else {
        val fn = Ident(freshTermName(config.srcPrefixTree))
        resolveRecursiveTransformerBody(config.withSrcPrefixTree(fn))(fromInnerT, toInnerT)
          .map {
            case DerivedTree(innerTree, DerivationTarget.TotalTransformer) =>
              DerivedTree.fromTotalTree(
                q"${config.srcPrefixTree}.map(($fn: $fromInnerT) => $innerTree)"
              )

            case DerivedTree(innerTree, pt @ DerivationTarget.PartialTransformer(_)) =>
              val tree =
                q"""
               ${config.srcPrefixTree}.fold[${pt.targetType(To)}](
                 ${Trees.PartialResult.value(Trees.Option.empty(toInnerT))}
               )(
                 ($fn: $fromInnerT) => $innerTree.map(${Trees.Option.apply(toInnerT)})
               )
             """
              DerivedTree(tree, config.derivationTarget)

            case DerivedTree(
                  innerTree,
                  DerivationTarget.LiftedTransformer(wrapperType, wrapperSupportInstance, _)
                ) =>
              val tree =
                q"""
               ${config.srcPrefixTree}.fold[${wrapperType.applyTypeArg(To)}](
                 ${wrapperSupportInstance}.pure(${Trees.Option.empty(toInnerT)})
               )(
                 ($fn: $fromInnerT) => ${wrapperSupportInstance}.map($innerTree, ${Trees.Option.apply(toInnerT)})
               )
             """
              DerivedTree(tree, config.derivationTarget)
          }
      }
    }
  }

  def expandEithers(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(bothEithers(From, To)) {
      val List(fromLeftT, fromRightT) = From.typeArgs
      val List(toLeftT, toRightT) = To.typeArgs

      val fnL = freshTermName("left")
      val fnR = freshTermName("right")

      if (From <:< leftTpe && !(To <:< rightTpe)) {
        resolveRecursiveTransformerBody(config.withSrcPrefixTree(q"${config.srcPrefixTree}.value"))(fromLeftT, toLeftT)
          .map { tbt =>
            mkTransformerBodyTree1(To, Target(fnL.toString, toLeftT), tbt, config.derivationTarget) { leftArgTree =>
              q"${Trees.Either.left(leftArgTree)}"
            }
          }
      } else if (From <:< rightTpe && !(To <:< leftTpe)) {
        resolveRecursiveTransformerBody(config.withSrcPrefixTree(q"${config.srcPrefixTree}.value"))(
          fromRightT,
          toRightT
        )
          .map { tbt =>
            mkTransformerBodyTree1(To, Target(fnR.toString, toRightT), tbt, config.derivationTarget) { rightArgTree =>
              q"${Trees.Either.right(rightArgTree)}"
            }
          }
      } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
        val leftTransformerE = resolveRecursiveTransformerBody(config.withSrcPrefixTree(Ident(fnL)))(fromLeftT, toLeftT)
        val rightTransformerE =
          resolveRecursiveTransformerBody(config.withSrcPrefixTree(Ident(fnR)))(fromRightT, toRightT)

        (leftTransformerE, rightTransformerE) match {
          case (Right(leftTbt), Right(rightTbt)) =>
            val leftN = freshTermName("left")
            val rightN = freshTermName("right")

            val leftBody =
              mkTransformerBodyTree1(To, Target(leftN.toString, toLeftT), leftTbt, config.derivationTarget) {
                leftArgTree => q"${Trees.Either.left(leftArgTree)}"
              }

            val rightBody =
              mkTransformerBodyTree1(To, Target(rightN.toString, toRightT), rightTbt, config.derivationTarget) {
                rightArgTree => q"${Trees.Either.right(rightArgTree)}"
              }

            Right(
              mkEitherFold(
                config.srcPrefixTree,
                To,
                InstanceClause(Some(fnL), fromLeftT, leftBody),
                InstanceClause(Some(fnR), fromRightT, rightBody),
                config.derivationTarget
              )
            )
          case _ =>
            Left(leftTransformerE.left.getOrElse(Nil) ++ rightTransformerE.left.getOrElse(Nil))
        }
      } else {
        notSupportedDerivation(config.srcPrefixTree, From, To)
      }
    }
  }

  def expandFromMap(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(isMap(From)) {
      val ToInnerT = To.collectionInnerTpe

      (config.derivationTarget, ToInnerT.caseClassParams.map(_.resultTypeIn(ToInnerT))) match {
        case (
              DerivationTarget.LiftedTransformer(
                wrapperType,
                wrapperSupportInstance,
                Some(wrapperErrorPathSupportInstance)
              ),
              List(toKeyT, toValueT)
            ) =>
          val List(fromKeyT, fromValueT) = From.typeArgs

          val fnK = Ident(freshTermName("k"))
          val fnV = Ident(freshTermName("v"))

          val keyTransformerE = resolveRecursiveTransformerBody(config.withSrcPrefixTree(fnK))(fromKeyT, toKeyT)
          val valueTransformerE = resolveRecursiveTransformerBody(config.withSrcPrefixTree(fnV))(fromValueT, toValueT)

          (keyTransformerE, valueTransformerE) match {
            case (Right(keyTransformer), Right(valueTransformer)) =>
              val WrappedToInnerT = wrapperType.applyTypeArg(ToInnerT)

              val keyTransformerWithPath =
                keyTransformer.target match {
                  case DerivationTarget.LiftedTransformer(_, _, _) =>
                    q"""${wrapperErrorPathSupportInstance}.addPath[$toKeyT](
                     ${keyTransformer.tree},
                     _root_.io.scalaland.chimney.ErrorPathNode.MapKey($fnK)
                   )"""
                  case DerivationTarget.TotalTransformer =>
                    q"${wrapperSupportInstance}.pure[$toKeyT](${keyTransformer.tree})"
                  case _: DerivationTarget.PartialTransformer => {
                    // $COVERAGE-OFF$
                    c.abort(c.enclosingPosition, "Not supported for partial transformers!")
                    // $COVERAGE-ON$
                  }
                }

              val valueTransformerWithPath =
                valueTransformer.target match {
                  case DerivationTarget.LiftedTransformer(_, _, _) =>
                    q"""${wrapperErrorPathSupportInstance}.addPath[$toValueT](
                      ${valueTransformer.tree},
                      _root_.io.scalaland.chimney.ErrorPathNode.MapValue($fnK)
                   )"""
                  case DerivationTarget.TotalTransformer =>
                    q"${wrapperSupportInstance}.pure[$toValueT](${valueTransformer.tree})"
                  case _: DerivationTarget.PartialTransformer => {
                    // $COVERAGE-OFF$
                    c.abort(c.enclosingPosition, "Not supported for partial transformers!")
                    // $COVERAGE-ON$
                  }
                }

              val tree = q"""${wrapperSupportInstance}.traverse[$To, $WrappedToInnerT, $ToInnerT](
                  ${config.srcPrefixTree}.iterator.map[$WrappedToInnerT] {
                    case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
                      ${wrapperSupportInstance}.product[$toKeyT, $toValueT](
                        $keyTransformerWithPath,
                        $valueTransformerWithPath
                      )
                  },
                  _root_.scala.Predef.identity[$WrappedToInnerT]
                )
             """
              Right(DerivedTree(tree, config.derivationTarget))
            case _ =>
              Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
          }

        case (pt @ DerivationTarget.PartialTransformer(_), List(toKeyT, toValueT)) =>
          val List(fromKeyT, fromValueT) = From.typeArgs

          val fnK = Ident(freshTermName("k"))
          val fnV = Ident(freshTermName("v"))

          val keyTransformerE = resolveRecursiveTransformerBody(config.withSrcPrefixTree(fnK))(fromKeyT, toKeyT)
          val valueTransformerE = resolveRecursiveTransformerBody(config.withSrcPrefixTree(fnV))(fromValueT, toValueT)

          (keyTransformerE, valueTransformerE) match {
            case (Right(keyTransformer), Right(valueTransformer)) =>
              val keyTransformerWithPath =
                keyTransformer.target match {
                  case _: DerivationTarget.PartialTransformer =>
                    q"${keyTransformer.tree}.prependErrorPath(${Trees.PathElement.mapKey(fnK)})"
                  case DerivationTarget.TotalTransformer =>
                    Trees.PartialResult.value(keyTransformer.tree)
                  case _: DerivationTarget.LiftedTransformer => {
                    // $COVERAGE-OFF$
                    c.abort(c.enclosingPosition, "Not supported for lifted transformers!")
                    // $COVERAGE-ON$
                  }
                }

              val valueTransformerWithPath =
                valueTransformer.target match {
                  case _: DerivationTarget.PartialTransformer =>
                    q"${valueTransformer.tree}.prependErrorPath(${Trees.PathElement.mapValue(fnK)})"
                  case DerivationTarget.TotalTransformer =>
                    Trees.PartialResult.value(valueTransformer.tree)
                  case _: DerivationTarget.LiftedTransformer => {
                    // $COVERAGE-OFF$
                    c.abort(c.enclosingPosition, "Not supported for lifted transformers!")
                    // $COVERAGE-ON$
                  }
                }

              val tree = Trees.PartialResult.traverse(
                tq"$To",
                tq"($fromKeyT, $fromValueT)",
                tq"($toKeyT, $toValueT)",
                q"${config.srcPrefixTree}.iterator",
                q"""{ case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
                  ${Trees.PartialResult
                    .product(toKeyT, toValueT, keyTransformerWithPath, valueTransformerWithPath, pt.failFastTree)}
               }""",
                pt.failFastTree
              )
              Right(DerivedTree(tree, config.derivationTarget))
            case _ =>
              Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
          }

        case _ =>
          expandIterableOrArray(config)(From, To).get // TODO: provide better support for calling other rules
      }
    }
  }

  def expandIterableOrArray(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(bothOfIterableOrArray(From, To)) {
      val FromInnerT = From.collectionInnerTpe
      val ToInnerT = To.collectionInnerTpe

      val fn = Ident(freshTermName(config.srcPrefixTree))

      resolveRecursiveTransformerBody(config.withSrcPrefixTree(fn))(FromInnerT, ToInnerT)
        .map {
          case DerivedTree(
                innerTransformerTree,
                DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, Some(wrapperErrorPathSupportInstance))
              ) =>
            val idx = Ident(freshTermName("idx"))

            val tree = q"""${wrapperSupportInstance}.traverse[$To, ($FromInnerT, _root_.scala.Int), $ToInnerT](
              ${config.srcPrefixTree}.iterator.zipWithIndex,
              { case (${fn.name}: $FromInnerT, ${idx.name}: _root_.scala.Int) =>
                ${wrapperErrorPathSupportInstance}.addPath[$ToInnerT](
                  $innerTransformerTree,
                  _root_.io.scalaland.chimney.ErrorPathNode.Index($idx)
                )
              }
            )
            """
            DerivedTree(tree, config.derivationTarget)
          case DerivedTree(
                innerTransformerTree,
                DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, None)
              ) =>
            val tree = q"""
              ${wrapperSupportInstance}.traverse[$To, $FromInnerT, $ToInnerT](
                ${config.srcPrefixTree}.iterator,
                ($fn: $FromInnerT) => $innerTransformerTree
              )
            """
            DerivedTree(tree, config.derivationTarget)

          case DerivedTree(innerTransformerTree, pt @ DerivationTarget.PartialTransformer(_)) =>
            val idx = Ident(freshTermName("idx"))

            val tree = Trees.PartialResult.traverse(
              tq"$To",
              tq"($FromInnerT, ${Trees.intTpe})",
              tq"$ToInnerT",
              q"${config.srcPrefixTree}.iterator.zipWithIndex",
              q"""{ case (${fn.name}: $FromInnerT, ${idx.name}: ${Trees.intTpe}) =>
                $innerTransformerTree.prependErrorPath(${Trees.PathElement.index(idx)})
              }""",
              pt.failFastTree
            )
            DerivedTree(tree, config.derivationTarget)

          case DerivedTree(innerTransformerTree, DerivationTarget.TotalTransformer) =>
            def isTransformationIdentity = fn == innerTransformerTree
            def sameCollectionTypes = From.typeConstructor =:= To.typeConstructor

            val transformedCollectionTree: Tree = (isTransformationIdentity, sameCollectionTypes) match {
              case (true, true) =>
                // identity transformation, same collection types
                config.srcPrefixTree

              case (true, false) =>
                // identity transformation, different collection types
                config.srcPrefixTree.convertCollection(To, ToInnerT)

              case (false, true) =>
                // non-trivial transformation, same collection types
                q"${config.srcPrefixTree}.map(($fn: $FromInnerT) => $innerTransformerTree)"

              case (false, false) =>
                q"${config.srcPrefixTree}.iterator.map(($fn: $FromInnerT) => $innerTransformerTree)"
                  .convertCollection(To, ToInnerT)
            }

            DerivedTree.fromTotalTree(transformedCollectionTree)
        }
    }
  }

  def expandSealedClasses(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(bothSealedClasses(From, To)) {
      if (isOption(To)) {
        expandSealedClasses(config)(From, To.typeArgs.head).get.map {
          _.mapTree(inner => q"_root_.scala.Option($inner)")
        }
      } else {
        resolveCoproductInstance(From, To, config)
          .map { instanceTree => Right(instanceTree) }
          .getOrElse {
            val fromCS = From.typeSymbol.classSymbolOpt.get
            val toCS = To.typeSymbol.classSymbolOpt.get

            // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
            val fromInstances = fromCS.subclasses.map(_.typeInSealedParent(From)).distinct
            val toInstances = toCS.subclasses.map(_.typeInSealedParent(To)).distinct

            val targetNamedInstances = toInstances.groupBy(_.typeSymbol.name.toString)

            val instanceClauses = fromInstances.map { instTpe =>
              val instName = instTpe.typeSymbol.name.toString

              resolveCoproductInstance(instTpe, To, config)
                .map { instanceTree =>
                  Right(InstanceClause(None, instTpe, instanceTree))
                }
                .orElse {
                  resolveImplicitTransformer(config)(instTpe, To)
                    .map { implicitTransformerTree =>
                      val fn = freshTermName(instName)
                      Right(
                        InstanceClause(Some(fn), instTpe, implicitTransformerTree.mapTree(_.callTransform(Ident(fn))))
                      )
                    }
                }
                .getOrElse {
                  val instSymbol = instTpe.typeSymbol

                  def fail: Left[Seq[CantFindCoproductInstanceTransformer], InstanceClause] = Left {
                    Seq(
                      CantFindCoproductInstanceTransformer(
                        instSymbol.fullName,
                        From.typeSymbol.fullName,
                        To.typeSymbol.fullName
                      )
                    )
                  }

                  targetNamedInstances.getOrElse(instName, Nil) match {
                    case _ :: _ :: _ =>
                      Left {
                        Seq(
                          AmbiguousCoproductInstance(
                            instName,
                            From.typeSymbol.fullName,
                            To.typeSymbol.fullName
                          )
                        )
                      }
                    case List(matchingTargetTpe) =>
                      resolveImplicitTransformer(config)(instTpe, matchingTargetTpe)
                        .map { implicitTransformerTree =>
                          val fn = freshTermName(instName)
                          Right(
                            InstanceClause(
                              Some(fn),
                              instTpe,
                              implicitTransformerTree.mapTree(_.callTransform(Ident(fn)))
                            )
                          )
                        }
                        .getOrElse {
                          if (
                            matchingTargetTpe.typeSymbol.isModuleClass && (instSymbol.isModuleClass || instSymbol.isCaseClass)
                          ) {
                            val objTree = q"${matchingTargetTpe.typeSymbol.asClass.module}"
                            Right(InstanceClause(None, instSymbol.asType.toType, DerivedTree.fromTotalTree(objTree)))
                          } else if (matchingTargetTpe.typeSymbol.isCaseClass && instSymbol.isCaseClass) {
                            val fn = freshTermName(instName)
                            expandDestinationCaseClass(config.rec.withSrcPrefixTree(Ident(fn)))(
                              instTpe,
                              matchingTargetTpe
                            ).get.map { innerTransformerTree =>
                              InstanceClause(Some(fn), instTpe, innerTransformerTree)
                            }
                          } else {
                            // $COVERAGE-OFF$
                            fail
                            // $COVERAGE-ON$
                          }
                        }
                    case _ =>
                      fail
                  }
                }
            }

            if (instanceClauses.forall(_.isRight)) {
              val clauses = instanceClauses.collect { case Right(clause) => clause }
              Right(mkCoproductPatternMatch(config.srcPrefixTree, clauses, config.derivationTarget))
            } else {
              Left {
                instanceClauses.collect { case Left(derivationErrors) => derivationErrors }.flatten
              }
            }
          }
      }
    }
  }

  def resolveCoproductInstance(
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Option[DerivedTree] = {
    val pureRuntimeDataIdxOpt = config.coproductInstanceOverrides.get((From.typeSymbol, To))
    val liftedRuntimeDataIdxOpt = config.coproductInstanceFOverrides.get((From.typeSymbol, To))
    val partialRuntimeDataIdxOpt = config.coproductInstancesPartialOverrides.get((From.typeSymbol, To))

    (config.derivationTarget, pureRuntimeDataIdxOpt, partialRuntimeDataIdxOpt, liftedRuntimeDataIdxOpt) match {
      case (liftedTarget: DerivationTarget.LiftedTransformer, _, _, Some(runtimeDataIdxLifted)) =>
        Some(
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            config.srcPrefixTree,
            To,
            runtimeDataIdxLifted,
            liftedTarget
          )
        )

      case (partialTarget: DerivationTarget.PartialTransformer, _, Some(runtimeDataIdxPartial), _) =>
        Some(
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            config.srcPrefixTree,
            To,
            runtimeDataIdxPartial,
            partialTarget
          )
        )

      case (_, Some(runtimeDataIdxPure), _, _) =>
        Some(
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            config.srcPrefixTree,
            To,
            runtimeDataIdxPure,
            DerivationTarget.TotalTransformer
          )
        )
      case _ =>
        None
    }
  }

  def expandDestinationTuple(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(isTuple(To)) {
      resolveSourceTupleAccessors(From, To)
        .flatMap { accessorsMapping =>
          resolveTransformerBodyTreeFromAccessorsMapping(accessorsMapping, From, To, config)
        }
        .map { transformerBodyPerTarget =>
          val targets = To.caseClassParams.map(Target.fromField(_, To))
          val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

          mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
            mkNewClass(To, args)
          }
        }
    }
  }

  def expandDestinationCaseClass(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(destinationCaseClass(To)) {
      val targets = To.caseClassParams.map(Target.fromField(_, To))

      val targetTransformerBodiesMapping = if (isTuple(From)) {
        resolveSourceTupleAccessors(From, To).flatMap { accessorsMapping =>
          resolveTransformerBodyTreeFromAccessorsMapping(accessorsMapping, From, To, config)
        }
      } else {
        val overridesMapping = resolveOverrides(From, targets, config)
        val notOverridenTargets = targets.diff(overridesMapping.keys.toSeq)
        val accessorsMapping = resolveAccessorsMapping(From, notOverridenTargets, config)

        resolveTransformerBodyTreeFromAccessorsMapping(accessorsMapping, From, To, config)
          .map(_ ++ overridesMapping)
      }

      targetTransformerBodiesMapping.map { transformerBodyPerTarget =>
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

        mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
          mkNewClass(To, args)
        }
      }
    }
  }

  def expandDestinationJavaBean(
      config: TransformerConfig
  )(From: Type, To: Type): Option[Either[Seq[TransformerDerivationError], DerivedTree]] = {
    Option.when(config.flags.beanSetters && destinationJavaBean(To)) {
      val beanSetters = To.beanSetterMethods
      val targets = beanSetters.map(Target.fromJavaBeanSetter(_, To))

      val accessorsMapping = resolveAccessorsMapping(From, targets, config)

      resolveTransformerBodyTreeFromAccessorsMapping(accessorsMapping, From, To, config)
        .map { transformerBodyPerTarget =>
          val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
          mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
            mkNewJavaBean(To, targets zip args)
          }
        }
    }
  }

  def resolveTransformerBodyTreeFromAccessorsMapping(
      accessorsMapping: Map[Target, AccessorResolution],
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Map[Target, DerivedTree]] = {

    val (erroredTargets, resolvedBodyTrees) = accessorsMapping.map {
      case (target, accessor: AccessorResolution.Resolved) =>
        target -> resolveTransformerBodyTreeFromAccessor(target, accessor, From, config)
      case (target, accessor) =>
        target -> Left(
          Seq(
            MissingAccessor(
              fieldName = target.name,
              fieldTypeName = target.tpe.typeSymbol.fullName,
              sourceTypeName = From.typeSymbol.fullName,
              targetTypeName = To.typeSymbol.fullName,
              defAvailable = accessor == AccessorResolution.DefAvailable
            )
          )
        )
    }.partitionEitherValues

    if (erroredTargets.isEmpty) {
      Right(resolvedBodyTrees)
    } else {
      val targetsToFallback = erroredTargets.collect {
        case (target, _) if !accessorsMapping(target).isResolved => target
      }
      val fallbackTransformerBodies = resolveFallbackTransformerBodies(targetsToFallback, To, config)
      val unresolvedTargets = accessorsMapping.keys.toList
        .diff(resolvedBodyTrees.keys.toList)
        .diff(fallbackTransformerBodies.keys.toList)

      if (unresolvedTargets.isEmpty) {
        Right(resolvedBodyTrees ++ fallbackTransformerBodies)
      } else {
        val errors = unresolvedTargets.flatMap { target =>
          accessorsMapping(target) match {
            case AccessorResolution.Resolved(symbol: MethodSymbol, _) =>
              erroredTargets(target) :+ MissingTransformer(
                fieldName = target.name,
                sourceFieldTypeName = symbol.resultTypeIn(From).typeSymbol.fullName,
                targetFieldTypeName = target.tpe.typeSymbol.fullName,
                sourceTypeName = From.typeSymbol.fullName,
                targetTypeName = To.typeSymbol.fullName
              )
            case _ => erroredTargets(target)
          }
        }
        Left(errors)
      }
    }
  }

  def resolveTransformerBodyTreeFromAccessor(
      target: Target,
      accessor: AccessorResolution.Resolved,
      From: Type,
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], DerivedTree] = {
    val resolved = resolveRecursiveTransformerBody(
      config.withSrcPrefixTree(q"${config.srcPrefixTree}.${accessor.symbol.name}")
    )(accessor.symbol.resultTypeIn(From), target.tpe)

    (resolved, config.derivationTarget) match {
      case (Right(bodyTree), DerivationTarget.LiftedTransformer(_, _, Some(errorPathSupport)))
          if bodyTree.isLiftedTarget =>
        Right {
          DerivedTree(
            q"""$errorPathSupport.addPath[${target.tpe}](
                 ${bodyTree.tree},
                 _root_.io.scalaland.chimney.ErrorPathNode.Accessor(${accessor.symbol.name.toString})
               )""",
            config.derivationTarget
          )
        }
      case (Right(bodyTree), DerivationTarget.PartialTransformer(_)) if bodyTree.isPartialTarget =>
        Right {
          DerivedTree(
            q"${bodyTree.tree}.prependErrorPath(${Trees.PathElement.accessor(accessor.symbol.name.toString)})",
            config.derivationTarget
          )
        }
      case _ => resolved
    }
  }

  def resolveRecursiveTransformerBody(
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], DerivedTree] = {
    resolveTransformerBody(config.rec)(From, To)
  }

  def resolveTransformerBody(
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], DerivedTree] = {
    config.derivationTarget match {
      case DerivationTarget.LiftedTransformer(wrapperType, _, _) =>
        val implicitTransformerF = resolveImplicitTransformer(config)(From, To)
        val implicitTransformer = findLocalImplicitTransformer(From, To, DerivationTarget.TotalTransformer)

        (implicitTransformerF, implicitTransformer) match {
          case (Some(localImplicitTreeF), Some(localImplicitTree)) =>
            c.abort(
              c.enclosingPosition,
              s"""Ambiguous implicits while resolving Chimney recursive transformation:
                 |
                 |TransformerF[${wrapperType}, $From, $To]: $localImplicitTreeF
                 |Transformer[$From, $To]: $localImplicitTree
                 |
                 |Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used
                 |""".stripMargin
            )
          case (Some(localImplicitTreeF), None) =>
            Right(localImplicitTreeF.mapTree(_.callTransform(config.srcPrefixTree)))
          case (None, Some(localImplicitTree)) =>
            Right(localImplicitTree.mapTree(_.callTransform(config.srcPrefixTree)))
          case (None, None) =>
            deriveTransformerTree(config)(From, To)
        }
      case pt @ DerivationTarget.PartialTransformer(_) =>
        val implicitPartialTransformer = resolveImplicitTransformer(config)(From, To)
        val implicitTransformer = findLocalImplicitTransformer(From, To, DerivationTarget.TotalTransformer)

        (implicitPartialTransformer, implicitTransformer) match {
          case (Some(localImplicitTreePartial), None) =>
            Right(localImplicitTreePartial.mapTree(_.callTransform(config.srcPrefixTree)))
          case (Some(localImplicitTreePartial), Some(_))
              if config.flags.implicitConflictResolution.contains(PreferPartialTransformer) =>
            Right(localImplicitTreePartial.mapTree(_.callTransform(config.srcPrefixTree)))
          case (None, Some(localImplicitTree)) =>
            Right(localImplicitTree.mapTree(_.callTransform(config.srcPrefixTree)))
          case (Some(_), Some(localImplicitTree))
              if config.flags.implicitConflictResolution.contains(PreferTotalTransformer) =>
            Right(localImplicitTree.mapTree(_.callTransform(config.srcPrefixTree)))
          case (Some(localImplicitTreePartial), Some(localImplicitTree)) =>
            c.abort(
              c.enclosingPosition,
              s"""Ambiguous implicits while resolving Chimney recursive transformation:
                 |
                 |PartialTransformer[$From, $To]: $localImplicitTreePartial
                 |Transformer[$From, $To]: $localImplicitTree
                 |
                 |Please eliminate ambiguity from implicit scope or use enableImplicitConflictResolution/withFieldComputed/withFieldComputedPartial to decide which one should be used
                 |""".stripMargin
            )

          case (None, None) =>
            deriveTransformerTree(config)(From, To)
        }
      case DerivationTarget.TotalTransformer =>
        expandTransformerTree(config)(From, To)
    }
  }

  def resolveImplicitTransformer(config: TransformerConfig)(From: Type, To: Type): Option[DerivedTree] = {
    if (config.definitionScope.contains((From, To))) {
      None
    } else {
      findLocalImplicitTransformer(From, To, config.derivationTarget)
    }
  }

  def findLocalTransformerConfigurationFlags: Tree = {
    val searchTypeTree =
      tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[? <: io.scalaland.chimney.internal.TransformerFlags]]}"
    inferImplicitTpe(searchTypeTree, macrosDisabled = false)
      .getOrElse {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, "Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
  }

  private def findLocalImplicitTransformer(
      From: Type,
      To: Type,
      derivationTarget: DerivationTarget
  ): Option[DerivedTree] = {
    val searchTypeTree: Tree = derivationTarget match {
      case DerivationTarget.LiftedTransformer(f, _, _) =>
        Trees.LiftedTransformer.tpe(f, From, To)
      case DerivationTarget.PartialTransformer(_) =>
        Trees.PartialTransformer.tpe(From, To)
      case DerivationTarget.TotalTransformer =>
        Trees.Transformer.tpe(From, To)
    }

    inferImplicitTpe(searchTypeTree, macrosDisabled = false)
      .filterNot(isDeriving)
      .map(tree => DerivedTree(tree, derivationTarget))
  }

  def findTransformerErrorPathSupport(wrapperType: Type): Option[Tree] = {
    inferImplicitTpe(tq"_root_.io.scalaland.chimney.TransformerFErrorPathSupport[$wrapperType]", macrosDisabled = false)
  }

  private def inferImplicitTpe(tpeTree: Tree, macrosDisabled: Boolean): Option[Tree] = {
    val typedTpeTree = c.typecheck(
      tree = tpeTree,
      silent = true,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = macrosDisabled
    )

    scala.util
      .Try(c.inferImplicitValue(typedTpeTree.tpe, silent = true, withMacrosDisabled = macrosDisabled))
      .toOption
      .filterNot(_ == EmptyTree)
  }

  private def isDeriving(tree: Tree): Boolean = {
    tree match {
      case TypeApply(Select(qualifier, name), _) =>
        (qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.Transformer.type] ||
          qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.PartialTransformer.type]) &&
        name.toString == "derive"
      case Apply(TypeApply(Select(qualifier, name), _), _) =>
        qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.TransformerF.type] && name.toString == "derive"
      case _ =>
        false
    }
  }

  private def notSupportedDerivation(
      srcPrefixTree: Tree,
      fromTpe: Type,
      toTpe: Type
  ): Left[Seq[NotSupportedTransformerDerivation], Nothing] =
    Left {
      Seq(
        NotSupportedTransformerDerivation(
          toFieldName(srcPrefixTree),
          fromTpe.typeSymbol.fullName,
          toTpe.typeSymbol.fullName
        )
      )
    }

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
