package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.EitherUtils

import scala.reflect.macros.blackbox

trait TransformerMacros extends MappingMacros with TargetConstructorMacros with EitherUtils {

  val c: blackbox.Context

  import c.universe._

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
        val $tdName = ${c.prefix.tree}
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
       val $tiName = ${c.prefix.tree}
       ${callTransform(derivedTransformerTree, q"$tiName.source")}
    """
  }

  def genTransformer[From: WeakTypeTag, To: WeakTypeTag](
      config: TransformerConfig
  ): Tree = {
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName = freshTermName(From)

    genTransformerTree(srcName, config)(From, To) match {

      case Right(transformerTree) =>
        config.derivationTarget match {
          case DerivationTarget.TotalTransformer =>
            q"""
               new _root_.io.scalaland.chimney.Transformer[$From, $To] {
                 def transform($srcName: $From): $To = {
                   $transformerTree
                 }
               }
            """
          case pt: DerivationTarget.PartialTransformer =>
            q"""
               new _root_.io.scalaland.chimney.PartialTransformer[$From, $To] {
                 def transform($srcName: $From, ${pt.failFastTermName}: Boolean): ${pt.targetType(To)} = {
                   $transformerTree
                 }
               }
            """
          case DerivationTarget.LiftedTransformer(f, _, _) =>
            q"""
               new _root_.io.scalaland.chimney.TransformerF[$f, $From, $To] {
                 def transform($srcName: $From): ${f.applyTypeArg(To)} = {
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
      srcName: TermName,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    resolveTransformerBody(srcPrefixTree, config)(From, To).map {
      case TransformerBodyTree(tree, derivedTarget) =>
        (config.derivationTarget, derivedTarget) match {
          case (DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _), DerivationTarget.TotalTransformer) =>
            q"${wrapperSupportInstance}.pure[$To]($tree)"
          case (DerivationTarget.PartialTransformer(_), DerivationTarget.TotalTransformer) =>
            q"_root_.io.scalaland.chimney.PartialTransformer.Result.Value[$To]($tree)"
          case _ =>
            tree
        }
    }
  }

  def expandTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    resolveImplicitTransformer(config)(From, To)
      .map(localImplicitTree => Right(localImplicitTree.callTransform(srcPrefixTree)))
      .getOrElse {
        deriveTransformerTree(srcPrefixTree, config)(From, To)
      }
  }

  def deriveTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
    if (isSubtype(From, To)) {
      expandSubtypes(srcPrefixTree, config)
    } else if (fromValueClassToType(From, To)) {
      expandValueClassToType(srcPrefixTree, config)(From, To)
    } else if (fromTypeToValueClass(From, To)) {
      expandTypeToValueClass(srcPrefixTree, config)(From, To)
    } else if (bothOptions(From, To)) {
      expandOptions(srcPrefixTree, config)(From, To)
    } else if (isOption(To)) {
      expandTargetWrappedInOption(srcPrefixTree, config)(From, To)
    } else if (config.flags.unsafeOption && isOption(From)) {
      expandSourceWrappedInOption(srcPrefixTree, config)(From, To)
    } else if (bothEithers(From, To)) {
      expandEithers(srcPrefixTree, config)(From, To)
    } else if (isMap(From)) {
      expandFromMap(srcPrefixTree, config)(From, To)
    } else if (bothOfIterableOrArray(From, To)) {
      expandIterableOrArray(srcPrefixTree, config)(From, To)
    } else if (isTuple(To)) {
      expandDestinationTuple(srcPrefixTree, config)(From, To)
    } else if (destinationCaseClass(To)) {
      expandDestinationCaseClass(srcPrefixTree, config)(From, To)
    } else if (config.flags.beanSetters && destinationJavaBean(To)) {
      expandDestinationJavaBean(srcPrefixTree, config)(From, To)
    } else if (bothSealedClasses(From, To)) {
      expandSealedClasses(srcPrefixTree, config)(From, To)
    } else {
      notSupportedDerivation(srcPrefixTree, From, To)
    }

  }

  def expandSubtypes(
      srcPrefixTree: Tree,
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Tree] = {
    Right {
      mkTransformerBodyTree0(config.derivationTarget)(srcPrefixTree)
    }
  }

  def expandValueClassToType(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type
  ): Either[Seq[TransformerDerivationError], Tree] = {

    From.valueClassMember
      .map { member =>
        Right {
          mkTransformerBodyTree0(config.derivationTarget) {
            q"$srcPrefixTree.${member.name}"
          }
        }
      }
      .getOrElse {
        // $COVERAGE-OFF$
        Left {
          Seq(CantFindValueClassMember(From.typeSymbol.name.toString, To.typeSymbol.name.toString))
        }
        // $COVERAGE-ON$
      }
  }

  def expandTypeToValueClass(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type
  ): Either[Seq[TransformerDerivationError], Tree] = {
    Right {
      mkTransformerBodyTree0(config.derivationTarget) {
        q"new $To($srcPrefixTree)"
      }
    }
  }

  def expandTargetWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
    if (To <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val optFrom = c.typecheck(tq"_root_.scala.Option[$From]", c.TYPEmode).tpe
      expandOptions(q"_root_.scala.Option[$From]($srcPrefixTree)", config)(optFrom, To)
    }
  }

  def expandSourceWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
    if (From <:< noneTpe || config.derivationTarget.isInstanceOf[DerivationTarget.PartialTransformer]) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fromInnerT = From.typeArgs.head
      val innerSrcPrefix = q"$srcPrefixTree.get"
      resolveRecursiveTransformerBody(innerSrcPrefix, config.rec)(fromInnerT, To)
        .map { innerTransformerBody =>
          val fn = freshTermName(innerSrcPrefix).toString
          mkTransformerBodyTree1(To, Target(fn, To), innerTransformerBody, config.derivationTarget) { tree =>
            q"($tree)"
          }
        }
    }
  }

  def expandOptions(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    def fromInnerT = From.typeArgs.head
    def toInnerT = To.typeArgs.head

    if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fn = Ident(freshTermName(srcPrefixTree))
      resolveRecursiveTransformerBody(fn, config.rec)(fromInnerT, toInnerT)
        .map {
          case TransformerBodyTree(innerTree, DerivationTarget.TotalTransformer) =>
            mkTransformerBodyTree0(config.derivationTarget) {
              q"$srcPrefixTree.map(($fn: $fromInnerT) => $innerTree)"
            }

          case TransformerBodyTree(innerTree, DerivationTarget.PartialTransformer(_)) =>
            q"""
              $srcPrefixTree.fold[_root_.io.scalaland.chimney.PartialTransformer.Result[$To]](
                _root_.io.scalaland.chimney.PartialTransformer.Result.fromValue(Option.empty[$toInnerT])
              )(
                ($fn: $fromInnerT) => $innerTree.map(Option.apply[$toInnerT])
              )
            """

          case TransformerBodyTree(
              innerTree,
              DerivationTarget.LiftedTransformer(wrapperType, wrapperSupportInstance, _)
              ) =>
            q"""
              $srcPrefixTree.fold[${wrapperType.applyTypeArg(To)}](
                ${wrapperSupportInstance}.pure(Option.empty[$toInnerT])
              )(
                ($fn: $fromInnerT) => ${wrapperSupportInstance}.map($innerTree, Option.apply[$toInnerT])
              )
            """
        }
    }
  }

  def expandEithers(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    val List(fromLeftT, fromRightT) = From.typeArgs
    val List(toLeftT, toRightT) = To.typeArgs

    val fnL = Ident(freshTermName("left"))
    val fnR = Ident(freshTermName("right"))

    if (From <:< leftTpe && !(To <:< rightTpe)) {
      resolveRecursiveTransformerBody(q"$srcPrefixTree.value", config.rec)(fromLeftT, toLeftT)
        .map { tbt =>
          mkTransformerBodyTree1(To, Target(fnL.name.toString, toLeftT), tbt, config.derivationTarget) { leftArgTree =>
            q"new _root_.scala.util.Left($leftArgTree)"
          }
        }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      resolveRecursiveTransformerBody(q"$srcPrefixTree.value", config.rec)(fromRightT, toRightT)
        .map { tbt =>
          mkTransformerBodyTree1(To, Target(fnR.name.toString, toRightT), tbt, config.derivationTarget) {
            rightArgTree =>
              q"new _root_.scala.util.Right($rightArgTree)"
          }
        }
    } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
      val leftTransformerE = resolveRecursiveTransformerBody(fnL, config.rec)(fromLeftT, toLeftT)
      val rightTransformerE = resolveRecursiveTransformerBody(fnR, config.rec)(fromRightT, toRightT)

      (leftTransformerE, rightTransformerE) match {
        case (Right(leftTbt), Right(rightTbt)) =>
          val targetTpe = config.derivationTarget.targetType(To)
          val leftN = freshTermName("left")
          val rightN = freshTermName("right")

          val leftBody = mkTransformerBodyTree1(To, Target(leftN.toString, toLeftT), leftTbt, config.derivationTarget) {
            leftArgTree =>
              q"new _root_.scala.util.Left($leftArgTree)"
          }

          val rightBody =
            mkTransformerBodyTree1(To, Target(rightN.toString, toRightT), rightTbt, config.derivationTarget) {
              rightArgTree =>
                q"new _root_.scala.util.Right($rightArgTree)"
            }

          Right {
            q"""
              $srcPrefixTree.fold[$targetTpe](
                ($fnL: $fromLeftT) => $leftBody,
                ($fnR: $fromRightT) => $rightBody
              )
            """
          }
        case _ =>
          Left(leftTransformerE.left.getOrElse(Nil) ++ rightTransformerE.left.getOrElse(Nil))
      }
    } else {
      notSupportedDerivation(srcPrefixTree, From, To)
    }
  }

  def expandFromMap(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
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

        val keyTransformerE = resolveRecursiveTransformerBody(fnK, config.rec)(fromKeyT, toKeyT)
        val valueTransformerE = resolveRecursiveTransformerBody(fnV, config.rec)(fromValueT, toValueT)

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
                case _: DerivationTarget.PartialTransformer =>
                  c.abort(c.enclosingPosition, "Not supported for partial transformers!")
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
                case _: DerivationTarget.PartialTransformer =>
                  c.abort(c.enclosingPosition, "Not supported for partial transformers!")
              }

            Right(
              q"""${wrapperSupportInstance}.traverse[$To, $WrappedToInnerT, $ToInnerT](
                  $srcPrefixTree.iterator.map[$WrappedToInnerT] {
                    case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
                      ${wrapperSupportInstance}.product[$toKeyT, $toValueT](
                        $keyTransformerWithPath,
                        $valueTransformerWithPath
                      )
                  },
                  _root_.scala.Predef.identity[$WrappedToInnerT]
                )
             """
            )
          case _ =>
            Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
        }

      case (pt @ DerivationTarget.PartialTransformer(_), List(toKeyT, toValueT)) =>
        val List(fromKeyT, fromValueT) = From.typeArgs

        val fnK = Ident(freshTermName("k"))
        val fnV = Ident(freshTermName("v"))

        val keyTransformerE = resolveRecursiveTransformerBody(fnK, config.rec)(fromKeyT, toKeyT)
        val valueTransformerE = resolveRecursiveTransformerBody(fnV, config.rec)(fromValueT, toValueT)

        (keyTransformerE, valueTransformerE) match {
          case (Right(keyTransformer), Right(valueTransformer)) =>
            val keyTransformerWithPath =
              keyTransformer.target match {
                case _: DerivationTarget.PartialTransformer =>
                  q"""
                     ${keyTransformer.tree}.wrapErrorPaths(
                       _root_.io.scalaland.chimney.PartialTransformer.ErrorPath.MapKey($fnK, _)
                     )
                   """
                case DerivationTarget.TotalTransformer =>
                  q"_root_.io.scalaland.chimney.PartialTransformer.Result.Value[$toKeyT](${keyTransformer.tree})"
                case _: DerivationTarget.LiftedTransformer =>
                  c.abort(c.enclosingPosition, "Not supported for lifted transformers!")
              }

            val valueTransformerWithPath =
              valueTransformer.target match {
                case _: DerivationTarget.PartialTransformer =>
                  q"""
                     ${valueTransformer.tree}.wrapErrorPaths(
                       _root_.io.scalaland.chimney.PartialTransformer.ErrorPath.MapValue($fnK, _)
                     )
                   """
                case DerivationTarget.TotalTransformer =>
                  q"_root_.io.scalaland.chimney.PartialTransformer.Result.Value[$toValueT](${valueTransformer.tree})"
                case _: DerivationTarget.LiftedTransformer =>
                  c.abort(c.enclosingPosition, "Not supported for lifted transformers!")
              }

            Right(
              q"""
                 _root_.io.scalaland.chimney.PartialTransformer.Result.traverse[$To, ($fromKeyT, $fromValueT), ($toKeyT, $toValueT)](
                   $srcPrefixTree.iterator,
                   { case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
                       _root_.io.scalaland.chimney.PartialTransformer.Result.product[$toKeyT, $toValueT](
                         $keyTransformerWithPath,
                         $valueTransformerWithPath,
                         ${pt.failFastTree}
                       )
                   },
                   ${pt.failFastTree}
                 )
               """
            )
          case _ =>
            Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
        }

      case _ =>
        expandIterableOrArray(srcPrefixTree, config)(From, To)
    }
  }

  def expandIterableOrArray(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    val FromInnerT = From.collectionInnerTpe
    val ToInnerT = To.collectionInnerTpe

    val fn = Ident(freshTermName(srcPrefixTree))

    resolveRecursiveTransformerBody(fn, config.rec)(FromInnerT, ToInnerT)
      .map {
        case TransformerBodyTree(
            innerTransformerTree,
            DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, Some(wrapperErrorPathSupportInstance))
            ) =>
          val idx = Ident(freshTermName("idx"))

          q"""${wrapperSupportInstance}.traverse[$To, ($FromInnerT, _root_.scala.Int), $ToInnerT](
            $srcPrefixTree.iterator.zipWithIndex,
            { case (${fn.name}: $FromInnerT, ${idx.name}: _root_.scala.Int) =>
              ${wrapperErrorPathSupportInstance}.addPath[$ToInnerT](
                $innerTransformerTree,
                _root_.io.scalaland.chimney.ErrorPathNode.Index($idx)
              )
            }
          )
          """
        case TransformerBodyTree(
            innerTransformerTree,
            DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, None)
            ) =>
          q"""
            ${wrapperSupportInstance}.traverse[$To, $FromInnerT, $ToInnerT](
              $srcPrefixTree.iterator,
              ($fn: $FromInnerT) => $innerTransformerTree
            )
          """
        case TransformerBodyTree(innerTransformerTree, pt @ DerivationTarget.PartialTransformer(_)) =>
          val idx = Ident(freshTermName("idx"))
          q"""
            _root_.io.scalaland.chimney.PartialTransformer.Result.traverse[$To, ($FromInnerT, _root_.scala.Int), $ToInnerT](
              $srcPrefixTree.iterator.zipWithIndex,
              { case (${fn.name}: $FromInnerT, ${idx.name}: _root_.scala.Int) =>
                $innerTransformerTree.wrapErrorPaths(PartialTransformer.ErrorPath.Index($idx, _))
              },
              ${pt.failFastTree}
            )
          """
        case TransformerBodyTree(innerTransformerTree, DerivationTarget.TotalTransformer) =>
          def isTransformationIdentity = fn == innerTransformerTree
          def sameCollectionTypes = From.typeConstructor =:= To.typeConstructor

          val transformedCollectionTree: Tree = (isTransformationIdentity, sameCollectionTypes) match {
            case (true, true) =>
              // identity transformation, same collection types
              srcPrefixTree

            case (true, false) =>
              // identity transformation, different collection types
              srcPrefixTree.convertCollection(To, ToInnerT)

            case (false, true) =>
              // non-trivial transformation, same collection types
              q"$srcPrefixTree.map(($fn: $FromInnerT) => $innerTransformerTree)"

            case (false, false) =>
              q"$srcPrefixTree.iterator.map(($fn: $FromInnerT) => $innerTransformerTree)"
                .convertCollection(To, ToInnerT)
          }

          mkTransformerBodyTree0(config.derivationTarget)(transformedCollectionTree)
      }
  }

  def expandSealedClasses(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    resolveCoproductInstance(srcPrefixTree, From, To, config)
      .map { instanceTree =>
        Right(instanceTree)
      }
      .getOrElse {
        val fromCS = From.typeSymbol.classSymbolOpt.get
        val toCS = To.typeSymbol.classSymbolOpt.get

        val fromInstances = fromCS.subclasses.map(_.typeInSealedParent(From))
        val toInstances = toCS.subclasses.map(_.typeInSealedParent(To))

        val targetNamedInstances = toInstances.groupBy(_.typeSymbol.name.toString)

        val instanceClauses = fromInstances.map { instTpe =>
          val instName = instTpe.typeSymbol.name.toString

          resolveCoproductInstance(srcPrefixTree, instTpe, To, config)
            .map { instanceTree =>
              Right(cq"_: $instTpe => $instanceTree")
            }
            .getOrElse {
              val instSymbol = instTpe.typeSymbol
              targetNamedInstances.getOrElse(instName, Nil) match {
                case List(matchingTargetTpe)
                    if (instSymbol.isModuleClass || instSymbol.isCaseClass) && matchingTargetTpe.typeSymbol.isModuleClass =>
                  val tree = mkTransformerBodyTree0(config.derivationTarget) {
                    q"${matchingTargetTpe.typeSymbol.asClass.module}"
                  }
                  Right(cq"_: ${instSymbol.asType} => $tree")
                case List(matchingTargetTpe) if instSymbol.isCaseClass && matchingTargetTpe.typeSymbol.isCaseClass =>
                  val fn = freshTermName(instName)
                  expandDestinationCaseClass(Ident(fn), config.rec)(instTpe, matchingTargetTpe)
                    .map { innerTransformerTree =>
                      cq"$fn: $instTpe => $innerTransformerTree"
                    }
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
                case _ =>
                  Left {
                    Seq(
                      CantFindCoproductInstanceTransformer(
                        instSymbol.fullName,
                        From.typeSymbol.fullName,
                        To.typeSymbol.fullName
                      )
                    )
                  }
              }
            }
        }

        if (instanceClauses.forall(_.isRight)) {
          val clauses = instanceClauses.collect { case Right(clause) => clause }
          Right {
            q"$srcPrefixTree match { case ..$clauses }"
          }
        } else {
          Left {
            instanceClauses.collect { case Left(derivationErrors) => derivationErrors }.flatten
          }
        }
      }

  }

  def resolveCoproductInstance(
      srcPrefixTree: Tree,
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Option[Tree] = {
    config.derivationTarget match {
      case DerivationTarget.LiftedTransformer(_, _, _) if config.coproductInstancesF.contains((From.typeSymbol, To)) =>
        Some(
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            srcPrefixTree,
            From.typeSymbol,
            To,
            config.derivationTarget
          )
        )

      case DerivationTarget.PartialTransformer(_) if config.coproductInstancesPartial.contains((From.typeSymbol, To)) =>
        Some(
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            srcPrefixTree,
            From.typeSymbol,
            To,
            config.derivationTarget
          )
        )

      case _ if config.coproductInstances.contains((From.typeSymbol, To)) =>
        Some(
          mkTransformerBodyTree0(config.derivationTarget) {
            mkCoproductInstance(
              config.transformerDefinitionPrefix,
              srcPrefixTree,
              From.typeSymbol,
              To,
              DerivationTarget.TotalTransformer
            )
          }
        )
      case _ =>
        None
    }
  }

  def expandDestinationTuple(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    resolveSourceTupleAccessors(From, To)
      .flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      }
      .map { transformerBodyPerTarget =>
        val targets = To.caseClassParams.map(Target.fromField(_, To))
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

        mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
          mkNewClass(To, args)
        }
      }
  }

  def expandDestinationCaseClass(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {
    val targets = To.caseClassParams.map(Target.fromField(_, To))

    val targetTransformerBodiesMapping = if (isTuple(From)) {
      resolveSourceTupleAccessors(From, To).flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      }
    } else {
      val overridesMapping = resolveOverrides(srcPrefixTree, From, targets, config)
      val notOverridenTargets = targets.diff(overridesMapping.keys.toSeq)
      val accessorsMapping = resolveAccessorsMapping(From, notOverridenTargets, config)

      resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
        .map(_ ++ overridesMapping)
    }

    targetTransformerBodiesMapping.map { transformerBodyPerTarget =>
      val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

      mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
        mkNewClass(To, args)
      }
    }
  }

  def expandDestinationJavaBean(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

    val beanSetters = To.beanSetterMethods
    val targets = beanSetters.map(Target.fromJavaBeanSetter(_, To))

    val accessorsMapping = resolveAccessorsMapping(From, targets, config)

    resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      .map { transformerBodyPerTarget =>
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
        mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
          mkNewJavaBean(To, targets zip args)
        }
      }
  }

  def resolveTransformerBodyTreeFromAccessorsMapping(
      srcPrefixTree: Tree,
      accessorsMapping: Map[Target, AccessorResolution],
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Map[Target, TransformerBodyTree]] = {

    val (erroredTargets, resolvedBodyTrees) = accessorsMapping.map {
      case (target, accessor: AccessorResolution.Resolved) =>
        target -> resolveTransformerBodyTreeFromAccessor(srcPrefixTree, target, accessor, From, config)
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
      srcPrefixTree: Tree,
      target: Target,
      accessor: AccessorResolution.Resolved,
      From: Type,
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], TransformerBodyTree] = {
    val resolved = resolveRecursiveTransformerBody(
      q"$srcPrefixTree.${accessor.symbol.name}",
      config
    )(
      accessor.symbol.resultTypeIn(From),
      target.tpe
    )

    (resolved, config.derivationTarget) match {
      case (Right(bodyTree), DerivationTarget.LiftedTransformer(_, _, Some(errorPathSupport)))
          if bodyTree.isLiftedTarget =>
        Right {
          TransformerBodyTree(
            q"""$errorPathSupport.addPath[${target.tpe}](
                 ${bodyTree.tree},
                 _root_.io.scalaland.chimney.ErrorPathNode.Accessor(${accessor.symbol.name.toString})
               )""",
            config.derivationTarget
          )
        }
      case (Right(bodyTree), DerivationTarget.PartialTransformer(_)) if bodyTree.isPartialTarget =>
        Right {
          TransformerBodyTree(
            q"""
              ${bodyTree.tree}.wrapErrorPaths(
                _root_.io.scalaland.chimney.PartialTransformer.ErrorPath.Accessor(${accessor.symbol.name.toString}, _)
              )
            """,
            config.derivationTarget
          )
        }
      case _ => resolved
    }
  }

  def resolveRecursiveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], TransformerBodyTree] = {
    resolveTransformerBody(srcPrefixTree, config.rec)(From, To)
  }

  def resolveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[TransformerDerivationError], TransformerBodyTree] = {
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
            Right(TransformerBodyTree(localImplicitTreeF.callTransform(srcPrefixTree), config.derivationTarget))
          case (None, Some(localImplicitTree)) =>
            Right(
              TransformerBodyTree(localImplicitTree.callTransform(srcPrefixTree), DerivationTarget.TotalTransformer)
            )
          case (None, None) =>
            deriveTransformerTree(srcPrefixTree, config)(From, To)
              .map(tree => TransformerBodyTree(tree, config.derivationTarget))
        }
      case pt @ DerivationTarget.PartialTransformer(_) =>
        val implicitPartialTransformer = resolveImplicitTransformer(config)(From, To)
        val implicitTransformer = findLocalImplicitTransformer(From, To, DerivationTarget.TotalTransformer)

        (implicitPartialTransformer, implicitTransformer) match {
          case (Some(localImplicitTreePartial), Some(localImplicitTree)) =>
            // TODO: manage ambiguity with DSL flag
            c.abort(
              c.enclosingPosition,
              s"""Ambiguous implicits while resolving Chimney recursive transformation:
                 |
                 |PartialTransformer[$From, $To]: $localImplicitTreePartial
                 |Transformer[$From, $To]: $localImplicitTree
                 |
                 |Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used
                 |""".stripMargin
            )
          case (Some(localImplicitTreePartial), None) =>
            Right(
              TransformerBodyTree(
                localImplicitTreePartial.callPartialTransform(srcPrefixTree, pt.failFastTree),
                config.derivationTarget
              )
            )
          case (None, Some(localImplicitTree)) =>
            Right(
              TransformerBodyTree(
                localImplicitTree.callTransform(srcPrefixTree),
                DerivationTarget.TotalTransformer
              )
            )
          case (None, None) =>
            deriveTransformerTree(srcPrefixTree, config)(From, To)
              .map(tree => TransformerBodyTree(tree, config.derivationTarget))
        }
      case DerivationTarget.TotalTransformer =>
        expandTransformerTree(srcPrefixTree, config)(From, To)
          .map(tree => TransformerBodyTree(tree, config.derivationTarget))
    }
  }

  def resolveImplicitTransformer(config: TransformerConfig)(From: Type, To: Type): Option[Tree] = {
    if (config.definitionScope.contains((From, To))) {
      None
    } else {
      findLocalImplicitTransformer(From, To, config.derivationTarget)
    }
  }

  def findLocalTransformerConfigurationFlags: Tree = {
    val searchTypeTree =
      tq"${typeOf[io.scalaland.chimney.dsl.TransformerConfiguration[_ <: io.scalaland.chimney.internal.TransformerFlags]]}"
    inferImplicitTpe(searchTypeTree, macrosDisabled = true)
      .getOrElse {
        // $COVERAGE-OFF$
        c.abort(c.enclosingPosition, "Can't locate implicit TransformerConfiguration!")
        // $COVERAGE-ON$
      }
  }

  private def findLocalImplicitTransformer(From: Type, To: Type, derivationTarget: DerivationTarget): Option[Tree] = {
    val searchTypeTree: Tree = derivationTarget match {
      case DerivationTarget.LiftedTransformer(f, _, _) =>
        tq"_root_.io.scalaland.chimney.TransformerF[$f, $From, $To]"
      case DerivationTarget.PartialTransformer(_) =>
        tq"_root_.io.scalaland.chimney.PartialTransformer[$From, $To]"
      case DerivationTarget.TotalTransformer =>
        tq"_root_.io.scalaland.chimney.Transformer[$From, $To]"
    }

    inferImplicitTpe(searchTypeTree, macrosDisabled = false).filterNot(isDeriving)
  }

  def findTransformerErrorPathSupport(wrapperType: Type): Option[Tree] = {
    inferImplicitTpe(tq"_root_.io.scalaland.chimney.TransformerFErrorPathSupport[$wrapperType]", macrosDisabled = true)
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
