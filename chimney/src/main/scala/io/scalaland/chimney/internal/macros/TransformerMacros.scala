package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}

import scala.reflect.macros.blackbox

trait TransformerMacros extends TransformerConfiguration with MappingMacros with TargetConstructorMacros {
  this: DerivationGuards with MacroUtils with EitherUtils =>

  val c: blackbox.Context

  import c.universe._

  def buildDefinedTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      tfsTree: c.Tree = c.universe.EmptyTree
  ): c.Tree = {
    val C = weakTypeOf[C]
    val config = captureTransformerConfig(C).copy(
      definitionScope = Some((weakTypeOf[From], weakTypeOf[To])),
      wrapperSupportInstance = tfsTree
    )

    if (!config.valueLevelAccessNeeded) {
      genTransformer[From, To](config)
    } else {
      val tdName = TermName(c.freshName("td"))
      val derivedTransformer = genTransformer[From, To](config.copy(transformerDefinitionPrefix = q"$tdName"))

      q"""
        val $tdName = ${c.prefix.tree}
        $derivedTransformer
      """
    }
  }

  def expandTransform[From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag](
      tfsTree: c.Tree = c.universe.EmptyTree
  ): c.Tree = {
    val C = weakTypeOf[C]
    val tiName = TermName(c.freshName("ti"))
    val config = captureTransformerConfig(C)
      .copy(
        transformerDefinitionPrefix = q"$tiName.td",
        wrapperSupportInstance = tfsTree
      )

    val derivedTransformerTree = genTransformer[From, To](config)

    q"""
       val $tiName = ${c.prefix.tree}
       $derivedTransformerTree.transform($tiName.source)
    """
  }

  def genTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag](
      config: TransformerConfig
  ): c.Tree = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName = freshTermName(From)
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    expandTransformerTree(srcPrefixTree, config)(From, To) match {

      case Right(transformerTree) =>
        config.wrapperType match {
          case Some(f) =>
            q"""
               new _root_.io.scalaland.chimney.TransformerF[$f, $From, $To] {
                 def transform($srcName: $From): ${f.applyTypeArg(To)} = {
                   $transformerTree
                 }
               }
            """

          case None =>
            q"""
               new _root_.io.scalaland.chimney.Transformer[$From, $To] {
                 def transform($srcName: $From): $To = {
                   $transformerTree
                 }
               }
            """
        }

      case Left(derivationErrors) =>
        val errorMessage =
          s"""Chimney can't derive transformation from $From to $To
             |
             |${DerivationError.printErrors(derivationErrors)}
             |Consult $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        c.abort(c.enclosingPosition, errorMessage)
    }
  }

  def expandTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val localInstance: Option[Tree] =
      if (config.definitionScope.contains((From, To))) {
        None
      } else {
        findLocalImplicitTransformer(From, To, config.wrapperType)
      }

    localInstance
      .map { localImplicitTree =>
        Right(q"$localImplicitTree.transform($srcPrefixTree)")
      }
      .getOrElse {
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
        } else if (config.enableUnsafeOption && isOption(From)) {
          expandSourceWrappedInOption(srcPrefixTree, config)(From, To)
        } else if (bothEithers(From, To)) {
          expandEithers(srcPrefixTree, config)(From, To)
        } else if (bothOfIterableOrArray(From, To)) {
          expandIterableOrArray(srcPrefixTree, config)(From, To)
        } else if (isTuple(To)) {
          expandDestinationTuple(srcPrefixTree, config)(From, To)
        } else if (destinationCaseClass(To)) {
          expandDestinationCaseClass(srcPrefixTree, config)(From, To)
        } else if (config.enableBeanSetters && destinationJavaBean(To)) {
          expandDestinationJavaBean(srcPrefixTree, config)(From, To)
        } else if (bothSealedClasses(From, To)) {
          expandSealedClasses(srcPrefixTree, config)(From, To)
        } else {
          notSupportedDerivation(srcPrefixTree, From, To)
        }
      }
  }

  def expandSubtypes(
      srcPrefixTree: Tree,
      config: TransformerConfig
  ): Either[Seq[DerivationError], Tree] = {
    Right {
      mkWrappedTransformerBody0(config)(srcPrefixTree)
    }
  }

  def expandValueClassToType(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(
      From: Type,
      To: Type
  ): Either[Seq[DerivationError], Tree] = {

    From.valueClassMember
      .map { member =>
        Right {
          mkWrappedTransformerBody0(config) {
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
  ): Either[Seq[DerivationError], Tree] = {
    Right {
      mkWrappedTransformerBody0(config) {
        q"new $To($srcPrefixTree)"
      }
    }
  }

  def expandTargetWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
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
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
    if (From <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fromInnerT = From.typeArgs.head
      val innerSrcPrefix = q"$srcPrefixTree.get"
      resolveRecursiveTransformerBody(innerSrcPrefix, config.rec)(fromInnerT, To)
        .mapRight { innerTransformerBody =>
          val fn = freshTermName(innerSrcPrefix).toString
          mkWrappedTransformerBody1(To, Target(fn, To), innerTransformerBody, config) { tree =>
            q"($tree)"
          }
        }
    }
  }

  def expandOptions(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    def fromInnerT = From.typeArgs.head
    def toInnerT = To.typeArgs.head

    if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fn = Ident(freshTermName(srcPrefixTree))
      resolveRecursiveTransformerBody(fn, config.rec)(fromInnerT, toInnerT)
        .mapRight {
          case TransformerBodyTree(innerTree, false) =>
            mkWrappedTransformerBody0(config) {
              q"$srcPrefixTree.map(($fn: $fromInnerT) => $innerTree)"
            }

          case TransformerBodyTree(innerTree, true) =>
            q"""
              $srcPrefixTree.fold[${config.wrapperType.get.applyTypeArg(To)}](
                ${config.wrapperSupportInstance}.pure(Option.empty[$toInnerT])
              )(
                ($fn: $fromInnerT) => ${config.wrapperSupportInstance}.map($innerTree, Option.apply[$toInnerT])
              )
            """
        }
    }
  }

  def expandEithers(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromLeftT, fromRightT) = From.typeArgs
    val List(toLeftT, toRightT) = To.typeArgs

    val fnL = Ident(freshTermName("left"))
    val fnR = Ident(freshTermName("right"))

    if (From <:< leftTpe && !(To <:< rightTpe)) {
      resolveRecursiveTransformerBody(srcPrefixTree.getLeftTree, config.rec)(fromLeftT, toLeftT)
        .mapRight { tbt =>
          mkWrappedTransformerBody1(To, Target(fnL.name.toString, toLeftT), tbt, config) { leftArgTree =>
            q"new _root_.scala.util.Left($leftArgTree)"
          }
        }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      resolveRecursiveTransformerBody(srcPrefixTree.getRightTree, config.rec)(fromRightT, toRightT)
        .mapRight { tbt =>
          mkWrappedTransformerBody1(To, Target(fnR.name.toString, toRightT), tbt, config) { rightArgTree =>
            q"new _root_.scala.util.Right($rightArgTree)"
          }
        }
    } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
      val leftTransformerE = resolveRecursiveTransformerBody(fnL, config.rec)(fromLeftT, toLeftT)
      val rightTransformerE = resolveRecursiveTransformerBody(fnR, config.rec)(fromRightT, toRightT)

      (leftTransformerE, rightTransformerE) match {
        case (Right(leftTbt), Right(rightTbt)) =>
          val targetTpe = config.wrapperType.map(_.applyTypeArg(To)).getOrElse(To)
          val leftN = freshTermName("left")
          val rightN = freshTermName("right")

          val leftBody = mkWrappedTransformerBody1(To, Target(leftN.toString, toLeftT), leftTbt, config) {
            leftArgTree =>
              q"new _root_.scala.util.Left($leftArgTree)"
          }

          val rightBody = mkWrappedTransformerBody1(To, Target(rightN.toString, toRightT), rightTbt, config) {
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

  def expandIterableOrArray(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val FromInnerT = From.collectionInnerTpe
    val ToInnerT = To.collectionInnerTpe

    val fn = Ident(freshTermName(srcPrefixTree))

    resolveRecursiveTransformerBody(fn, config.rec)(FromInnerT, ToInnerT)
      .mapRight {
        case TransformerBodyTree(innerTransformerTree, true) =>
          if (config.wrapperType.isDefined) {
            q"""
              ${config.wrapperSupportInstance}
                .traverse[$To, $FromInnerT, $ToInnerT](
                  $srcPrefixTree.iterator,
                  ($fn: $FromInnerT) => $innerTransformerTree
                )
            """
          } else {
            // this case is not possible due to resolveRecursiveTransformerBody semantics: it will not
            // even search for lifted inner transformer when wrapper type is not requested
            // $COVERAGE-OFF$
            c.abort(c.enclosingPosition, "Impossible case!")
            // $COVERAGE-ON$
          }

        case TransformerBodyTree(innerTransformerTree, false) =>
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

          if (config.wrapperType.isDefined) {
            q"${config.wrapperSupportInstance}.pure($transformedCollectionTree)"
          } else {
            transformedCollectionTree
          }
      }
  }

  def expandSealedClasses(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    resolveCoproductInstance(srcPrefixTree, From, To, config)
      .map { instanceTree =>
        Right(instanceTree)
      }
      .getOrElse {
        val fromCS = From.typeSymbol.classSymbolOpt.get
        val toCS = To.typeSymbol.classSymbolOpt.get

        val fromInstances = fromCS.knownDirectSubclasses.toSeq.map(_.typeInSealedParent(From))
        val toInstances = toCS.knownDirectSubclasses.toSeq.map(_.typeInSealedParent(To))

        val targetNamedInstances = toInstances.map(tpe => tpe.typeSymbol.name.toString -> tpe).toMap

        val instanceClauses = fromInstances.map { instTpe =>
          val instName = instTpe.typeSymbol.name.toString

          resolveCoproductInstance(srcPrefixTree, instTpe, To, config)
            .map { instanceTree =>
              val fn = freshTermName(instName)
              Right(cq"$fn: $instTpe => $instanceTree")
            }
            .getOrElse {
              val instSymbol = instTpe.typeSymbol
              targetNamedInstances.get(instName) match {
                case Some(matchingTargetTpe)
                    if (instSymbol.isModuleClass || instSymbol.isCaseClass) && matchingTargetTpe.typeSymbol.isModuleClass =>
                  val tree = mkWrappedTransformerBody0(config) {
                    q"${matchingTargetTpe.typeSymbol.asClass.module}"
                  }
                  Right(cq"_: ${instSymbol.asType} => $tree")
                case Some(matchingTargetTpe) if instSymbol.isCaseClass && matchingTargetTpe.typeSymbol.isCaseClass =>
                  val fn = freshTermName(instName)
                  expandDestinationCaseClass(Ident(fn), config.rec)(instTpe, matchingTargetTpe)
                    .mapRight { innerTransformerTree =>
                      cq"$fn: $instTpe => $innerTransformerTree"
                    }
                case _ =>
                  Left {
                    Seq(
                      CantFindCoproductInstanceTransformer(
                        instSymbol.fullName,
                        From.typeSymbol.fullName.toString,
                        To.typeSymbol.fullName.toString
                      )
                    )
                  }
              }
            }
        }

        if (instanceClauses.forall(_.isRight)) {
          val clauses = instanceClauses.map(_.getRight)
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
    if (config.wrapperType.isDefined && config.coproductInstancesF.contains((From.typeSymbol, To))) {
      Some(
        mkCoproductInstance(
          config.transformerDefinitionPrefix,
          srcPrefixTree,
          From.typeSymbol,
          To,
          config.wrapperType
        )
      )
    } else if (config.coproductInstances.contains((From.typeSymbol, To))) {
      Some(
        mkWrappedTransformerBody0(config) {
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            srcPrefixTree,
            From.typeSymbol,
            To,
            config.wrapperType
          )
        }
      )
    } else {
      None
    }
  }

  def expandDestinationTuple(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    resolveSourceTupleAccessors(From, To)
      .flatMapRight { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping.map {
          case (k, v) => k -> Some(v)
        }, From, To, config)
      }
      .mapRight { transformerBodyPerTarget =>
        val targets = To.caseClassParams.map(Target.fromField(_, To))
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

        mkWrappedTransformerBody(To, targets, bodyTreeArgs, config) { args =>
          mkNewClass(To, args)
        }
      }
  }

  def expandDestinationCaseClass(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val targets = To.caseClassParams.map(Target.fromField(_, To))

    val targetTransformerBodiesMapping = if (isTuple(From)) {
      resolveSourceTupleAccessors(From, To).flatMapRight { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping.map {
          case (k, v) => k -> Some(v)
        }, From, To, config)
      }
    } else {
      val overridesMapping = resolveOverrides(srcPrefixTree, From, targets, config)
      val notOverridenTargets = targets.diff(overridesMapping.keys.toSeq)
      val accessorsMapping = resolveAccessorsMapping(From, notOverridenTargets, config)

      resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
        .mapRight(_ ++ overridesMapping)
    }

    targetTransformerBodiesMapping.mapRight { transformerBodyPerTarget =>
      val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

      mkWrappedTransformerBody(To, targets, bodyTreeArgs, config) { args =>
        mkNewClass(To, args)
      }
    }
  }

  def expandDestinationJavaBean(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val beanSetters = To.beanSetterMethods
    val targets = beanSetters.map(Target.fromJavaBeanSetter(_, To))

    val accessorsMapping = resolveAccessorsMapping(From, targets, config)

    resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      .mapRight { transformerBodyPerTarget =>
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
        mkWrappedTransformerBody(To, targets, bodyTreeArgs, config) { args =>
          mkNewJavaBean(To, targets zip args)
        }
      }
  }

  def resolveTransformerBodyTreeFromAccessorsMapping(
      srcPrefixTree: Tree,
      accessorsMapping: Map[Target, Option[ResolvedAccessor]],
      From: Type,
      To: Type,
      config: TransformerConfig
  ): Either[Seq[DerivationError], Map[Target, TransformerBodyTree]] = {

    val (erroredTargets, resolvedBodyTrees) = accessorsMapping.map {
      case (target, Some(accessor)) =>
        target -> resolveTransformerBodyTreeFromAccessor(srcPrefixTree, target, accessor, From, config)
      case (target, None) =>
        target -> Left(
          Seq(
            MissingAccessor(
              fieldName = target.name,
              fieldTypeName = target.tpe.typeSymbol.fullName,
              sourceTypeName = From.typeSymbol.fullName,
              targetTypeName = To.typeSymbol.fullName
            )
          )
        )
    }.partitionEitherValues

    if (erroredTargets.isEmpty) {
      Right(resolvedBodyTrees)
    } else {
      val targetsToFallback = erroredTargets.collect {
        case (target, _) if accessorsMapping(target).isEmpty =>
          target
      }
      val fallbackTransformerBodies = resolveFallbackTransformerBodies(targetsToFallback, To, config)
      val unresolvedTargets = accessorsMapping.keySet
        .diff(resolvedBodyTrees.keySet)
        .diff(fallbackTransformerBodies.keySet)

      if (unresolvedTargets.isEmpty) {
        Right(resolvedBodyTrees ++ fallbackTransformerBodies)
      } else {
        val errors = unresolvedTargets.toSeq.flatMap { target =>
          erroredTargets(target) ++ accessorsMapping(target).map { accessor =>
            MissingTransformer(
              fieldName = target.name,
              sourceFieldTypeName = accessor.symbol.resultTypeIn(From).typeSymbol.fullName,
              targetFieldTypeName = target.tpe.typeSymbol.fullName,
              sourceTypeName = From.typeSymbol.fullName,
              targetTypeName = To.typeSymbol.fullName
            )
          }
        }
        Left(errors)
      }
    }
  }

  def resolveTransformerBodyTreeFromAccessor(
      srcPrefixTree: Tree,
      target: Target,
      accessor: ResolvedAccessor,
      From: Type,
      config: TransformerConfig
  ): Either[Seq[DerivationError], TransformerBodyTree] = {
    resolveRecursiveTransformerBody(
      q"$srcPrefixTree.${accessor.symbol.name}",
      config
    )(
      accessor.symbol.resultTypeIn(From),
      target.tpe
    )
  }

  def resolveRecursiveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerBodyTree] = {
    def resursiveTransformerBody =
      expandTransformerTree(srcPrefixTree, config.rec.copy(wrapperType = None))(From, To)
        .mapRight(tree => TransformerBodyTree(tree, isWrapped = false))

    def recursiveTransformerBodyWrapped =
      expandTransformerTree(srcPrefixTree, config.rec)(From, To)
        .mapRight(tree => TransformerBodyTree(tree, isWrapped = true))

    if (config.wrapperType.isDefined) {
      resursiveTransformerBody rightOrElse recursiveTransformerBodyWrapped
    } else {
      resursiveTransformerBody
    }
  }

  def findLocalImplicitTransformer(From: Type, To: Type, wrapperType: Option[Type]): Option[Tree] = {
    val searchTypeTree: Tree = wrapperType match {
      case Some(f) =>
        tq"_root_.io.scalaland.chimney.TransformerF[$f, $From, $To]"
      case None =>
        tq"_root_.io.scalaland.chimney.Transformer[$From, $To]"
    }

    val tpeTree = c.typecheck(
      tree = searchTypeTree,
      silent = true,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = true
    )

    scala.util
      .Try(c.inferImplicitValue(tpeTree.tpe, silent = true, withMacrosDisabled = true))
      .toOption
      .filterNot(_ == EmptyTree)
  }

  private def notSupportedDerivation(
      srcPrefixTree: Tree,
      fromTpe: Type,
      toTpe: Type
  ): Left[Seq[NotSupportedDerivation], Nothing] =
    Left {
      Seq(
        NotSupportedDerivation(
          toFieldName(srcPrefixTree),
          fromTpe.typeSymbol.fullName.toString,
          toTpe.typeSymbol.fullName.toString
        )
      )
    }

  private def freshTermName(srcPrefixTree: Tree): c.universe.TermName = {
    freshTermName(toFieldName(srcPrefixTree))
  }

  private def freshTermName(tpe: Type): c.universe.TermName = {
    freshTermName(tpe.typeSymbol.name.decodedName.toString.toLowerCase)
  }

  private def freshTermName(prefix: String): c.universe.TermName = {
    c.internal.reificationSupport.freshTermName(prefix.toLowerCase + "$")
  }

  private def toFieldName(srcPrefixTree: Tree): String = {
    // undo the encoding of freshTermName
    srcPrefixTree
      .toString()
      .replaceAll("\\$\\d+", "")
      .replaceAllLiterally("$u002E", ".")
  }

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
