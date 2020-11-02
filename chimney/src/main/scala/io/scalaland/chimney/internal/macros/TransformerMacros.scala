package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}

import scala.reflect.macros.blackbox

trait TransformerMacros extends TransformerConfigSupport with MappingMacros with TargetConstructorMacros {
  this: DerivationGuards with MacroUtils with EitherUtils =>

  val c: blackbox.Context

  import c.universe._

  def buildDefinedTransformer[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](
      tfsTree: Tree = EmptyTree,
      wrapperType: Option[Type] = None
  ): Tree = {
    val config = readConfig[C, InstanceFlags, ScopeFlags](tfsTree).copy(
      definitionScope = Some((weakTypeOf[From], weakTypeOf[To])),
      wrapperErrorPathSupportInstance = findTransformerErrorPathSupport(wrapperType)
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

  def expandTransform[
      From: WeakTypeTag,
      To: WeakTypeTag,
      C: WeakTypeTag,
      InstanceFlags: WeakTypeTag,
      ScopeFlags: WeakTypeTag
  ](
      tcTree: Tree,
      tfsTree: Tree = EmptyTree,
      wrapperType: Option[Type] = None
  ): Tree = {
    val tiName = TermName(c.freshName("ti"))

    val config = readConfig[C, InstanceFlags, ScopeFlags](tfsTree).copy(
      transformerDefinitionPrefix = q"$tiName.td",
      wrapperErrorPathSupportInstance = findTransformerErrorPathSupport(wrapperType)
    )

    val derivedTransformerTree = genTransformer[From, To](config)

    q"""
       val _ = $tcTree // hack to avoid unused warnings
       val $tiName = ${c.prefix.tree}
       ${derivedTransformerTree.callTransform(q"$tiName.source")}
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

  def genTransformerTree(
      srcName: TermName,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    resolveTransformerBody(srcPrefixTree, config)(From, To).map {
      case TransformerBodyTree(tree, false) if config.wrapperType.isDefined =>
        q"${config.wrapperSupportInstance}.pure[$To]($tree)"
      case TransformerBodyTree(tree, _) => tree
    }
  }

  def expandTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    resolveImplicitTransformer(config)(From, To)
      .map(localImplicitTree => Right(localImplicitTree.callTransform(srcPrefixTree)))
      .getOrElse {
        deriveTransformerTree(srcPrefixTree, config)(From, To)
      }
  }

  def deriveTransformerTree(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
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
  ): Either[Seq[DerivationError], Tree] = {
    Right {
      mkTransformerBodyTree0(config)(srcPrefixTree)
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
          mkTransformerBodyTree0(config) {
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
      mkTransformerBodyTree0(config) {
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
        .map { innerTransformerBody =>
          val fn = freshTermName(innerSrcPrefix).toString
          mkTransformerBodyTree1(To, Target(fn, To), innerTransformerBody, config) { tree =>
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
        .map {
          case TransformerBodyTree(innerTree, false) =>
            mkTransformerBodyTree0(config) {
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
      resolveRecursiveTransformerBody(q"$srcPrefixTree.value", config.rec)(fromLeftT, toLeftT)
        .map { tbt =>
          mkTransformerBodyTree1(To, Target(fnL.name.toString, toLeftT), tbt, config) { leftArgTree =>
            q"new _root_.scala.util.Left($leftArgTree)"
          }
        }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      resolveRecursiveTransformerBody(q"$srcPrefixTree.value", config.rec)(fromRightT, toRightT)
        .map { tbt =>
          mkTransformerBodyTree1(To, Target(fnR.name.toString, toRightT), tbt, config) { rightArgTree =>
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

          val leftBody = mkTransformerBodyTree1(To, Target(leftN.toString, toLeftT), leftTbt, config) { leftArgTree =>
            q"new _root_.scala.util.Left($leftArgTree)"
          }

          val rightBody = mkTransformerBodyTree1(To, Target(rightN.toString, toRightT), rightTbt, config) {
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
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
    val ToInnerT = To.collectionInnerTpe

    (config.wrapperErrorPathSupportInstance, config.wrapperType, ToInnerT.caseClassParams.map(_.resultTypeIn(ToInnerT))) match {
      case (Some(errorPathSupport), Some(f), List(toKeyT, toValueT)) =>
        val List(fromKeyT, fromValueT) = From.typeArgs

        val fnK = Ident(freshTermName("k"))
        val fnV = Ident(freshTermName("v"))

        val keyTransformerE = resolveRecursiveTransformerBody(fnK, config.rec)(fromKeyT, toKeyT)
        val valueTransformerE = resolveRecursiveTransformerBody(fnV, config.rec)(fromValueT, toValueT)

        (keyTransformerE, valueTransformerE) match {
          case (Right(keyTransformer), Right(valueTransformer)) =>
            val wrapper = config.wrapperSupportInstance
            val WrappedToInnerT = f.applyTypeArg(ToInnerT)

            val keyTransformerWithPath =
              if (keyTransformer.isWrapped)
                q"""$errorPathSupport.addPath[$toKeyT](
                   ${keyTransformer.tree},
                   _root_.io.scalaland.chimney.ErrorPathNode.MapKey($fnK)
                 )"""
              else q"$wrapper.pure[$toKeyT](${keyTransformer.tree})"

            val valueTransformerWithPath =
              if (valueTransformer.isWrapped)
                q"""$errorPathSupport.addPath[$toValueT](
                    ${valueTransformer.tree},
                    _root_.io.scalaland.chimney.ErrorPathNode.MapValue($fnK)
                 )"""
              else q"$wrapper.pure[$toValueT](${valueTransformer.tree})"

            Right(
              q"""$wrapper.traverse[$To, $WrappedToInnerT, $ToInnerT](
                  $srcPrefixTree.iterator.map[$WrappedToInnerT] {
                    case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
                      $wrapper.product[$toKeyT, $toValueT](
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

      case _ => expandIterableOrArray(srcPrefixTree, config)(From, To)
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
      .map {
        case TransformerBodyTree(innerTransformerTree, true) =>
          if (config.wrapperType.isDefined) {
            config.wrapperErrorPathSupportInstance match {
              case Some(errorPathSupport) =>
                val idx = Ident(freshTermName("idx"))

                q"""${config.wrapperSupportInstance}.traverse[$To, ($FromInnerT, _root_.scala.Int), $ToInnerT](
                  $srcPrefixTree.iterator.zipWithIndex,
                  { case (${fn.name}: $FromInnerT, ${idx.name}: _root_.scala.Int) =>
                    $errorPathSupport.addPath[$ToInnerT](
                      $innerTransformerTree,
                      _root_.io.scalaland.chimney.ErrorPathNode.Index($idx)
                    )
                  }
                )
              """
              case None =>
                q"""${config.wrapperSupportInstance}.traverse[$To, $FromInnerT, $ToInnerT](
                  $srcPrefixTree.iterator,
                  ($fn: $FromInnerT) => $innerTransformerTree
                )
              """
            }

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
                  val tree = mkTransformerBodyTree0(config) {
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
        mkTransformerBodyTree0(config) {
          mkCoproductInstance(
            config.transformerDefinitionPrefix,
            srcPrefixTree,
            From.typeSymbol,
            To,
            None
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
      .flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
      }
      .map { transformerBodyPerTarget =>
        val targets = To.caseClassParams.map(Target.fromField(_, To))
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))

        mkTransformerBodyTree(To, targets, bodyTreeArgs, config) { args =>
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

      mkTransformerBodyTree(To, targets, bodyTreeArgs, config) { args =>
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
      .map { transformerBodyPerTarget =>
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
        mkTransformerBodyTree(To, targets, bodyTreeArgs, config) { args =>
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
  ): Either[Seq[DerivationError], Map[Target, TransformerBodyTree]] = {

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
  ): Either[Seq[DerivationError], TransformerBodyTree] = {
    val resolved = resolveRecursiveTransformerBody(
      q"$srcPrefixTree.${accessor.symbol.name}",
      config
    )(
      accessor.symbol.resultTypeIn(From),
      target.tpe
    )

    (resolved, config.wrapperErrorPathSupportInstance) match {
      case (Right(bodyTree), Some(errorPathSupport)) if bodyTree.isWrapped =>
        Right {
          TransformerBodyTree(
            q"""$errorPathSupport.addPath[${target.tpe}](
                 ${bodyTree.tree},
                 _root_.io.scalaland.chimney.ErrorPathNode.Accessor(${accessor.symbol.name.toString})
               )""",
            isWrapped = true
          )
        }
      case _ => resolved
    }
  }

  def resolveRecursiveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerBodyTree] = {
    resolveTransformerBody(srcPrefixTree, config.rec)(From, To)
  }

  def resolveTransformerBody(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], TransformerBodyTree] = {
    if (config.wrapperType.isDefined) {
      val implicitTransformerF = resolveImplicitTransformer(config)(From, To)
      val implicitTransformer = findLocalImplicitTransformer(From, To, None)

      (implicitTransformerF, implicitTransformer) match {
        case (Some(localImplicitTreeF), Some(localImplicitTree)) =>
          c.abort(
            c.enclosingPosition,
            s"""Ambiguous implicits while resolving Chimney recursive transformation:
               |
               |TransformerF[${config.wrapperType.get}, $From, $To]: $localImplicitTreeF
               |Transformer[$From, $To]: $localImplicitTree
               |
               |Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used
               |""".stripMargin
          )
        case (Some(localImplicitTreeF), None) =>
          Right(TransformerBodyTree(localImplicitTreeF.callTransform(srcPrefixTree), isWrapped = true))
        case (None, Some(localImplicitTree)) =>
          Right(TransformerBodyTree(localImplicitTree.callTransform(srcPrefixTree), isWrapped = false))
        case (None, None) =>
          deriveTransformerTree(srcPrefixTree, config)(From, To)
            .map(tree => TransformerBodyTree(tree, isWrapped = true))
      }
    } else {
      expandTransformerTree(srcPrefixTree, config)(From, To)
        .map(tree => TransformerBodyTree(tree, isWrapped = false))
    }
  }

  def resolveImplicitTransformer(config: TransformerConfig)(From: Type, To: Type): Option[Tree] = {
    if (config.definitionScope.contains((From, To))) {
      None
    } else {
      findLocalImplicitTransformer(From, To, config.wrapperType)
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

  private def findLocalImplicitTransformer(From: Type, To: Type, wrapperType: Option[Type]): Option[Tree] = {
    val searchTypeTree: Tree = wrapperType match {
      case Some(f) =>
        tq"_root_.io.scalaland.chimney.TransformerF[$f, $From, $To]"
      case None =>
        tq"_root_.io.scalaland.chimney.Transformer[$From, $To]"
    }

    inferImplicitTpe(searchTypeTree, macrosDisabled = false).filterNot(isDeriving)
  }

  def findTransformerErrorPathSupport(wrapperType: Option[Type]): Option[Tree] = {
    wrapperType.flatMap(tpe =>
      inferImplicitTpe(tq"_root_.io.scalaland.chimney.TransformerFErrorPathSupport[$tpe]", macrosDisabled = true)
    )
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
        qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.Transformer.type] && name.toString == "derive"
      case Apply(TypeApply(Select(qualifier, name), _), _) =>
        qualifier.tpe =:= weakTypeOf[io.scalaland.chimney.TransformerF.type] && name.toString == "derive"
      case _ => false
    }
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
      .replace("$u002E", ".")
  }

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
