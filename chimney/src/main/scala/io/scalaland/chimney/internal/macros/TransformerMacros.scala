package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.{DerivationGuards, EitherUtils, MacroUtils}

import scala.reflect.macros.blackbox

trait TransformerMacros extends TransformerConfiguration {
  this: DerivationGuards with MacroUtils with EitherUtils =>

  val c: blackbox.Context

  import c.universe._

  def buildDefinedTransformer[F[_]: WTTF, From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val config = captureTransformerConfig(C).copy(
      definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
    )

    if (!config.valueLevelAccessNeeded) {
      genTransformer[F, From, To](config).tree
    } else {
      val tdName = TermName(c.freshName("td"))
      val derivedTransformer = genTransformer[F, From, To](config.copy(transformerDefinitionPrefix = q"$tdName")).tree

      q"""
        val $tdName = ${c.prefix.tree}
        $derivedTransformer
      """
    }
  }

  def expandTransform[F[_]: WTTF, From: c.WeakTypeTag, To: c.WeakTypeTag, C: c.WeakTypeTag]: c.Tree = {
    val C = weakTypeOf[C]
    val tiName = TermName(c.freshName("ti"))
    val config = captureTransformerConfig(C).copy(transformerDefinitionPrefix = q"$tiName.td")

    val derivedTransformerTree = genTransformer[F, From, To](config).tree

    q"""
       val $tiName = ${c.prefix.tree}
       $derivedTransformerTree.transform($tiName.source)
    """
  }

  def genTransformer[F[_]: WTTF, From: c.WeakTypeTag, To: c.WeakTypeTag](
      config: TransformerConfig
  ): c.Expr[io.scalaland.chimney.TransformerF[F, From, To]] = {

    val F = WTTF[F]
    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName =
      c.internal.reificationSupport.freshTermName(From.typeSymbol.name.decodedName.toString.toLowerCase + "$")
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    expandTransformerTree(srcPrefixTree, config)(From, To, F) match {

      case Right(transformerTree) =>
        val tree = q"""
           new _root_.io.scalaland.chimney.TransformerF[$F, $From, $To] {
             def transform($srcName: $From): ${FTo(F, To)} = {
               $transformerTree
             }
           }
        """

        c.Expr[io.scalaland.chimney.TransformerF[F, From, To]](tree)

      case Left(derivationErrors) =>
        val errorMessage =
          s"""Chimney can't derive transformation from $From to $To in $F
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
  )(
      From: Type,
      To: Type,
      F: Type
  ): Either[Seq[DerivationError], Tree] = {

    val localInstance: Option[Tree] =
      if (config.definitionScope.contains((From, To))) {
        None
      } else {
        findLocalImplicitTransformerF(F, From, To)
      }

    val ctx = q"_root_.io.scalaland.chimney.TransformationContext[$F]"

    lazy val traverse = findLocalImplicitTraverse(From)

    localInstance
      .map { localImplicitTree =>
        Right(q"$localImplicitTree.transform($srcPrefixTree)")
      }
      .getOrElse {
        if (isSubtype(From, To)) {
          Right(q"$ctx.pure[$To]($srcPrefixTree)")
        } else if (fromValueClassToType(From, To)) {
          expandValueClassToType(srcPrefixTree, ctx)(From, To)
        } else if (fromTypeToValueClass(From, To)) {
          expandTypeToValueClass(srcPrefixTree, ctx)(From, To)
        } else if (bothOptions(From, To)) {
          expandOptions(srcPrefixTree, config, ctx)(From, To, F)
        } else if (isOption(To)) {
          expandTargetWrappedInOption(srcPrefixTree, config, ctx)(From, To, F)
        } else if (isOption(From)) {
          expandSourceWrappedInOption(srcPrefixTree, config, ctx)(From, To, F)
        } else if (bothEithers(From, To) && isId(F)) {
          expandEithers(srcPrefixTree, config)(From, To)
        } else if (bothMaps(From, To)) {
          expandMaps(srcPrefixTree, config, ctx)(From, To, F)
        } else if (bothOfTraversableOrArray(From, To) && isId(F)) {
          expandTraversableOrArray(srcPrefixTree, config)(From, To)
        } else if (From.typeConstructor =:= To.typeConstructor && traverse.isDefined) {
          expandTraverse(srcPrefixTree, config, ctx, traverse.get)(From, To, F)
        } else if (isTuple(To)) {
          expandDestinationTuple(srcPrefixTree, config, ctx)(From, To, F)
        } else if (destinationCaseClass(To)) {
          expandDestinationCaseClass(srcPrefixTree, config, ctx)(From, To, F)
        } else if (config.enableBeanSetters && destinationJavaBean(To) && isId(F)) {
          expandDestinationJavaBean(srcPrefixTree, config, ctx)(From, To)
        } else if (bothSealedClasses(From, To)) {
          expandSealedClasses(srcPrefixTree, config, ctx)(From, To, F)
        } else {
          notSupportedDerivation(srcPrefixTree, From, To, F)
        }
      }
  }

  def expandTraverse(srcPrefixTree: Tree, config: TransformerConfig, ctx: Tree, traverse: Tree)(
      From: Type,
      To: Type,
      F: Type
  ): Either[Seq[DerivationError], Tree] = {
    val FromCollectionT = From.typeArgs.head
    val ToCollectionT = To.typeArgs.head

    val fn = Ident(c.internal.reificationSupport.freshTermName("x$"))

    expandTransformerTree(fn, config.rec)(FromCollectionT, ToCollectionT, F).mapRight { inner =>
      q"$traverse.traverseWithPrefix[$F, $FromCollectionT, $ToCollectionT]($srcPrefixTree)(($fn: $FromCollectionT) => $inner)"
    }
  }

  def expandValueClassToType(
      srcPrefixTree: Tree,
      ctx: Tree
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    From.valueClassMember
      .map { member =>
        Right {
          q"$ctx.pure[$To]($srcPrefixTree.${member.name})"
        }
      }
      .getOrElse {
        Left {
          Seq(CantFindValueClassMember(From.typeSymbol.name.toString, To.typeSymbol.name.toString))
        }
      }
  }

  def expandTypeToValueClass(
      srcPrefixTree: Tree,
      ctx: Tree
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    Right(q"$ctx.pure[$To](new $To($srcPrefixTree))")
  }

  def expandTargetWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {
    def toInnerT = To.typeArgs.head
    if (To <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To, F)
    } else if (toInnerT =:= From) {
      Right(q"$ctx.pure[$To](_root_.scala.Option[$From]($srcPrefixTree))")
    } else {
      expandTransformerTree(q"$srcPrefixTree", config.rec)(From, toInnerT, F).mapRight { innerTransformer =>
        q"$ctx.map($innerTransformer)(_root_.scala.Option[$toInnerT](_))"
      }
    }
  }

  def expandSourceWrappedInOption(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {
    if (From <:< noneTpe || (isId(F) && !config.enableUnsafeOption)) {
      notSupportedDerivation(srcPrefixTree, From, To, F)
    } else {
      val fromInnerT = From.typeArgs.head
      val fn = freshTermName(srcPrefixTree)
      expandTransformerTree(Ident(fn), config.rec)(fromInnerT, To, F).mapRight { innerTransformer =>
        q"""$srcPrefixTree.fold($ctx.error[$To]("Required field expected. Got None"))(($fn: $fromInnerT) => $innerTransformer)"""
      }
    }
  }

  def expandOptions(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {

    val fromInnerT = From.typeArgs.head
    val toInnerT = To.typeArgs.head

    if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
      notSupportedDerivation(srcPrefixTree, From, To, F)
    } else if (From <:< someTpe && To <:< someTpe) {
      expandTransformerTree(q"$srcPrefixTree.get", config.rec)(fromInnerT, toInnerT, F).mapRight { innerTransformer =>
        q"$ctx.pure[$To](new _root_.scala.Some[$toInnerT]($innerTransformer))"
      }
    } else {
      val fn = freshTermName(srcPrefixTree)
      expandTransformerTree(Ident(fn), config.rec)(fromInnerT, toInnerT, F).mapRight { innerTransformer =>
        q"""$srcPrefixTree.fold($ctx.pure[$To](_root_.scala.Option.empty[$toInnerT]))(
             ($fn: $fromInnerT) => $ctx.map[$toInnerT, $To]($innerTransformer)(_root_.scala.Some[$toInnerT](_)))"""
      }
    }
  }

  // only for idF
  def expandEithers(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromLeftT, fromRightT) = From.typeArgs
    val List(toLeftT, toRightT) = To.typeArgs

    val fnL = c.internal.reificationSupport.freshTermName("left$")
    val fnR = c.internal.reificationSupport.freshTermName("right$")

    if (From <:< leftTpe && !(To <:< someTpe)) {
      val prefixTree = if (scala.util.Properties.versionNumberString >= "2.12") {
        q"$srcPrefixTree.value"
      } else {
        q"$srcPrefixTree.left.get"
      }
      expandTransformerTree(prefixTree, config.rec)(fromLeftT, toLeftT, idF).mapRight { leftTransformer =>
        q"new _root_.scala.util.Left($leftTransformer)"
      }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      val prefixTree = if (scala.util.Properties.versionNumberString >= "2.12") {
        q"$srcPrefixTree.value"
      } else {
        q"$srcPrefixTree.right.get"
      }
      expandTransformerTree(prefixTree, config.rec)(fromRightT, toRightT, idF).mapRight { rightTransformer =>
        q"new _root_.scala.util.Right($rightTransformer)"
      }
    } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
      val leftTransformerE = expandTransformerTree(Ident(fnL), config.rec)(fromLeftT, toLeftT, idF)
      val rightTransformerE = expandTransformerTree(Ident(fnR), config.rec)(fromRightT, toRightT, idF)

      (leftTransformerE, rightTransformerE) match {
        case (Right(leftTransformer), Right(rightTransformer)) =>
          Right {
            q"""
            $srcPrefixTree match {
              case _root_.scala.util.Left($fnL) =>
                new _root_.scala.util.Left($leftTransformer)
              case _root_.scala.util.Right($fnR) =>
                new _root_.scala.util.Right($rightTransformer)
            }
          """
          }
        case _ =>
          Left(leftTransformerE.left.getOrElse(Nil) ++ rightTransformerE.left.getOrElse(Nil))
      }
    } else {
      notSupportedDerivation(srcPrefixTree, From, To, idF)
    }
  }

  def expandMaps(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromKeyT, fromValueT) = From.typeArgs
    val List(toKeyT, toValueT) = To.typeArgs

    val fnK = c.internal.reificationSupport.freshTermName("k$")
    val fnV = c.internal.reificationSupport.freshTermName("v$")

    val keyTransformerE = expandTransformerTree(Ident(fnK), config.rec)(fromKeyT, toKeyT, F)
    val valueTransformerE = expandTransformerTree(Ident(fnV), config.rec)(fromValueT, toValueT, F)

    (keyTransformerE, valueTransformerE) match {
      case (Right(keyTransformer), Right(valueTransformer)) =>
        if (isId(F)) {
          Right {
            q"""
          $srcPrefixTree.map { case ($fnK: $fromKeyT, $fnV: $fromValueT) =>
            ($keyTransformer, $valueTransformer)
          }
         """
          }
        } else {
          findLocalImplicitTraverse(weakTypeOf[List[Unit]]) match {
            case Some(listTraverse) =>
              Right(q"""$ctx.map(
                  $listTraverse.sequence(
                    $srcPrefixTree.toList.map { case ($fnK: $fromKeyT, $fnV: $fromValueT) =>
                      $ctx.product(
                        $ctx.addPrefix($keyTransformer, "keys"),
                        $ctx.addPrefix($valueTransformer, $fnK.toString)
                      )
                    }
                  )
                )(_.toMap)""")
            case _ =>
              Left(Seq(CantFindTraverseList(From.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString)))
          }
        }
      case _ =>
        Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
    }
  }

  // only for idF
  def expandTraversableOrArray(
      srcPrefixTree: Tree,
      config: TransformerConfig
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val FromCollectionT = From.typeArgs.head
    val ToCollectionT = To.typeArgs.head

    val fn = Ident(freshTermName(srcPrefixTree))

    expandTransformerTree(fn, config.rec)(FromCollectionT, ToCollectionT, idF).mapRight { innerTransformerTree =>
      val sameCollectionTypes = From.typeConstructor =:= To.typeConstructor

      if (fn == innerTransformerTree) {
        if (sameCollectionTypes) {
          srcPrefixTree
        } else if (scala.util.Properties.versionNumberString >= "2.13") {
          val ToCompanionRef = patchedCompanionRef(c)(To)
          q"$srcPrefixTree.to[$To]($ToCompanionRef)"
        } else {
          q"$srcPrefixTree.to[${To.typeConstructor}]"
        }
      } else {
        val f = q"($fn: $FromCollectionT) => $innerTransformerTree"
        if (sameCollectionTypes) {
          q"$srcPrefixTree.map($f)"
        } else if (scala.util.Properties.versionNumberString >= "2.13") {
          val ToCompanionRef = patchedCompanionRef(c)(To)
          q"$srcPrefixTree.iterator.map[$ToCollectionT]($f).to[$To]($ToCompanionRef)"
        } else {
          q"$srcPrefixTree.iterator.map[$ToCollectionT]($f).to[${To.typeConstructor}]"
        }
      }
    }
  }

  def expandSealedClasses(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {

    if (config.coproductInstances.contains((From.typeSymbol, To))) {

      val instFullName = From.typeSymbol.fullName.toString
      val fullTargetName = To.typeSymbol.fullName.toString

      Right {
        q"""$ctx.pure[$To](
            ${config.transformerDefinitionPrefix}
              .instances(($instFullName, $fullTargetName))
              .asInstanceOf[Any => $To]
              .apply($srcPrefixTree)
              .asInstanceOf[$To]
            )
        """
      }
    } else {

      val fromCS = From.typeSymbol.classSymbolOpt.get
      val toCS = To.typeSymbol.classSymbolOpt.get

      val fromInstances = fromCS.knownDirectSubclasses
      val toInstances = toCS.knownDirectSubclasses

      val targetNamedInstances = toInstances.map(s => s.name.toString -> s).toMap

      val instanceClauses = fromInstances.toSeq.map { instSymbol =>
        instSymbol.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
        val instName = instSymbol.name.toString
        targetNamedInstances.get(instName) match {
          case Some(matchingTargetSymbol) =>
            if ((instSymbol.isModuleClass || instSymbol.isCaseClass) && matchingTargetSymbol.isModuleClass) {
              Right(cq"_: ${instSymbol.asType} => ${matchingTargetSymbol.asClass.module}")
            } else if (instSymbol.isCaseClass && matchingTargetSymbol.isCaseClass) {

              val fn = c.internal.reificationSupport.freshTermName(instName.toLowerCase + "$")
              val instTpe = instSymbol.asType.toType
              val targetTpe = matchingTargetSymbol.asType.toType

              expandDestinationCaseClass(Ident(fn), config.rec, ctx)(instTpe, targetTpe, F).mapRight {
                innerTransformerTree =>
                  cq"$fn: ${instSymbol.asType} => $innerTransformerTree"
              }
            } else {
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
          case None if config.coproductInstances.contains((instSymbol, To)) =>
            val fn = c.internal.reificationSupport.freshTermName(instName.toLowerCase + "$")
            val instFullName = instSymbol.fullName.toString
            val fullTargetName = To.typeSymbol.fullName.toString
            Right(
              cq"$fn: ${instSymbol.asType} => $ctx.pure[$To](${config.transformerDefinitionPrefix}.instances(($instFullName, $fullTargetName)).asInstanceOf[Any => $To]($fn).asInstanceOf[$To])"
            )

          case None =>
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

  def expandDestinationTuple(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {

    var errors = Seq.empty[DerivationError]

    val fromGetters = From.caseClassParams
    val toFields = To.caseClassParams

    val mapping = if (fromGetters.size != toFields.size) {
      errors :+= IncompatibleSourceTuple(
        fromGetters.size,
        toFields.size,
        From.typeSymbol.fullName.toString,
        To.typeSymbol.fullName.toString
      )
      Iterable.empty
    } else {
      (fromGetters zip toFields).map {
        case (sourceField, targetField) =>
          Target.fromField(targetField, To) -> Option(MatchingSourceAccessor(sourceField))
      }
    }

    val (resolutionErrors, args) = resolveTargetArgTrees(srcPrefixTree, config, ctx, F, From, To)(mapping)

    errors ++= resolutionErrors

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      Right(mkCaseClassF(F, ctx, To, args.toList, mapping.map(_._1.tpe).toList))
    }
  }

  def expandDestinationCaseClass(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type, F: Type): Either[Seq[DerivationError], Tree] = {

    var errors = Seq.empty[DerivationError]

    val toFields = To.caseClassParams

    val mapping = if (isTuple(From)) {
      val fromGetters = From.caseClassParams
      if (fromGetters.size != toFields.size) {
        errors :+= IncompatibleSourceTuple(
          fromGetters.size,
          toFields.size,
          From.typeSymbol.fullName.toString,
          To.typeSymbol.fullName.toString
        )
        Iterable.empty
      } else {
        (fromGetters zip toFields).map {
          case (sourceField, targetField) =>
            Target.fromField(targetField, To) -> Some(MatchingSourceAccessor(sourceField))
        }
      }
    } else {
      val fromGetters = From.getterMethods
      toFields.map { targetField =>
        val target = Target.fromField(targetField, To)
        target -> resolveTarget(srcPrefixTree, ctx, config, From, To, F)(
          target,
          fromGetters,
          Some(To.typeSymbol.asClass)
        )
      }
    }

    val missingTargets = mapping.collect { case (target, None) => target }

    missingTargets.foreach { target =>
      errors :+= MissingField(
        fieldName = target.name.toString,
        fieldTypeName = target.tpe.typeSymbol.fullName,
        sourceTypeName = From.typeSymbol.fullName,
        targetTypeName = To.typeSymbol.fullName
      )
    }

    val (resolutionErrors, args) = resolveTargetArgTrees(srcPrefixTree, config, ctx, F, From, To)(mapping)

    errors ++= resolutionErrors

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      Right(mkCaseClassF(F, ctx, To, args.toList, mapping.map(_._1.tpe).toList))
    }
  }

  def mkCaseClassF(F: Type, ctx: Tree, To: Type, args: List[Tree], targetTypes: List[Type]): Tree = {
    if (isId(F))
      q"new $To(..$args)"
    else if (args.size == 1) {
      q"$ctx.map[${targetTypes.head}, $To](${args.head})(new $To(_))"
    } else {
      val (productTree, productTpe) = args.tail.zip(targetTypes.tail).foldLeft((args.head, tq"${targetTypes.head}")) {
        case ((accTree, accTpe), (nextTree, nextTpe)) =>
          (q"$ctx.product[$accTpe, $nextTpe]($accTree, $nextTree)", tq"($accTpe, $nextTpe)")
      }
      val argTerms = (1 to args.size).map(idx => c.internal.reificationSupport.freshTermName("arg$" + idx)).toList
      val emptyIdent = Ident(TermName("_"))
      val tuple = argTerms.tail.foldLeft[Tree](Bind(argTerms.head, emptyIdent))((acc, current) =>
        q"($acc, ${Bind(current, emptyIdent)})"
      )
      q"$ctx.map[$productTpe, $To]($productTree) { case $tuple => new $To(..$argTerms) }"
    }
  }

  // only for idF
  def expandDestinationJavaBean(
      srcPrefixTree: Tree,
      config: TransformerConfig,
      ctx: Tree
  )(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
    var errors = Seq.empty[DerivationError]

    val fromGetters = From.getterMethods
    val beanSetters = To.beanSetterMethods

    val mapping = beanSetters.map { beanSetter =>
      val target = Target.fromJavaBeanSetter(beanSetter, To)
      target -> resolveTarget(srcPrefixTree, ctx, config, From, To, idF)(target, fromGetters, None)
    }

    val missingTargets = mapping.collect { case (target, None) => target }

    missingTargets.foreach { target =>
      errors :+= MissingJavaBeanSetterParam(
        setterName = target.name,
        requiredTypeName = target.tpe.typeSymbol.fullName,
        sourceTypeName = From.typeSymbol.fullName,
        targetTypeName = To.typeSymbol.fullName
      )
    }

    val (resolutionErrors, args) = resolveTargetArgTrees(srcPrefixTree, config, ctx, idF, From, To)(mapping)

    errors ++= resolutionErrors

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      val fn = TermName(c.freshName(To.typeSymbol.name.decodedName.toString.toLowerCase))

      val objCreation = q"val $fn = new $To"
      val setterInvocations = (mapping.map(_._1) zip args).map {
        case (target, argTree) =>
          val setterName = TermName("set" + target.name.capitalize)
          q"$fn.$setterName($argTree)"
      }.toSeq

      Right {
        q"{..${objCreation +: setterInvocations}; $fn}"
      }
    }
  }

  def resolveTargetArgTrees(srcPrefixTree: Tree, config: TransformerConfig, ctx: Tree, F: Type, From: Type, To: Type)(
      mapping: Iterable[(Target, Option[TargetResolution])]
  ): (Seq[DerivationError], Iterable[Tree]) = {

    var errors = Seq.empty[DerivationError]

    val args = mapping.collect {
      case (_, Some(ResolvedTargetTree(tree))) =>
        tree

      case (target, Some(MatchingSourceAccessor(sourceField))) =>
        val prefix = sourceField.canonicalName

        findLocalImplicitTransformerF(F, sourceField.resultTypeIn(From), target.tpe) match {
          case Some(localImplicitTransformer) =>
            q"$ctx.addPrefix($localImplicitTransformer.transform($srcPrefixTree.${sourceField.name}), $prefix)"

          case None if canTryDeriveTransformer(sourceField.resultTypeIn(From), target.tpe) =>
            expandTransformerTree(q"$srcPrefixTree.${sourceField.name}", config.rec)(
              sourceField.resultTypeIn(From),
              target.tpe,
              F
            ) match {
              case Left(errs) =>
                errors ++= errs
                EmptyTree
              case Right(tree) =>
                q"$ctx.addPrefix($tree, $prefix)"
            }

          case None =>
            errors :+= MissingTransformer(
              fieldName = target.name,
              sourceFieldTypeName = sourceField.resultTypeIn(From).typeSymbol.fullName,
              targetFieldTypeName = target.tpe.typeSymbol.fullName,
              sourceTypeName = From.typeSymbol.fullName,
              targetTypeName = To.typeSymbol.fullName
            )

            EmptyTree
        }
    }

    (errors, args)
  }

  def resolveTarget(srcPrefixTree: Tree, ctx: Tree, config: TransformerConfig, From: Type, To: Type, F: Type)(
      target: Target,
      fromGetters: Iterable[MethodSymbol],
      targetCaseClass: Option[ClassSymbol]
  ): Option[TargetResolution] = {

    if (config.constFields.contains(target.name)) {
      Some {
        ResolvedTargetTree {
          q"""
             $ctx.addPrefix(
              ${config.transformerDefinitionPrefix}
                .overrides(${target.name})
                .asInstanceOf[${FTo(F, target.tpe)}],
                ${target.name}
              )
          """
        }
      }
    } else if (config.computedFields.contains(target.name)) {
      Some {
        ResolvedTargetTree {
          q"""
             $ctx.addPrefix(
                ${config.transformerDefinitionPrefix}
                .overrides(${target.name})
                .asInstanceOf[$From => ${FTo(F, target.tpe)}]
                .apply($srcPrefixTree),
                ${target.name}
             )
          """
        }
      }
    } else if (config.renamedFields.contains(target.name)) {
      val fromFieldName = TermName(config.renamedFields(target.name))
      fromGetters.find(_.name.decodedName == fromFieldName.decodedName).map { ms =>
        if (target.tpe <:< ms.resultTypeIn(From)) {
          ResolvedTargetTree {
            q"$ctx.pure[${target.tpe}]($srcPrefixTree.${fromFieldName.encodedName.toTermName})"
          }
        } else {
          MatchingSourceAccessor(ms)
        }
      }
    } else {
      fromGetters
        .find(lookupAccessor(config, target, From))
        .map { ms =>
          if (ms.resultTypeIn(From) <:< target.tpe) {
            ResolvedTargetTree {
              q"$ctx.pure[${target.tpe}]($srcPrefixTree.${ms.name})"
            }
          } else {
            MatchingSourceAccessor(ms)
          }
        }
        .orElse {
          if (config.processDefaultValues && targetCaseClass.isDefined) {
            val targetDefault = targetCaseClass.get.caseClassDefaults.get(target.name)
            if (targetDefault.isDefined) {
              Some(ResolvedTargetTree(q"$ctx.pure[${target.tpe}](${targetDefault.get})"))
            } else {
              None
            }
          } else {
            None
          }
        }
        .orElse {
          val targetTypeIsOption = target.tpe <:< typeOf[Option[_]]
          if (targetTypeIsOption && config.optionDefaultsToNone) {
            Some(ResolvedTargetTree(q"$ctx.pure[${target.tpe}](_root_.scala.None)"))
          } else {
            None
          }
        }
        .orElse {
          if (target.tpe <:< typeOf[Unit]) {
            Some(ResolvedTargetTree(q"$ctx.pure[Unit](())"))
          } else {
            None
          }
        }
    }
  }

  def lookupAccessor(config: TransformerConfig, target: Target, From: Type)(ms: MethodSymbol): Boolean = {
    val sourceName = ms.name.decodedName.toString
    if (config.enableBeanGetters) {
      val targetNameCapitalized = target.name.capitalize
      sourceName == target.name ||
      sourceName == s"get$targetNameCapitalized" ||
      (sourceName == s"is$targetNameCapitalized" && ms.resultTypeIn(From) == typeOf[Boolean])
    } else {
      sourceName == target.name
    }
  }

  def findLocalImplicitTransformerF(F: Type, From: Type, To: Type): Option[Tree] = {
    findLocalImplicit(tq"_root_.io.scalaland.chimney.TransformerF[$F, $From, $To]")
  }

  def findLocalImplicitTraverse(G: Type): Option[Tree] = {
    findLocalImplicit(tq"_root_.io.scalaland.chimney.typeclasses.Traverse[${G.typeConstructor}]")
  }

  def findLocalImplicit(tree: Tree): Option[Tree] = {
    val tpeTree = c.typecheck(
      tree = tree,
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
      toTpe: Type,
      F: Type
  ): Left[Seq[NotSupportedDerivation], Nothing] =
    Left {
      Seq(
        NotSupportedDerivation(
          toFieldName(srcPrefixTree),
          fromTpe.typeSymbol.fullName.toString,
          toTpe.typeSymbol.fullName.toString,
          F.typeSymbol.fullName.toString
        )
      )
    }

  private def freshTermName(srcPrefixTree: Tree): c.universe.TermName = {
    c.internal.reificationSupport.freshTermName(toFieldName(srcPrefixTree) + "$")
  }

  private def toFieldName(srcPrefixTree: Tree): String = {
    // undo the encoding of freshTermName
    srcPrefixTree
      .toString()
      .replaceAll("\\$\\d+", "")
      .replaceAllLiterally("$u002E", ".")
  }

  case class Target(name: String, tpe: Type, kind: Target.Kind)
  object Target {
    sealed trait Kind
    case object ClassField extends Kind
    case object JavaBeanSetter extends Kind

    def fromJavaBeanSetter(ms: MethodSymbol, site: Type): Target =
      Target(ms.canonicalName, ms.beanSetterParamTypeIn(site), JavaBeanSetter)

    def fromField(ms: MethodSymbol, site: Type): Target =
      Target(ms.canonicalName, ms.resultTypeIn(site), ClassField)
  }

  sealed trait TargetResolution
  case class ResolvedTargetTree(tree: Tree) extends TargetResolution
  case class MatchingSourceAccessor(ms: MethodSymbol) extends TargetResolution

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"

  protected val idF = weakTypeOf[io.scalaland.chimney.Id[Unit]].typeConstructor

  protected def isId(F: Type): Boolean =
    idF =:= F

  private def FTo(F: Type, To: Type): Type = {
    val FA = F.etaExpand.finalResultType
    FA.substituteTypes(FA.typeArgs.map(_.typeSymbol), List(To))
  }
}
