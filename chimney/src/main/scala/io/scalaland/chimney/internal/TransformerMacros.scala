package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait TransformerMacros {
  this: DerivationGuards with MacroUtils with DerivationConfig with EitherUtils =>

  val c: blackbox.Context

  import c.universe._

  def genTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag](
    config: Config
  ): c.Expr[io.scalaland.chimney.Transformer[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName =
      c.internal.reificationSupport.freshTermName(From.typeSymbol.name.decodedName.toString.toLowerCase + "$")
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    expandTransformerTree(srcPrefixTree, config)(From, To) match {

      case Right(transformerTree) =>
        val tree = q"""
           new _root_.io.scalaland.chimney.Transformer[$From, $To] {
             def transform($srcName: $From): $To = {
               $transformerTree
             }
           }
        """

        c.Expr[io.scalaland.chimney.Transformer[From, To]](tree)

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

  def expandTransformerTree(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                 To: Type): Either[Seq[DerivationError], Tree] = {

    findLocalImplicitTransformer(From, To)
      .map { localImplicitTree =>
        Right(q"$localImplicitTree.transform($srcPrefixTree)")
      }
      .getOrElse {
        if (isSubtype(From, To)) {
          Right(srcPrefixTree)
        } else if (fromValueClassToType(From, To)) {
          expandValueClassToType(srcPrefixTree)(From, To)
        } else if (fromTypeToValueClass(From, To)) {
          expandTypeToValueClass(srcPrefixTree)(From, To)
        } else if (bothOptions(From, To)) {
          expandOptions(srcPrefixTree, config)(From, To)
        } else if (isWrappedInOption(To)) {
          expandTargetWrappedInOption(srcPrefixTree, config)(From, To)
        } else if (config.enableUnsafeOption && isWrappedInOption(From)) {
          expandSourceWrappedInOption(srcPrefixTree, config)(From, To)
        } else if (bothEithers(From, To)) {
          expandEithers(srcPrefixTree, config)(From, To)
        } else if (bothMaps(From, To)) {
          expandMaps(srcPrefixTree, config)(From, To)
        } else if (bothOfTraversableOrArray(From, To)) {
          expandTraversableOrArray(srcPrefixTree, config)(From, To)
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

  def expandValueClassToType(srcPrefixTree: Tree)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    From.valueClassMember
      .map { member =>
        Right {
          q"$srcPrefixTree.${member.name}"
        }
      }
      .getOrElse {
        Left {
          Seq(CantFindValueClassMember(From.typeSymbol.name.toString, To.typeSymbol.name.toString))
        }
      }
  }

  def expandTypeToValueClass(srcPrefixTree: Tree)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    Right(q"new $To($srcPrefixTree)")
  }

  def expandTargetWrappedInOption(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                       To: Type): Either[Seq[DerivationError], Tree] = {
    def toInnerT = To.typeArgs.head
    if (To <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else if (toInnerT =:= From) {
      Right(q"_root_.scala.Option[$From]($srcPrefixTree)")
    } else {
      expandTransformerTree(q"$srcPrefixTree", config.rec)(From, toInnerT).mapRight { innerTransformer =>
        q"_root_.scala.Option[$toInnerT]($innerTransformer)"
      }
    }
  }

  def expandSourceWrappedInOption(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                       To: Type): Either[Seq[DerivationError], Tree] = {
    if (From <:< noneTpe) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else {
      val fromInnerT = From.typeArgs.head
      expandTransformerTree(q"$srcPrefixTree.get", config.rec)(fromInnerT, To).mapRight { innerTransformer =>
        q"($innerTransformer)"
      }
    }
  }

  def expandOptions(srcPrefixTree: Tree, config: Config)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val fromInnerT = From.typeArgs.head
    val toInnerT = To.typeArgs.head

    if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
      notSupportedDerivation(srcPrefixTree, From, To)
    } else if (From <:< someTpe && To <:< someTpe) {
      expandTransformerTree(q"$srcPrefixTree.get", config.rec)(fromInnerT, toInnerT).mapRight { innerTransformer =>
        q"new _root_.scala.Some[$toInnerT]($innerTransformer)"
      }
    } else {
      val fn = freshTermName(srcPrefixTree)
      expandTransformerTree(Ident(fn), config.rec)(fromInnerT, toInnerT).mapRight { innerTransformer =>
        q"$srcPrefixTree.map(($fn: $fromInnerT) => $innerTransformer)"
      }
    }
  }

  def expandEithers(srcPrefixTree: Tree, config: Config)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

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
      expandTransformerTree(prefixTree, config.rec)(fromLeftT, toLeftT).mapRight { leftTransformer =>
        q"new _root_.scala.util.Left($leftTransformer)"
      }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      val prefixTree = if (scala.util.Properties.versionNumberString >= "2.12") {
        q"$srcPrefixTree.value"
      } else {
        q"$srcPrefixTree.right.get"
      }
      expandTransformerTree(prefixTree, config.rec)(fromRightT, toRightT).mapRight { rightTransformer =>
        q"new _root_.scala.util.Right($rightTransformer)"
      }
    } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
      val leftTransformerE = expandTransformerTree(Ident(fnL), config.rec)(fromLeftT, toLeftT)
      val rightTransformerE = expandTransformerTree(Ident(fnR), config.rec)(fromRightT, toRightT)

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
      notSupportedDerivation(srcPrefixTree, From, To)
    }
  }

  def expandMaps(srcPrefixTree: Tree, config: Config)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromKeyT, fromValueT) = From.typeArgs
    val List(toKeyT, toValueT) = To.typeArgs

    val fnK = c.internal.reificationSupport.freshTermName("k$")
    val fnV = c.internal.reificationSupport.freshTermName("v$")

    val keyTransformerE = expandTransformerTree(Ident(fnK), config.rec)(fromKeyT, toKeyT)
    val valueTransformerE = expandTransformerTree(Ident(fnV), config.rec)(fromValueT, toValueT)

    (keyTransformerE, valueTransformerE) match {
      case (Right(keyTransformer), Right(valueTransformer)) =>
        Right {
          q"""
          $srcPrefixTree.map { case ($fnK: $fromKeyT, $fnV: $fromValueT) =>
            ($keyTransformer, $valueTransformer)
          }
         """
        }
      case _ =>
        Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
    }
  }

  def expandTraversableOrArray(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                    To: Type): Either[Seq[DerivationError], Tree] = {

    val FromCollectionT = From.typeArgs.head
    val ToCollectionT = To.typeArgs.head

    val fn = Ident(freshTermName(srcPrefixTree))

    expandTransformerTree(fn, config.rec)(FromCollectionT, ToCollectionT).mapRight { innerTransformerTree =>
      val sameCollectionTypes = From.typeConstructor =:= To.typeConstructor

      if (fn == innerTransformerTree) {
        if (sameCollectionTypes) {
          srcPrefixTree
        } else if (scala.util.Properties.versionNumberString >= "2.13") {
          val ToCompanionRef = patchedCompanionRef(c)(To)
          q"$srcPrefixTree.to($ToCompanionRef)"
        } else {
          q"$srcPrefixTree.to[${To.typeConstructor}]"
        }
      } else {
        val f = q"($fn: $FromCollectionT) => $innerTransformerTree"
        if (sameCollectionTypes) {
          q"$srcPrefixTree.map($f)"
        } else if (scala.util.Properties.versionNumberString >= "2.13") {
          val ToCompanionRef = patchedCompanionRef(c)(To)
          q"$srcPrefixTree.iterator.map($f).to($ToCompanionRef)"
        } else {
          q"$srcPrefixTree.iterator.map($f).to[${To.typeConstructor}]"
        }
      }
    }
  }

  def expandSealedClasses(srcPrefixTree: Tree, config: Config)(From: Type,
                                                               To: Type): Either[Seq[DerivationError], Tree] = {

    if (config.coproductInstances.contains((From.typeSymbol, To))) {

      val instFullName = From.typeSymbol.fullName.toString
      val fullTargetName = To.typeSymbol.fullName.toString

      Right {
        q"${TermName(config.prefixValName)}.instances(($instFullName, $fullTargetName)).asInstanceOf[Any => $To]($srcPrefixTree).asInstanceOf[$To]"
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

              expandDestinationCaseClass(Ident(fn), config.rec)(instTpe, targetTpe).mapRight { innerTransformerTree =>
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
              cq"$fn: ${instSymbol.asType} => ${TermName(config.prefixValName)}.instances(($instFullName, $fullTargetName)).asInstanceOf[Any => $To]($fn).asInstanceOf[$To]"
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

  def expandDestinationTuple(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                  To: Type): Either[Seq[DerivationError], Tree] = {

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

    val (resolutionErrors, args) = resolveTargetArgTrees(srcPrefixTree, config, From, To)(mapping)

    errors ++= resolutionErrors

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      Right(q"new $To(..$args)")
    }
  }

  def expandDestinationCaseClass(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                      To: Type): Either[Seq[DerivationError], Tree] = {

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
        target -> resolveTarget(srcPrefixTree, config, From, To)(target, fromGetters, Some(To.typeSymbol.asClass))
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

    val (resolutionErrors, args) = resolveTargetArgTrees(srcPrefixTree, config, From, To)(mapping)

    errors ++= resolutionErrors

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      Right(q"new $To(..$args)")
    }
  }

  def expandDestinationJavaBean(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                     To: Type): Either[Seq[DerivationError], Tree] = {
    var errors = Seq.empty[DerivationError]

    val fromGetters = From.getterMethods
    val beanSetters = To.beanSetterMethods

    val mapping = beanSetters.map { beanSetter =>
      val target = Target.fromJavaBeanSetter(beanSetter, To)
      target -> resolveTarget(srcPrefixTree, config, From, To)(target, fromGetters, None)
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

    val (resolutionErrors, args) = resolveTargetArgTrees(srcPrefixTree, config, From, To)(mapping)

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

  def resolveTargetArgTrees(srcPrefixTree: Tree, config: Config, From: Type, To: Type)(
    mapping: Iterable[(Target, Option[TargetResolution])]
  ): (Seq[DerivationError], Iterable[Tree]) = {

    var errors = Seq.empty[DerivationError]

    val args = mapping.collect {
      case (_, Some(ResolvedTargetTree(tree))) =>
        tree

      case (target, Some(MatchingSourceAccessor(sourceField))) =>
        findLocalImplicitTransformer(sourceField.resultTypeIn(From), target.tpe) match {
          case Some(localImplicitTransformer) =>
            q"$localImplicitTransformer.transform($srcPrefixTree.${sourceField.name})"

          case None if canTryDeriveTransformer(sourceField.resultTypeIn(From), target.tpe) =>
            expandTransformerTree(q"$srcPrefixTree.${sourceField.name}", config.rec)(
              sourceField.resultTypeIn(From),
              target.tpe
            ) match {
              case Left(errs) =>
                errors ++= errs
                EmptyTree
              case Right(tree) =>
                tree
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

  def resolveTarget(srcPrefixTree: Tree, config: Config, From: Type, To: Type)(
    target: Target,
    fromGetters: Iterable[MethodSymbol],
    targetCaseClass: Option[ClassSymbol]
  ): Option[TargetResolution] = {

    if (config.overridenFields.contains(target.name)) {
      Some {
        ResolvedTargetTree {
          q"${TermName(config.prefixValName)}.overrides(${target.name}).asInstanceOf[${target.tpe}]"
        }
      }
    } else if (config.renamedFields.contains(target.name)) {
      val fromFieldName = TermName(config.renamedFields(target.name))
      fromGetters.find(_.name.decodedName == fromFieldName.decodedName).map { ms =>
        if (target.tpe <:< ms.resultTypeIn(From)) {
          ResolvedTargetTree {
            q"$srcPrefixTree.${fromFieldName.encodedName.toTermName}"
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
              q"$srcPrefixTree.${ms.name}"
            }
          } else {
            MatchingSourceAccessor(ms)
          }
        }
        .orElse {
          if (config.processDefaultValues && targetCaseClass.isDefined) {
            val targetDefault = targetCaseClass.get.caseClassDefaults.get(target.name)
            if (targetDefault.isDefined) {
              Some(ResolvedTargetTree(targetDefault.get))
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
            Some(ResolvedTargetTree(q"_root_.scala.None"))
          } else {
            None
          }
        }
    }
  }

  def lookupAccessor(config: Config, target: Target, From: Type)(ms: MethodSymbol): Boolean = {
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

  def findLocalImplicitTransformer(From: Type, To: Type): Option[Tree] = {
    val tpeTree = c.typecheck(
      tree = tq"_root_.io.scalaland.chimney.Transformer[$From, $To]",
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

  private def notSupportedDerivation(srcPrefixTree: Tree,
                                     fromTpe: Type,
                                     toTpe: Type): Left[Seq[NotSupportedDerivation], Nothing] =
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
}
