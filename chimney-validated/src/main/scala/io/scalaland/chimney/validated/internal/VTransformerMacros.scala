package io.scalaland.chimney.validated.internal

import cats.implicits._
import io.scalaland.chimney.internal._

// import cats.implicits for successful usage
trait VTransformerMacros {
  this: TransformerMacros with DerivationGuards with MacroUtils with DerivationVConfig with EitherUtils =>

  import c.universe._

  def genVTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag](
    config: VConfig,
    findImplicit: Boolean = true
  ): c.Expr[io.scalaland.chimney.validated.VTransformer[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    val srcName =
      c.internal.reificationSupport.freshTermName(From.typeSymbol.name.decodedName.toString.toLowerCase + "$")
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    expandVTransformerTree(srcPrefixTree, config, findImplicit)(From, To) match {

      case Right(transformerTree) =>
        val tree = q"""
           new _root_.io.scalaland.chimney.validated.VTransformer[$From, $To] {
             def transform($srcName: $From): _root_.cats.data.ValidatedNec[_root_.io.scalaland.chimney.validated.VTransformer.Error, $To] = {
               $transformerTree
             }
           }
        """

        c.Expr[io.scalaland.chimney.validated.VTransformer[From, To]](tree)

      case Left(derivationErrors) =>
        val errorMessage =
          s"""Chimney can't derive validation transformation from $From to $To
             |
             |${DerivationError.printErrors(derivationErrors)}
             |
             |""".stripMargin

        c.abort(c.enclosingPosition, errorMessage)
    }
  }

  def expandVTransformerTree(srcPrefixTree: Tree,
                             config: VConfig,
                             findImplicit: Boolean = true)(From: Type, To: Type): Either[Seq[DerivationError], Tree] =
    expandTransformerTree(srcPrefixTree, config.underlying)(From, To)
      .mapRight { succeed =>
        q"$succeed.validNec"
      }
      .orElse {
        (if (findImplicit) findLocalImplicitVTransformer(From, To) else None)
          .map { localImplicitTree =>
            Right(q"$localImplicitTree.transform($srcPrefixTree)")
          }
          .getOrElse {
            if (From <:< optionTpe) {
              expandVFromOption(srcPrefixTree, config)(From, To)
            } else if (destinationCaseClass(To)) {
              expandVDestinationCaseClass(srcPrefixTree, config)(From, To)
            } else if (bothMaps(From, To)) {
              expandVMaps(srcPrefixTree, config)(From, To)
            } else if (bothOfTraversableOrArray(From, To)) {
              expandVTraversableOrArray(srcPrefixTree, config)(From, To)
            } else {
              notSupportedDerivation(srcPrefixTree, From, To)
            }
          }
      }

  def findLocalImplicitVTransformer(From: Type, To: Type): Option[Tree] = {
    val tpeTree = c.typecheck(
      tree = tq"_root_.io.scalaland.chimney.validated.VTransformer[$From, $To]",
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

  def expandVDestinationCaseClass(srcPrefixTree: Tree,
                                  config: VConfig)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {
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
            Target.fromField(targetField, To) -> Some(VBaseTargetResolution(MatchingSourceAccessor(sourceField)))
        }
      }
    } else {
      val fromGetters = From.getterMethods
      toFields.map { targetField =>
        val target = Target.fromField(targetField, To)
        target -> resolveVTarget(srcPrefixTree, config, From, To)(target, fromGetters, Some(To.typeSymbol.asClass))
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

    val (resolutionErrors, args) = resolveVTargetArgTrees(srcPrefixTree, config, From, To)(mapping)

    errors ++= resolutionErrors

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      val argValues = args.map(_._1)
      if (argValues.size == 1)
        Right(q"(..$argValues).map(new $To(_))")
      else {
        val argTerms = (1 to args.size).map(idx => c.internal.reificationSupport.freshTermName("arg$" + idx)).toList
        val argTypes = args.map(_._2)
        val mkCaseClass =
          Function(
            argTerms.zip(argTypes).map { case (term, tpe) => ValDef(Modifiers.apply, term, tq"$tpe", EmptyTree) },
            q"new $To(..$argTerms)"
          )
        Right(q"(..$argValues).mapN($mkCaseClass)")
      }
    }
  }

  def resolveVTarget(srcPrefixTree: Tree, config: VConfig, From: Type, To: Type)(
    target: Target,
    fromGetters: Iterable[MethodSymbol],
    targetCaseClass: Option[ClassSymbol]
  ): Option[VTargetResolution] = {
    if (config.overridenVFields.contains(target.name)) {
      Some {
        ResolvedTargetVTree(
          q"${TermName(config.underlying.prefixValName)}.overridesV(${target.name}).asInstanceOf[_root_.cats.data.ValidatedNec[_root_.io.scalaland.chimney.validated.VTransformer.Error, ${target.tpe}]]",
          target.name
        )
      }
    } else
      resolveTarget(srcPrefixTree, config.underlying, From, To)(target, fromGetters, targetCaseClass)
        .map(VBaseTargetResolution)
  }

  def resolveVTargetArgTrees(srcPrefixTree: Tree, config: VConfig, From: Type, To: Type)(
    mapping: Iterable[(Target, Option[VTargetResolution])]
  ): (Seq[DerivationError], Iterable[(Tree, Type)]) = {

    var errors = Seq.empty[DerivationError]

    val args = mapping.collect {
      case (target, Some(VBaseTargetResolution(ResolvedTargetTree(tree)))) =>
        (q"_root_.cats.data.Validated.Valid($tree)", target.tpe)

      case (target, Some(ResolvedTargetVTree(tree, name))) =>
        (q"_root_.io.scalaland.chimney.validated.VTransformer.addPrefix($tree, $name)", target.tpe)

      case (target, Some(VBaseTargetResolution(MatchingSourceAccessor(sourceField)))) =>
        val prefix = sourceField.canonicalName

        findLocalImplicitVTransformer(sourceField.resultTypeIn(From), target.tpe) match {
          case Some(localImplicitTransformer) =>
            (
              q"_root_.io.scalaland.chimney.validated.VTransformer.addPrefix($localImplicitTransformer.transform($srcPrefixTree.${sourceField.name}), $prefix)",
              target.tpe
            )

          case None =>
            expandVTransformerTree(q"$srcPrefixTree.${sourceField.name}", config.rec)(
              sourceField.resultTypeIn(From),
              target.tpe
            ) match {
              case Left(errs) =>
                errors ++= errs
                (EmptyTree, target.tpe)
              case Right(tree) =>
                (q"_root_.io.scalaland.chimney.validated.VTransformer.addPrefix($tree, $prefix)", target.tpe)
            }
        }
    }

    (errors, args)
  }

  def expandVFromOption(srcPrefixTree: Tree, config: VConfig)(From: Type,
                                                              To: Type): Either[Seq[DerivationError], Tree] = {
    val fn = c.internal.reificationSupport.freshTermName("inn$")
    val fromInnerT = From.typeArgs.head
    expandVTransformerTree(Ident(fn), config.rec)(fromInnerT, To).mapRight { inner =>
      q"""$srcPrefixTree.fold[_root_.cats.data.ValidatedNec[_root_.io.scalaland.chimney.validated.VTransformer.Error, $To]](_root_.io.scalaland.chimney.validated.VTransformer.error[$To]("Should be required"))(($fn: $fromInnerT) => $inner)"""
    }
  }

  def expandVTraversableOrArray(srcPrefixTree: Tree, config: VConfig)(From: Type,
                                                                      To: Type): Either[Seq[DerivationError], Tree] = {

    val FromCollectionT = From.typeArgs.head
    val ToCollectionT = To.typeArgs.head

    val fn = Ident(c.internal.reificationSupport.freshTermName("x$"))

    expandVTransformerTree(fn, config.rec)(FromCollectionT, ToCollectionT)
      .mapRight { innerTransformerTree =>
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
            q"$srcPrefixTree.map($f).to($ToCompanionRef)"
          } else {
            q"$srcPrefixTree.map($f).to[${To.typeConstructor}]"
          }
        }
      }
      .mapRight { listTree =>
        q"""$listTree.mapWithIndex((c, index) => _root_.io.scalaland.chimney.validated.VTransformer.addPrefix(c, s"[$$index]")).sequence"""
      }
  }

  def expandVMaps(srcPrefixTree: Tree, config: VConfig)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromKeyT, fromValueT) = From.typeArgs
    val List(toKeyT, toValueT) = To.typeArgs

    val fnK = c.internal.reificationSupport.freshTermName("key$")
    val fnV = c.internal.reificationSupport.freshTermName("value$")

    val keyTransformerE = expandVTransformerTree(Ident(fnK), config.rec)(fromKeyT, toKeyT)
    val valueTransformerE = expandVTransformerTree(Ident(fnV), config.rec)(fromValueT, toValueT)

    (keyTransformerE, valueTransformerE) match {
      case (Right(keyTransformer), Right(valueTransformer)) =>
        Right {
          q"""
          $srcPrefixTree.toList.map { case ($fnK: $fromKeyT, $fnV: $fromValueT) =>
              (
                _root_.io.scalaland.chimney.validated.VTransformer.addPrefix($keyTransformer, "keys"),
                _root_.io.scalaland.chimney.validated.VTransformer.addPrefix($valueTransformer, $fnK)
             ).bisequence
          }.sequence.map(_.toMap)
         """
        }
      case _ =>
        Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
    }
  }

  sealed trait VTargetResolution

  case class VBaseTargetResolution(underlying: TargetResolution) extends VTargetResolution
  case class ResolvedTargetVTree(tree: Tree, name: String) extends VTargetResolution
}
