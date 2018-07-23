package io.scalaland.chimney.internal

import scala.reflect.macros.blackbox

trait TransformerMacros {
  this: DerivationGuards with MacroUtils with DerivationConfig =>

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
        } else if (bothEithers(From, To)) {
          expandEithers(srcPrefixTree, config)(From, To)
        } else if (bothMaps(From, To)) {
          expandMaps(srcPrefixTree, config)(From, To)
        } else if (bothOfTraversableOrArray(From, To)) {
          expandTraversableOrArray(srcPrefixTree, config)(From, To)
        } else if (destinationCaseClass(To)) {
          expandDestinationCaseClass(srcPrefixTree, config)(From, To)
        } else if (bothSealedClasses(From, To)) {
          expandSealedClasses(srcPrefixTree, config)(From, To)
        } else {
          Left {
            Seq(NotSupportedDerivation(From.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString))
          }
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

  def expandOptions(srcPrefixTree: Tree, config: Config)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val fromInnerT = From.typeArgs.head
    val toInnerT = To.typeArgs.head

    if ((From <:< someTpe && To <:< noneTpe) || (From <:< noneTpe && To <:< someTpe)) {
      Left {
        Seq(NotSupportedDerivation(From.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString))
      }
    } else if (From <:< someTpe && To <:< someTpe) {
      expandTransformerTree(q"$srcPrefixTree.get", config.rec)(fromInnerT, toInnerT).right.map { innerTransformer =>
        q"new _root_.scala.Some[$toInnerT]($innerTransformer)"
      }
    } else {
      val fn = c.internal.reificationSupport.freshTermName("x$")
      expandTransformerTree(Ident(fn), config.rec)(fromInnerT, toInnerT).right.map { innerTransformer =>
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
      expandTransformerTree(q"$srcPrefixTree.left.get", config.rec)(fromLeftT, toLeftT).right.map { leftTransformer =>
        q"new _root_.scala.util.Left($leftTransformer)"
      }
    } else if (From <:< rightTpe && !(To <:< leftTpe)) {
      expandTransformerTree(q"$srcPrefixTree.right.get", config.rec)(fromRightT, toRightT).right.map {
        rightTransformer =>
          q"new _root_.scala.util.Right($rightTransformer)"
      }
    } else if (!(To <:< leftTpe) && !(To <:< rightTpe)) {
      val leftTransformerE = expandTransformerTree(Ident(fnL), config.rec)(fromLeftT, toLeftT)
      val rightTransformerE = expandTransformerTree(Ident(fnR), config.rec)(fromRightT, toRightT)

      if (leftTransformerE.isLeft || rightTransformerE.isLeft) {
        Left(leftTransformerE.left.getOrElse(Nil) ++ rightTransformerE.left.getOrElse(Nil))
      } else {
        val leftTransformer = leftTransformerE.right.get
        val rightTransformer = rightTransformerE.right.get

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
      }
    } else {
      Left {
        Seq(NotSupportedDerivation(From.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString))
      }
    }
  }

  def expandMaps(srcPrefixTree: Tree, config: Config)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    val List(fromKeyT, fromValueT) = From.typeArgs
    val List(toKeyT, toValueT) = To.typeArgs

    val fnK = c.internal.reificationSupport.freshTermName("k$")
    val fnV = c.internal.reificationSupport.freshTermName("v$")

    val keyTransformerE = expandTransformerTree(Ident(fnK), config.rec)(fromKeyT, toKeyT)
    val valueTransformerE = expandTransformerTree(Ident(fnV), config.rec)(fromValueT, toValueT)

    if (keyTransformerE.isLeft || valueTransformerE.isLeft) {
      Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
    } else {
      val keyTransformer = keyTransformerE.right.get
      val valueTransformer = valueTransformerE.right.get

      Right {
        q"""
          $srcPrefixTree.map { case ($fnK: $fromKeyT, $fnV: $fromValueT) =>
            ($keyTransformer, $valueTransformer)
          }
         """
      }
    }
  }

  def expandTraversableOrArray(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                    To: Type): Either[Seq[DerivationError], Tree] = {

    val FromCollectionT = From.typeArgs.head
    val ToCollectionT = To.typeArgs.head

    val fn = Ident(c.internal.reificationSupport.freshTermName("x$"))

    expandTransformerTree(fn, config.rec)(FromCollectionT, ToCollectionT).right.map { innerTransformerTree =>
      val sameCollectionTypes = From.typeConstructor =:= To.typeConstructor

      if (fn == innerTransformerTree) {
        if (sameCollectionTypes) {
          srcPrefixTree
        } else {
          q"$srcPrefixTree.to[${To.typeConstructor}]"
        }
      } else {
        val f = q"($fn: $FromCollectionT) => $innerTransformerTree"
        if (sameCollectionTypes) {
          q"$srcPrefixTree.map($f)"
        } else {
          q"$srcPrefixTree.map($f).to[${To.typeConstructor}]"
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

              expandDestinationCaseClass(Ident(fn), config.rec)(instTpe, targetTpe).right.map { innerTransformerTree =>
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
        val clauses = instanceClauses.map(_.right.get)
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

  def expandDestinationCaseClass(srcPrefixTree: Tree, config: Config)(From: Type,
                                                                      To: Type): Either[Seq[DerivationError], Tree] = {

    var errors = Seq.empty[DerivationError]

    val fromFields = From.getterMethods
    val toFields = To.caseClassParams

    val mapping = toFields.map { targetField =>
      targetField -> resolveField(srcPrefixTree, config, From, To)(targetField, fromFields, To.typeSymbol.asClass)
    }

    val missingFields = mapping.collect { case (field, None) => field }

    missingFields.foreach { ms =>
      errors :+= MissingField(
        fieldName = ms.name.toString,
        fieldTypeName = ms.returnType.typeSymbol.fullName,
        sourceTypeName = From.typeSymbol.fullName,
        targetTypeName = To.typeSymbol.fullName
      )
    }

    val args = mapping.collect {
      case (_, Some(ResolvedFieldTree(tree))) =>
        tree

      case (targetField, Some(MatchingField(sourceField))) =>
        findLocalImplicitTransformer(sourceField.typeSignatureIn(From), targetField.typeSignatureIn(To)) match {
          case Some(localImplicitTransformer) =>
            q"$localImplicitTransformer.transform($srcPrefixTree.${sourceField.name})"

          case None if canTryDeriveTransformer(sourceField.returnType, targetField.returnType) =>
            expandTransformerTree(q"$srcPrefixTree.${sourceField.name}", config.rec)(
              sourceField.returnType,
              targetField.returnType
            ) match {
              case Left(errs) =>
                errors ++= errs
                EmptyTree
              case Right(tree) =>
                tree
            }

          case None =>
            errors :+= MissingTransformer(
              fieldName = targetField.name.toString,
              sourceFieldTypeName = sourceField.returnType.typeSymbol.fullName,
              targetFieldTypeName = targetField.returnType.typeSymbol.fullName,
              sourceTypeName = From.typeSymbol.fullName,
              targetTypeName = To.typeSymbol.fullName
            )

            EmptyTree
        }
    }

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      Right(q"new $To(..$args)")
    }
  }

  sealed trait FieldResolution
  case class ResolvedFieldTree(tree: Tree) extends FieldResolution
  case class MatchingField(ms: MethodSymbol) extends FieldResolution

  def resolveField(srcPrefixTree: Tree, config: Config, tFrom: Type, tTo: Type)(
    targetField: MethodSymbol,
    fromParams: Iterable[MethodSymbol],
    targetCaseClass: ClassSymbol
  ): Option[FieldResolution] = {

    val fieldName = targetField.name.decodedName.toString

    if (config.overridenFields.contains(fieldName)) {
      Some {
        ResolvedFieldTree {
          q"${TermName(config.prefixValName)}.overrides($fieldName).asInstanceOf[${targetField.typeSignatureIn(tTo)}]"
        }
      }
    } else if (config.renamedFields.contains(fieldName)) {
      val fromFieldName = TermName(config.renamedFields(fieldName))
      Some {
        ResolvedFieldTree {
          q"$srcPrefixTree.$fromFieldName"
        }
      }
    } else {
      fromParams
        .find(_.name == targetField.name)
        .map { ms =>
          if (ms.typeSignatureIn(tFrom) <:< targetField.typeSignatureIn(tTo)) {
            ResolvedFieldTree {
              q"$srcPrefixTree.${targetField.name}"
            }
          } else {
            MatchingField(ms)
          }
        }
        .orElse {
          val targetDefault = targetCaseClass.caseClassDefaults.get(targetField.name.toString)
          if (!config.disableDefaultValues && targetDefault.isDefined) {
            Some(ResolvedFieldTree(targetDefault.get))
          } else {
            None
          }
        }
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

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
