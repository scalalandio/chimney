package io.scalaland.chimney.internal

import scala.reflect.macros.whitebox

trait TransformerMacros {
  this: MacroUtils with DerivationConfig with Prefixes  =>

  val c: whitebox.Context

  import c.universe._

  def genTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag](
    config: Config
  ): c.Expr[io.scalaland.chimney.Transformer[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    println(s"XXXXX: $From ~~~~> $To, config = $config")

    val srcName =
      c.internal.reificationSupport.freshTermName(From.typeSymbol.name.decodedName.toString.toLowerCase + "$")
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    expandTransformerTree(srcPrefixTree, config)(From, To) match {

      case Right(transformerTree) =>

        val prefixStats = c.prefix.tree.extractStats

        val tree = q"""
           new _root_.io.scalaland.chimney.Transformer[$From, $To] {
             def transform($srcName: $From): $To = {
               ..$prefixStats
               $transformerTree
             }
           }
        """
        println(s"FINAL TREE:\n$tree")

        c.Expr[io.scalaland.chimney.Transformer[From, To]](tree)

      case Left(derivationErrors) =>
        val errorMessage =
          s"""Chimney can't derive transformation from $From to $To
             |
             |${DerivationError.printErrors(derivationErrors)}
             |See $chimneyDocUrl for usage examples.
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
        if (From.isCaseClass && To.isCaseClass) {
          expandCaseClassTransformerTree(srcPrefixTree, config)(From, To)
        } else {
          Left(Seq(NotSupportedDerivation(From.typeSymbol.name.toString, To.typeSymbol.name.toString)))
        }
      }
  }

  sealed trait FieldResolution
  case class ResolvedFieldTree(tree: Tree) extends FieldResolution
  case class MatchingField(ms: MethodSymbol) extends FieldResolution

  def resolveField(targetField: MethodSymbol,
                   fromParams: Iterable[MethodSymbol],
                   srcPrefixTree: Tree,
                   config: Config): Option[FieldResolution] = {

    val fieldName = targetField.name.decodedName.toString

    if(config.overridenFields.contains(fieldName)) {
      Some {
        ResolvedFieldTree {
          q"overrides.apply($fieldName).asInstanceOf[${targetField.returnType}]"
        }
      }
    } else {
      fromParams
        .find(_.name == targetField.name)
        .map { ms =>
          if (ms.returnType <:< targetField.returnType) {
            ResolvedFieldTree {
              q"$srcPrefixTree.${targetField.name}"
            }
            //            } else if (!config.disableDefaultValues && targetField.isParamWithDefault) {
            //              println("LALALALALALALALALALALA")
            //              ResolvedFieldTree {
            //                q"""Bar3.apply$$default$$1()"""
            //              }
          } else {
            MatchingField(ms)
          }
        }
    }
//
//    config.fieldTrees
//      .get(fieldName)
//      .map {
//        case PastedTree(isFun, tree) =>
//          ResolvedFieldTree {
//            if (isFun) {
//              q"${computedRefName(fieldName)}($srcPrefixTree)"
//            } else {
//              q"${constRefName(fieldName)}"
//            }
//          }
//      }
//      .orElse {
//
//      }
  }

  def expandCaseClassTransformerTree(srcPrefixTree: Tree,
                                     config: Config)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    var errors = Seq.empty[DerivationError]

    val fromFields = From.caseClassParams
    val toFields = To.caseClassParams

    val mapping = toFields.map { targetField =>
      targetField -> resolveField(targetField, fromFields, srcPrefixTree, config)
    }

    val missingFields = mapping.collect { case (field, None) => field }

    if (missingFields.nonEmpty) {
      missingFields.foreach { ms =>
        errors :+= MissingField(
          fieldName = ms.name.toString,
          fieldTypeName = ms.returnType.toString,
          sourceTypeName = From.toString,
          targetTypeName = To.toString
        )
      }
    }

    val args = mapping.collect {
      case (targetField, Some(ResolvedFieldTree(tree))) =>
        tree

      case (targetField, Some(MatchingField(sourceField))) =>
        findLocalImplicitTransformer(sourceField.returnType, targetField.returnType) match {
          case Some(localImplicitTransformer) =>
            q"$localImplicitTransformer.transform($srcPrefixTree.${sourceField.name})"

          case None if sourceField.returnType.isCaseClass && targetField.returnType.isCaseClass =>
            val recConfig = config.copy(overridenFields = Set.empty)
            expandTransformerTree(q"$srcPrefixTree.${sourceField.name}", recConfig)(
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
              sourceFieldTypeName = sourceField.returnType.toString,
              targetFieldTypeName = targetField.returnType.toString,
              sourceTypeName = From.toString,
              targetTypeName = To.toString
            )

            EmptyTree
        }
    }

    if (errors.nonEmpty) {
      Left(errors)
    } else {
      Right(q"new $To(..$args)".debug)
    }
  }

  def findLocalImplicitTransformer(From: Type, To: Type): Option[Tree] = {
    val tpeTree = c.typecheck(
      tree = tq"_root_.io.scalaland.chimney.Transformer[$From, $To]",
      silent = false,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = true
    )

    scala.util
      .Try(c.inferImplicitValue(tpeTree.tpe, withMacrosDisabled = true))
      .toOption
      .filterNot(_ == EmptyTree)
  }

  private val chimneyDocUrl = "http://scalalandio.github.io/chimney"
}
