package io.scalaland.chimney.internal


import scala.reflect.macros.blackbox


trait TransformerMacros {
  this: MacroUtils with DerivationConfig =>

  val c: blackbox.Context

  import c.universe._

  def genTransformer[From: c.WeakTypeTag, To: c.WeakTypeTag](config: Config): c.Expr[io.scalaland.chimney.Transformer[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    println(s"XXXXX: $From ~~~~> $To, config = $config")

    val srcName =
      c.internal.reificationSupport.freshTermName(From.typeSymbol.name.decodedName.toString.toLowerCase + "$")
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

    println("PREFIX TPE DECLS:" + c.prefix.tree.tpe.decls)

    expandTransformerTree(srcPrefixTree)(From, To) match {

      case Right(transformerTree) =>
        val tree = q"""
           new _root_.io.scalaland.chimney.Transformer[$From, $To] {
             def transform($srcName: $From): $To = $transformerTree
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

  def expandTransformerTree(srcPrefixTree: Tree)(From: Type, To: Type): Either[Seq[DerivationError], Tree] = {

    var errors = Seq.empty[DerivationError]

    if (From.isCaseClass && To.isCaseClass) {
      expandCaseClassTransformerTree(srcPrefixTree)(From, To)
    } else {
      Left(Seq(NotSupportedDerivation(From.typeSymbol.name.toString, To.typeSymbol.name.toString)))
    }
  }

  def expandCaseClassTransformerTree(srcPrefixTree: Tree)(From: Type,
                                                          To: Type): Either[Seq[DerivationError], Tree] = {

    var errors = Seq.empty[DerivationError]

    val fromParams = From.caseClassParams
    val toParams = To.caseClassParams

    println(s"expandCaseClassTransformingExpr: $From ~> $To")
    println(s"fromParams: $fromParams | toParams: $toParams")

    val mapping = toParams.map { param =>
      param -> fromParams.find(_.name == param.name)
    }

    mapping.foreach(println)

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
      case (targetField, Some(sourceField)) if sourceField.returnType <:< targetField.returnType =>
        q"$srcPrefixTree.${sourceField.name}"

      case (targetField, Some(sourceField)) =>
        println("SRCRET: " + sourceField.returnType + "  TARGRET: " + targetField.returnType)

        findLocalImplicitTransformer(sourceField.returnType, targetField.returnType) match {
          case Some(localImplicitTransformer) =>
            q"$localImplicitTransformer.transform($srcPrefixTree.${sourceField.name})"

          case None if sourceField.returnType.isCaseClass && targetField.returnType.isCaseClass =>
            expandTransformerTree(q"$srcPrefixTree.${sourceField.name}")(sourceField.returnType, targetField.returnType) match {
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
