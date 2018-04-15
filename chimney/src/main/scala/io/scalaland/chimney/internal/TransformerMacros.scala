package io.scalaland.chimney.internal

private[chimney] class TransformerMacros(val c: scala.reflect.macros.blackbox.Context) {

  import c.universe._

  def genImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[io.scalaland.chimney.Transformer[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    println(s"XXXXX: $From ~~~~> $To")

    val srcName = c.internal.reificationSupport.freshTermName(From.typeSymbol.name.decodedName.toString.toLowerCase + "$")
    val srcPrefixTree = Ident(TermName(srcName.decodedName.toString))

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
             |${printDerivationErrors(derivationErrors)}
             |See $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        c.abort(c.enclosingPosition, errorMessage)
    }
  }

  def expandTransformerTree(srcPrefixTree: Tree)(From: Type, To: Type): Either[Seq[ChimneyDerivationError], Tree] = {

    var errors = Seq.empty[ChimneyDerivationError]

    if(From.isCaseClass && To.isCaseClass) {
      expandCaseClassTransformerTree(srcPrefixTree)(From, To)
    } else {
      Left(
        Seq(
          NotSupportedDerivation(From.typeSymbol.name.toString, To.typeSymbol.name.toString)
        )
      )
    }
  }

  def expandCaseClassTransformerTree(srcPrefixTree: Tree)(From: Type, To: Type): Either[Seq[ChimneyDerivationError], Tree] = {

    var errors = Seq.empty[ChimneyDerivationError]

    val fromParams = From.caseClassParams
    val toParams = To.caseClassParams

    println(s"expandCaseClassTransformingExpr: $From ~> $To")
    println(s"fromParams: $fromParams | toParams: $toParams")

    val mapping = toParams.map { param =>
      param -> fromParams.find(_.name == param.name)
    }

    mapping.foreach(println)

    val missingFields = mapping.collect { case (field, None) => field }

    if(missingFields.nonEmpty) {
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

    if(errors.nonEmpty) {
      Left(errors)
    } else {
      Right(q"new $To(..$args)".debug)
    }
  }


  def findLocalImplicitTransformer(From: Type, To: Type): Option[Tree] = {
    val tpeTree = c.typecheck(tree = tq"_root_.io.scalaland.chimney.Transformer[$From, $To]",
      silent = false,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = true)

    scala.util.Try(c.inferImplicitValue(tpeTree.tpe, withMacrosDisabled = true))
      .toOption
      .filterNot(_ == EmptyTree)
  }

  private val primitives = Set(
    typeOf[Double],
    typeOf[Float],
    typeOf[Short],
    typeOf[Byte],
    typeOf[Int],
    typeOf[Long],
    typeOf[Char],
    typeOf[Boolean],
    typeOf[Unit]
  )

  private implicit class TypeOps(t: Type) {

    def isValueClass: Boolean =
      t <:< typeOf[AnyVal] && !primitives.exists(_ =:= t)

    def isCaseClass: Boolean =
      t.typeSymbol.classSymbolOpt.exists(_.isCaseClass)

    def caseClassParams: Iterable[MethodSymbol] =
      t.decls.collect {
        case m: MethodSymbol if m.isCaseAccessor || (isValueClass && m.isParamAccessor) =>
          m.asMethod
      }
  }

  private implicit class SymbolOps(s: Symbol) {

    def classSymbolOpt: Option[ClassSymbol] =
      if(s.isClass) Some(s.asClass) else None
  }

  private implicit class TreeOps(t: Tree) {

    def debug: Tree = {
      println("TREE: " + t)
      println("RAW:  " + showRaw(t))
      t
    }
  }

  private val chimneyDocUrl = "http://scalalandio.github.io/chimney"


  sealed trait ChimneyDerivationError {
    def sourceTypeName: String
    def targetTypeName: String
  }

  case class MissingField(fieldName: String,
                          fieldTypeName: String,
                          sourceTypeName: String,
                          targetTypeName: String) extends ChimneyDerivationError

  case class MissingTransformer(fieldName: String,
                                sourceFieldTypeName: String,
                                targetFieldTypeName: String,
                                sourceTypeName: String,
                                targetTypeName: String) extends ChimneyDerivationError

  case class NotSupportedDerivation(sourceTypeName: String,
                                    targetTypeName: String) extends ChimneyDerivationError

  private def printDerivationErrors(errors: Seq[ChimneyDerivationError]): String = {

    errors
      .groupBy(_.targetTypeName)
      .map { case (targetTypeName, errs) =>

        val errStrings = errs.map {
          case MissingField(fieldName, fieldTypeName, sourceTypeName, _) =>
            s"  $fieldName: $fieldTypeName - no field named $fieldName in source type $sourceTypeName"
          case MissingTransformer(fieldName, sourceFieldTypeName, targetFieldTypeName, sourceTypeName, _) =>
            s"  $fieldName: $targetFieldTypeName - can't derive transformation from $fieldName: $sourceFieldTypeName in source type $sourceTypeName"
          case NotSupportedDerivation(sourceTypeName, _) =>
            s"  derivation from $sourceTypeName is not supported in Chimney!"
        }

        s"""$targetTypeName
           |${errStrings.mkString("\n")}
           |""".stripMargin
      }
      .mkString("\n")
  }
}
