package io.scalaland.chimney.internal

private[chimney] class TransformerMacros(val c: scala.reflect.macros.blackbox.Context) {

  import c.universe._

  def genImpl[From: c.WeakTypeTag, To: c.WeakTypeTag]: c.Expr[io.scalaland.chimney.Transformer[From, To]] = {

    val From = weakTypeOf[From]
    val To = weakTypeOf[To]

    println(s"XXXXX: $From ~~~~> $To")

    val FromSymbol = From.typeSymbol
    val ToSymbol = To.typeSymbol

    (FromSymbol.classSymbolOpt, ToSymbol.classSymbolOpt) match {
      case (Some(fromClassType), Some(toClassType)) if fromClassType.isCaseClass && toClassType.isCaseClass =>

        println("Transforming between case classes!!!")

        val srcName = c.internal.reificationSupport.freshTermName(fromClassType.name.decodedName.toString.toLowerCase + "$")
        val srcPrefix = Ident(TermName(srcName.decodedName.toString))
        val transformingTree = expandCaseClassTransformingExpr(srcPrefix)(From, To)
        val tree = q"""
           new _root_.io.scalaland.chimney.Transformer[$From, $To] {
             def transform($srcName: $From): $To = $transformingTree
           }
        """

        println(s"FINAL TREE:\n$tree")

        c.Expr[io.scalaland.chimney.Transformer[From, To]](tree)

      case (from, to) =>
        c.abort(c.enclosingPosition, s"Unsupported case yet: $from ~~> $to")
    }
  }

  def expandCaseClassTransformingExpr(srcPrefix: Tree)(From: Type, To: Type): c.Tree = {

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

      def missingFieldLine(ms: MethodSymbol): String =
        s"  ${ms.name}: ${ms.returnType} - no field named ${ms.name} in source type $From"

      val errorMessage =
        s"""Chimney can't find a data source for the following fields:
           |$To
           |${missingFields.map(missingFieldLine).mkString("\n")}
           |
           |Try adding .withFieldConst, .withFieldComputed or .withFieldRenamed. See more examples at $chimneyDocUrl
           |
           |""".stripMargin

      c.abort(c.enclosingPosition, errorMessage)

    } else {

      val args = mapping.collect {
        case (targetField, Some(sourceField)) if sourceField.returnType <:< targetField.returnType =>
          // we know field types are the same
          // TODO: we can be less strict and require only sourceField.returnType <:< targetField.returnType
          q"$srcPrefix.${sourceField.name}"

        case (targetField, Some(sourceField)) =>

          println("SRCRET: " + sourceField.returnType + "  TARGRET: " + targetField.returnType)

          findLocalImplicitTransformer(sourceField.returnType, targetField.returnType) match {
            case Some(localImplicitTransformer) =>

              q"$localImplicitTransformer.transform($srcPrefix.${sourceField.name})"

            case None if sourceField.returnType.isCaseClass && targetField.returnType.isCaseClass =>

              println("Both case classes, so GOOO!!!")

              expandCaseClassTransformingExpr(q"$srcPrefix.${sourceField.name}")(sourceField.returnType, targetField.returnType)

            case None =>

              c.abort(c.enclosingPosition, "!!!")
          }
      }

      q"new $To(..$args)".debug
    }
  }

  def findLocalImplicitTransformer(From: Type, To: Type): Option[Tree] = {
    println(s"From = $From, To = $To")

    val tpeTree = c.typecheck(tree = tq"_root_.io.scalaland.chimney.Transformer[$From, $To]",
      silent = false,
      mode = c.TYPEmode,
      withImplicitViewsDisabled = true,
      withMacrosDisabled = true)


    println(tpeTree + " ||| " + tpeTree.tpe)

    val res = scala.util.Try(c.inferImplicitValue(tpeTree.tpe, withMacrosDisabled = true))
    println(res)
    val res2 = res.toOption.filterNot(_ == EmptyTree)
    println(res2)
    res2
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
}
