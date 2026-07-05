package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions
import io.scalaland.chimney.internal.runtime

/** Shared (Scala 2 + Scala 3) parsing of DSL lambdas into type-level representations.
  *
  * Selector lambdas (`_.foo.matching[A].everyItem`) become `runtime.Path` types and constructor lambdas (`(a: Int)(b:
  * String) => ...`) become `runtime.ArgumentLists` types, both parsed through Hearth's [[DestructuredExpr]] - the
  * platform-specific tree differences (Scala 2 implicit-class wrappers vs Scala 3 extension methods, `Function` vs
  * `DefDef`+`Closure` lambdas) are already normalized by Hearth.
  */
private[compiletime] trait DslDefinitions { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Selector methods (implicit-class on Scala 2 / extensions on Scala 3 in `io.scalaland.chimney.syntax`) recognized
    * as path steps rather than field accesses.
    */
  private val pathMarkers =
    Set("matching", "matchingSome", "matchingLeft", "matchingRight", "everyItem", "everyMapKey", "everyMapValue")

  protected def parsePathType(selector: Expr[Any]): Either[String, ??<:[runtime.Path]] =
    DestructuredExpr.parseUntyped(UntypedExpr.fromTyped(selector)) match {
      case lambda: DestructuredExpr.Lambda if lambda.params.sizeIs == 1 =>
        parsePathBody(selector, lambda.params.head, lambda.body)
      case _ => Left(invalidSelectorMessage(selector))
    }

  private def parsePathBody(
      selector: Expr[Any],
      root: DestructuredExpr.Lambda.Param,
      body: DestructuredExpr
  ): Either[String, ??<:[runtime.Path]] = body match {
    // matches `_` part in `_.foo.bar.baz...`
    case ref: DestructuredExpr.Lambda.ParamRef if ref.param eq root => Right(rootPath)
    case mc: DestructuredExpr.MethodCall                            => parsePathStep(selector, root, mc)
    case block: DestructuredExpr.Block                              => parsePathBody(selector, root, block.result)
    // matches `_ => something unrelated` - not allowed
    case external if isExternalIdentifier(external) => Left(ignoredInputMessage(external, selector))
    // matches an unresolvable application, e.g. an operator on a mistyped receiver - not allowed
    case nd: DestructuredExpr.NonDestructurable if nd.description.trim.endsWith(")") =>
      Left(arbitraryFunctionMessage(nd, selector))
    case _ => Left(invalidSelectorMessage(selector))
  }

  private def parsePathStep(
      selector: Expr[Any],
      root: DestructuredExpr.Lambda.Param,
      mc: DestructuredExpr.MethodCall
  ): Either[String, ??<:[runtime.Path]] = {
    val instance = mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance => ai.value }
    // Type arguments of the marker itself: on both platforms the marker's OWN type parameters come last
    // (receiver-inferred type arguments, if any, come first).
    def markerTypeArgs: List[??] =
      mc.applied.collect { case at: DestructuredExpr.MethodCall.AppliedTypes => at.typeArgs }.lastOption.getOrElse(Nil)
    def valueClauses: List[List[DestructuredExpr]] =
      mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }

    def recurse(subject: DestructuredExpr): Either[String, ??<:[runtime.Path]] =
      parsePathBody(selector, root, subject)

    // A plain member access (a field named e.g. `everyItem` must not be mistaken for the marker, which always
    // carries an implicit wrapper/evidence/type arguments on top).
    val isPlainAccess = mc.applied match {
      case List(_: DestructuredExpr.MethodCall.AppliedInstance)                                                => true
      case List(_: DestructuredExpr.MethodCall.AppliedInstance, av: DestructuredExpr.MethodCall.AppliedValues) =>
        av.args.isEmpty
      case _ => false
    }

    mc.method.name match {
      case marker if pathMarkers(marker) && !isPlainAccess =>
        // On Scala 2 the receiver is hidden inside the implicit-class conversion call (its first value argument);
        // on Scala 3 Hearth already normalizes the extension receiver into the instance slot.
        val subject = instance match {
          case Some(wrapper: DestructuredExpr.MethodCall) if wrapper.method.isImplicit =>
            wrapper.applied
              .collectFirst {
                case av: DestructuredExpr.MethodCall.AppliedValues if av.args.nonEmpty => av.args.head
              }
              .getOrElse(wrapper)
          case Some(direct) => direct
          case None         => return Left(invalidSelectorMessage(selector))
        }
        recurse(subject).flatMap { init =>
          (marker, markerTypeArgs) match {
            // matches `.matching[Subtype]`
            case ("matching", args) if args.nonEmpty => Right(matchingPath(init, args.last))
            // matches `.matchingSome` == `.matching[Some[A]].value` (marker type args are `[SomeValue, Some]`)
            case ("matchingSome", List(_, some)) => Right(selectPath(matchingPath(init, some), "value"))
            // matches `.matchingLeft` == `.matching[Left[L, R]].value` (marker type args are `[LV, RV, Left, Right]`)
            case ("matchingLeft", List(_, _, left, _)) => Right(selectPath(matchingPath(init, left), "value"))
            // matches `.matchingRight` == `.matching[Right[L, R]].value`
            case ("matchingRight", List(_, _, _, right)) => Right(selectPath(matchingPath(init, right), "value"))
            case ("everyItem", _)                        => Right(everyItemPath(init))
            case ("everyMapKey", _)                      => Right(everyMapKeyPath(init))
            case ("everyMapValue", _)                    => Right(everyMapValuePath(init))
            case _                                       => Left(invalidSelectorMessage(selector))
          }
        }

      case "apply" if namedTupleField(mc).isDefined =>
        // matches `.fieldName` on a named tuple (Scala 3), desugared to `NamedTuple.apply[Ns, Vs](subject)(idx)`
        val (subject, fieldName) = namedTupleField(mc).get
        recurse(subject).map(init => selectPath(init, fieldName))

      case fieldName =>
        mc.applied match {
          // matches `.fieldName` and `.fieldName()`
          case List(_: DestructuredExpr.MethodCall.AppliedInstance) =>
            recurse(instance.get).map(init => selectPath(init, fieldName))
          case List(_: DestructuredExpr.MethodCall.AppliedInstance, av: DestructuredExpr.MethodCall.AppliedValues)
              if av.args.isEmpty =>
            recurse(instance.get).map(init => selectPath(init, fieldName))
          // matches module methods/vals (`_ => someObject.something`, Scala 3 shape) - not allowed
          case _ if instance.isEmpty => Left(ignoredInputMessage(mc, selector))
          // matches `someFunctionName(...)` - not allowed
          case _ if valueClauses.exists(_.nonEmpty) || markerTypeArgs.nonEmpty =>
            Left(arbitraryFunctionMessage(mc, selector))
          case _ => Left(invalidSelectorMessage(selector))
        }
    }
  }

  private def isExternalIdentifier(node: DestructuredExpr): Boolean = node match {
    case _: DestructuredExpr.Singleton => true
    // A local val/var reference: an identifier the destructurer could not resolve further.
    case nd: DestructuredExpr.NonDestructurable => isSimpleIdentifier(nd.description)
    case _                                      => false
  }

  private def isSimpleIdentifier(description: String): Boolean =
    description.nonEmpty && description.head.isUnicodeIdentifierStart && description.forall(c =>
      c.isUnicodeIdentifierPart || c == '$'
    )

  /** Field name recovered from Scala 3's named-tuple accessor shape:
    * `NamedTuple.apply[Names, Values](subject)(nameIdx)`. Never matches on Scala 2.
    */
  private def namedTupleField(mc: DestructuredExpr.MethodCall): Option[(DestructuredExpr, String)] = {
    val isNamedTupleModule = mc.applied.collectFirst { case ai: DestructuredExpr.MethodCall.AppliedInstance =>
      ai.value
    } match {
      case Some(singleton: DestructuredExpr.Singleton) => singleton.tpe.plainPrint == "scala.NamedTuple.type"
      case _                                           => false
    }
    if (!isNamedTupleModule) None
    else {
      val valueClauses = mc.applied.collect { case av: DestructuredExpr.MethodCall.AppliedValues => av.args }
      for {
        names <- mc.applied
          .collectFirst { case at: DestructuredExpr.MethodCall.AppliedTypes => at.typeArgs }
          .flatMap(_.headOption)
        subject <- valueClauses.headOption.flatMap(_.headOption)
        index <- valueClauses.lift(1).flatMap(_.headOption).collect { case literal: DestructuredExpr.Literal =>
          literal.value
        } match {
          case Some(i: Int) => Some(i)
          case _            => None
        }
        fieldName <- tupleStringElements(names).lift(index)
      } yield (subject, fieldName)
    }
  }

  /** Unrolls a names-tuple type (all elements are constant `String` types) into its element values. Handles both the
    * `TupleN["a", "b", ...]` and the `"a" *: "b" *: EmptyTuple` shapes.
    */
  private def tupleStringElements(names: ??): List[String] = {
    def stringOf(tpe: UntypedType): Option[String] =
      Type.StringCodec.fromType(tpe.asTyped[String]).map(_.value)
    def loop(tpe: UntypedType): List[String] =
      UntypedType.typeArguments(UntypedType.dealias(tpe)) match {
        case Nil  => Nil
        case args =>
          val strings = args.map(stringOf)
          if (strings.forall(_.isDefined)) strings.map(_.get) // TupleN of names
          else
            (strings.headOption.flatten, args) match {
              case (Some(head), List(_, tail)) => head :: loop(tail) // `name *: moreNames` cons
              case _                           => Nil
            }
      }
    loop(names.Underlying.asUntyped)
  }

  // --- runtime.Path building ---

  private lazy val rootPath: ??<:[runtime.Path] = ChimneyType.Path.Root.as_??<:[runtime.Path]

  private def selectPath(init: ??<:[runtime.Path], fieldName: String): ??<:[runtime.Path] = {
    import init.Underlying as Init
    implicit val FieldName: Type[String] = Type.StringCodec.toType(fieldName)
    ChimneyType.Path.Select[Init, String].as_??<:[runtime.Path]
  }

  private def matchingPath(init: ??<:[runtime.Path], subtype: ??): ??<:[runtime.Path] = {
    import init.Underlying as Init, subtype.Underlying as Subtype
    ChimneyType.Path.Matching[Init, Subtype].as_??<:[runtime.Path]
  }

  protected def sourceMatchingRootPath(subtype: ??): ??<:[runtime.Path] = {
    import subtype.Underlying as Subtype
    implicit val RootType: Type[runtime.Path.Root] = ChimneyType.Path.Root
    ChimneyType.Path.SourceMatching[runtime.Path.Root, Subtype].as_??<:[runtime.Path]
  }

  protected def matchingRootPath(subtype: ??): ??<:[runtime.Path] = matchingPath(rootPath, subtype)

  protected lazy val rootPathType: ??<:[runtime.Path] = rootPath

  private def everyItemPath(init: ??<:[runtime.Path]): ??<:[runtime.Path] = {
    import init.Underlying as Init
    ChimneyType.Path.EveryItem[Init].as_??<:[runtime.Path]
  }

  private def everyMapKeyPath(init: ??<:[runtime.Path]): ??<:[runtime.Path] = {
    import init.Underlying as Init
    ChimneyType.Path.EveryMapKey[Init].as_??<:[runtime.Path]
  }

  private def everyMapValuePath(init: ??<:[runtime.Path]): ??<:[runtime.Path] = {
    import init.Underlying as Init
    ChimneyType.Path.EveryMapValue[Init].as_??<:[runtime.Path]
  }

  // --- runtime.ArgumentLists building ---

  protected def parseArgumentListsType(f: Expr[Any]): Either[String, ??<:[runtime.ArgumentLists]] =
    DestructuredExpr.parseUntyped(UntypedExpr.fromTyped(f)) match {
      case lambda: DestructuredExpr.Lambda => Right(clausesToType(collectParameterClauses(lambda)))
      case _                               => Left(invalidConstructorMessage(f))
    }

  /** Collects parameter clauses of (possibly curried/eta-expanded, i.e. nested) constructor lambdas. */
  private def collectParameterClauses(lambda: DestructuredExpr.Lambda): List[List[DestructuredExpr.Lambda.Param]] =
    lambda.body match {
      case inner: DestructuredExpr.Lambda => lambda.params :: collectParameterClauses(inner)
      case _                              => List(lambda.params)
    }

  private def clausesToType(clauses: List[List[DestructuredExpr.Lambda.Param]]): ??<:[runtime.ArgumentLists] =
    clauses
      .map { clause =>
        clause.foldRight(ChimneyType.ArgumentList.Empty.as_??<:[runtime.ArgumentList])(prependArgument)
      }
      .foldRight(ChimneyType.ArgumentLists.Empty.as_??<:[runtime.ArgumentLists])(prependArgumentList)

  private def prependArgument(
      param: DestructuredExpr.Lambda.Param,
      args: ??<:[runtime.ArgumentList]
  ): ??<:[runtime.ArgumentList] = {
    implicit val ParamName: Type[String] = Type.StringCodec.toType(param.name)
    val paramType: ?? = param.tpe
    import paramType.Underlying as ParamType, args.Underlying as Args
    ChimneyType.ArgumentList.Argument[String, ParamType, Args].as_??<:[runtime.ArgumentList]
  }

  private def prependArgumentList(
      head: ??<:[runtime.ArgumentList],
      tail: ??<:[runtime.ArgumentLists]
  ): ??<:[runtime.ArgumentLists] = {
    import head.Underlying as Head, tail.Underlying as Tail
    ChimneyType.ArgumentLists.List[Head, Tail].as_??<:[runtime.ArgumentLists]
  }

  /** `Type[A]` as an existential - for the platform adapters (which have `A` as a macro type parameter). */
  def typeOf_??[A: Type]: ?? = Type[A].as_??

  // --- Java-enum value subtypes (`.withSealedSubtypeHandled((value: Color.Black.type) => ...)`) ---

  /** Recovers the single Java-enum value that Scala 2 erases from `withSealedSubtypeHandled[Subtype]`.
    *
    * `Subtype` is precise on Scala 3 (`Color.Black.type`) but on Scala 2 the value's singleton is erased from EVERY
    * typed carrier - the inferred `Subtype`, the handler lambda's `Function1` type and its [[DestructuredExpr]]
    * `param.tpe` all widen to the enum class `Color` (empirically `param.tpe.Underlying.plainPrint == "…Color"`,
    * `isJavaEnumValue == false`). Both spellings answer `isJavaEnum && !isJavaEnumValue`, so the two are told apart by
    * `shortName`: Scala 3's value type has a VALUE short name (`Black`, already among the enum's `directChildrenList`
    * values) and is passed through untouched, while Scala 2's widened subtype has the ENUM-CLASS short name (`Color`)
    * and needs the value recovered. The one surviving witness of the value name on Scala 2 is the handler parameter's
    * DECLARED (un-widened) type, which Hearth exposes cross-platform via `DestructuredExpr.Lambda.Param.declaredTpe`
    * (hearth#341): the parameter's `Enum.Value.type` ascription names the value. We read it there, cross-check it
    * against the enum's actual values and re-encode it as `runtime.RefinedJavaEnum[E, "Name"]` (a Scala 2 whitebox
    * cannot spell the platform's Java-enum-value type inside the refined `Overrides` type; `Configurations.extractPath`
    * / `fixJavaEnumCompat` decode the marker back into the value type). When no single value is named (the handler
    * covers the whole enum) the type is passed through unchanged.
    */
  protected def javaEnumFixedSubtype(f: Expr[Any], subtype: ??): ?? = {
    import subtype.Underlying as Subtype
    if (Type[Subtype].isJavaEnum && !Type[Subtype].isJavaEnumValue) {
      val valueNames = Type.directChildrenList[Subtype].fold(List.empty[String])(_.map(_._1))
      // Already a precise value (Scala 3): its short name is one of the enum's values - nothing to recover.
      if (valueNames.contains(Type[Subtype].shortName)) subtype
      else
        handledJavaEnumValueName(f, valueNames).fold(subtype) { valueName =>
          implicit val ValueName: Type[String] = Type.StringCodec.toType(valueName)
          ChimneyType.RefinedJavaEnum[Subtype, String].as_??
        }
    } else subtype
  }

  /** The single enum value named by a `(value: Enum.Value.type) => ...` handler. Its singleton is gone from every typed
    * carrier on Scala 2 (`param.tpe` widens the `Enum.Value.type` singleton away to the enum class), so - having
    * confirmed the handler is a one-argument lambda via [[DestructuredExpr]] - the name is recovered from the
    * parameter's `declaredTpe`, which Hearth preserves as the DECLARED (un-widened) `Enum.Value.type` ascription
    * (hearth#341). Its `shortName` is the value name, cross-checked against the enum's real value names.
    */
  private def handledJavaEnumValueName(f: Expr[Any], valueNames: List[String]): Option[String] =
    DestructuredExpr.parseUntyped(UntypedExpr.fromTyped(f)) match {
      case lambda: DestructuredExpr.Lambda if lambda.params.sizeIs == 1 =>
        val declared = lambda.params.head.declaredTpe
        import declared.Underlying as Declared
        Some(Type[Declared].shortName).filter(valueNames.contains)
      case _ => None
    }

  // --- abort-on-error entry points ---

  protected def withPathType[Out](selector: Expr[Any])(use: ??<:[runtime.Path] => Out): Out =
    parsePathType(selector).fold(reportError, use)

  protected def withPathTypes[Out](selector1: Expr[Any], selector2: Expr[Any])(
      use: (??<:[runtime.Path], ??<:[runtime.Path]) => Out
  ): Out =
    (parsePathType(selector1), parsePathType(selector2)) match {
      case (Right(path1), Right(path2)) => use(path1, path2)
      case (Left(error1), Left(error2)) => reportError(s"Invalid selectors:\n$error1\n$error2")
      case (Left(error), _)             => reportError(error)
      case (_, Left(error))             => reportError(error)
    }

  protected def withArgumentListsType[Out](f: Expr[Any])(use: ??<:[runtime.ArgumentLists] => Out): Out =
    parseArgumentListsType(f).fold(reportError, use)

  // --- error messages (tests pin the prefixes up to the colon - keep them byte-identical) ---

  private def prettyExpr(e: Expr[Any]): String = Expr.prettyPrint(e)

  private def prettyNode(node: DestructuredExpr): String = {
    val e: Expr_?? = UntypedExpr.as_??(node.toUntypedExpr)
    Expr.prettyPrint(e.value)
  }

  private def invalidSelectorMessage(selector: Expr[Any]): String =
    s"The path expression has to be a single chain of calls on the original input, got: ${prettyExpr(selector)}"

  private def arbitraryFunctionMessage(node: DestructuredExpr, selector: Expr[Any]): String =
    s"The path expression has to be a single chain of calls on the original input, got operation other than value extraction: ${prettyNode(node)} in: ${prettyExpr(selector)}"

  private def ignoredInputMessage(node: DestructuredExpr, selector: Expr[Any]): String =
    s"The path expression has to be a single chain of calls on the original input, got external identifier: ${prettyNode(node)} in: ${prettyExpr(selector)}"

  private def invalidConstructorMessage(f: Expr[Any]): String = {
    val fType: Expr_?? = UntypedExpr.as_??(UntypedExpr.fromTyped(f))
    s"Expected function, instead got: ${prettyExpr(f)}: ${Type.prettyPrint(using fType.Underlying)}"
  }
}
