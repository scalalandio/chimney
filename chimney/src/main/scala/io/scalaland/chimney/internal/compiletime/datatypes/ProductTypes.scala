package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

import scala.collection.immutable.ListMap

/** `Product`/`ProductType` view - thin layer over Hearth's `Method`/`Parameter` API and `SingletonValue`.
  *
  * Chimney's `Product` is a superset of Hearth's `CaseClass`/`JavaBean` views: one `Constructor` mixes primary-ctor
  * arguments WITH Java Bean setters on the same type, and one `Getters` map classifies case fields, body vals, plain
  * accessors and bean getters together (the RULES decide, flag-driven, which of them to use) - hence the assembly stays
  * here while every ingredient (method listing, classification, invocation via `Method.fold`, singleton construction
  * via `SingletonValue`) is Hearth's.
  */
private[compiletime] trait ProductTypes { this: ChimneyDefinitions & hearth.MacroCommons =>

  /** Describes all types which could be considered products in a very loose way.
    *
    * For type to be considered "product" it has to be:
    *   - non abstract
    *   - have a public (primary) constructor
    *
    * If it's a "product" then we are able to provide both a way to construct it as well as a way to extract its
    * properties. This is rather unrestricted since:
    *   - our "constructor" allows passing arguments to Java Bean setters
    *   - our properties include: `def`s without arguments, Java Bean getters and it's the code using the extractors and
    *     constructors that should check the type of getter/constructor argument.
    *
    * In case we don't need a "product" per se, but rather any instantiable type to instantiate or any type to obtain
    * its methods, we can use `unapply` from `Extraction` or `Construction`.
    */
  final protected case class Product[A](extraction: Product.Extraction[A], construction: Product.Constructor[A])
  protected object Product {

    final case class Getter[From, A](sourceType: Getter.SourceType, isInherited: Boolean, get: Expr[From] => Expr[A])
    object Getter {

      /** Let us decide whether or now we can use the getter based on configuration */
      sealed trait SourceType extends scala.Product with Serializable
      object SourceType {

        /** `val`/`var` initialized by constructor as a parameter */
        case object ConstructorArgVal extends SourceType

        /** `val`/`lazy val`/`var` initialized by constructor in the body */
        case object ConstructorBodyVal extends SourceType

        /** `def` without parameters which cannot be treated as Java Bean getter */
        case object AccessorMethod extends SourceType

        /** `def` without parameters which name starts with `get` or `is` if it returns `Boolean` */
        case object JavaBeanGetter extends SourceType
      }
    }
    final type Getters[From] = ListMap[String, Existential[Getter[From, *]]]

    /** Let us obtain a list of: vals, lazy vals and parameterless defs that we can always call. */
    final case class Extraction[From](extraction: Getters[From])
    object Extraction {
      def unapply[From](From: Type[From]): Option[Getters[From]] =
        ProductType.parseExtraction(using From).map(getters => getters.extraction)
    }

    final case class Parameter[A](targetType: Parameter.TargetType, defaultValue: Option[Expr[A]])
    object Parameter {
      sealed trait TargetType extends scala.Product with Serializable
      object TargetType {

        /** When constructing, value will be passed as constructor argument */
        case object ConstructorParameter extends TargetType

        /** When constructing, value will be passed as setter argument */
        final case class SetterParameter(returnedType: ??) extends TargetType {
          override def toString: String = s"SetterParameter(returnedType = ${returnedType.prettyPrint})"
        }
      }
    }
    final type Parameters = ListMap[String, Existential[Parameter]]

    final type Arguments = Map[String, ExistentialExpr]

    /** Let us obtain a list of primary constructor's parameters as well as setter parameters, as well as a method of
      * taking all computed arguments and turning it into constructed value.
      */
    final case class Constructor[To](parameters: Parameters, constructor: Arguments => Expr[To])
    object Constructor {
      def unapply[To](To: Type[To]): Option[(Parameters, Arguments => Expr[To])] =
        ProductType.parseConstructor(using To).map(constructor => constructor.parameters -> constructor.constructor)

      def exprAsInstanceOfMethod[To: Type](args: List[ListMap[String, ??]])(expr: ExistentialExpr): Constructor[To] = {
        implicit val AnyT: Type[Any] = ProductType.AnyType
        import expr.Underlying as ExprType
        ProductType.exprAsInstanceOfMethod[To](args)(castToExpr[ExprType, Any](expr.value))
      }
    }
  }

  /** Named-tuple element getter (Scala 3-only types; never called on Scala 2). `productElement` + cast works for every
    * arity - kept in a helper def with its own `Type` bounds (cross-quotes helper-def pattern).
    */
  private def namedTupleGetter[A: Type, Elem: Type](in: Expr[A], idx: Int): Expr[Elem] = {
    val idxExpr = Expr.IntExprCodec.toExpr(idx)
    Expr.quote {
      Expr.splice(in).asInstanceOf[scala.Product].productElement(Expr.splice(idxExpr)).asInstanceOf[Elem]
    }
  }

  protected object ProductType {

    private[datatypes] lazy val AnyType: Type[Any] = Type.of[Any]
    private lazy val StringType: Type[String] = Type.of[String]
    private lazy val BooleanType: Type[Boolean] = Type.of[Boolean]

    /** The public primary constructor, or - failing that - the only public constructor. */
    private[datatypes] def unambiguousConstructorOf[A: Type]: Option[Method] =
      Type[A].primaryConstructor.filter(_.isAvailable(Everywhere)).orElse {
        Type[A].constructors.filter(_.isAvailable(Everywhere)) match {
          case ctor :: Nil => Some(ctor)
          case _           => None
        }
      }

    /** Runs a Hearth `Method` to completion via `Method.fold`, providing the instance (if needed) and the arguments
      * (each `ApplyValues` step selects the parameters it needs by name). Callers pre-filter methods with type
      * parameters, so a `NeedsTypes` step is a bug.
      */
    private[datatypes] def invoke(
        method: Method
    )(instance: Option[Expr_??], arguments: Product.Arguments): Either[String, Expr_??] =
      method.fold(
        onInstance =
          _ => instance.getOrElse(assertionFailed(s"Method ${method.name} unexpectedly requires an instance")),
        onTypes = _ => assertionFailed(s"Method ${method.name} unexpectedly requires explicit type arguments"),
        onValues = _ => arguments
      )

    /** Invokes a nullary instance method (`val`, nullary `def`, `def foo()`, ...) on the given instance. */
    private[datatypes] def invokeNullaryInstanceMethod[A: Type, R](method: Method)(in: Expr[A]): Expr[R] =
      invoke(method)(Some(in.as_??), Map.empty).fold(
        error => assertionFailed(s"Failed to call ${method.name}: $error"),
        result => result.value.asInstanceOf[Expr[R]]
      )

    private def hasTypeParameters(method: Method): Boolean = method.expectations.exists {
      case MethodExpectation.NeedsTypes(_) => true
      case _                               => false
    }

    /** Dealiases the type before symbol-based Hearth lookups - product parsing is deliberately alias-TRANSPARENT.
      *
      * NO LONGER a hearth#315 workaround: since 0.4.0-16-gd4adc1c Hearth's `Type.isClass`/`Type.isAbstract` classify
      * `export`-created aliases by their underlying class (verified by probe, see the it.30 comment on hearth#315), so
      * the original justification (issue 758's `export Inner.Foo` failing `isPOJO`) is gone. The dealias stays because
      * OTHER aliases still need it: e.g. Scala 3 `NamedTuple[...]` types reached through override paths
      * (`withFieldConst(_.bar.matchingSome.baz, ...)`) arrive in alias form and fail to parse as products without it
      * (16 NamedTuple-spec regressions when removed against 0.4.0-16-gd4adc1c-SNAPSHOT). Scala 2's `typeSymbol`
      * auto-dealiases, so this is a no-op there.
      */
    private def dealiasedType[A](A: Type[A]): Type[A] =
      UntypedType.toTyped[A](UntypedType.dealias(UntypedType.fromTyped[A](using A)))

    /** Any class with a public constructor... explicitly excluding: primitives (incl. `Unit`), String and Java enums.
      */
    def isPOJO[A](implicit A0: Type[A]): Boolean = isPOJOImpl(dealiasedType(A0))
    private def isPOJOImpl[A](implicit A: Type[A]): Boolean =
      !Type.isPrimitive[A] && !(A <:< StringType) && Type.isClass[A] && !Type.isAbstract[A] &&
        unambiguousConstructorOf[A].isDefined

    /** Publicly referencable singleton value (case object, plain object, parameterless Scala 3 enum case, Java enum
      * value, Scala Enumeration value, stable term ref) - Hearth's `SingletonValue` semantics, gated on availability so
      * that the generated reference compiles at every expansion site.
      */
    private[datatypes] def parseSingleton[A](implicit A0: Type[A]): Option[SingletonValue[A]] =
      parseSingletonImpl(dealiasedType(A0))
    private def parseSingletonImpl[A](implicit A: Type[A]): Option[SingletonValue[A]] =
      SingletonValue.unapply(A).filter(_ => Type.isAvailable[A](Everywhere))

    private type CachedExtraction[A] = Option[Product.Extraction[A]]
    private val extractionCache = new Type.Cache[CachedExtraction]
    def parseExtraction[A](implicit A0: Type[A]): Option[Product.Extraction[A]] =
      extractionCache.getOrPut(A0)(parseExtractionImpl(dealiasedType(A0)))
    private def parseExtractionImpl[A](implicit A: Type[A]): Option[Product.Extraction[A]] =
      Some(Product.Extraction(NamedTuple.unapply(Type[A]) match {
        case Some(namedTuple) => namedTupleGetters[A](namedTuple)
        case None             => methodGetters[A]
      }))

    private def methodGetters[A: Type]: Product.Getters[A] = {
      // Filter-first pattern: `unsortedMethods` skips Hearth's expensive position-resolving sort, and `Method.sort`
      // then restores the exact same stable order on the small filtered subset (its sort key is per-method, so
      // filter-then-sort == sort-then-filter), paying position resolution only for the methods that survived.
      val candidates = Method.sort(
        (Type[A].unsortedMethods: List[Method]).iterator
          // Instance methods only - Hearth's `unsortedMethods` also lists companion-object members.
          .collect { case oi: Method.OnInstance => (oi: Method) }
          .filter(_.isAvailable(Everywhere))
          .filter(_.isNullary)
          .filterNot(hasTypeParameters) // remove methods with type parameters
          .filterNot(method => ProductTypes.isGarbageName(method.name.trim))
          .filterNot(method => method.name.endsWith("_=") || method.name.endsWith("_$eq")) // Scala var setters
          .toList
      )

      // Getter ordering: constructor arg vals (in ctor order) ++ body vals ++ accessors and getters.
      val ctorParamOrder: Map[String, Int] = Type[A].primaryConstructor.fold(Map.empty[String, Int]) { ctor =>
        ctor.totalParameters.flatten.map(_._1.trim).zipWithIndex.toMap
      }
      def isBodyVal(m: Method): Boolean = m.isVal || m.isVar || m.isLazy
      val (argVals, rest) = candidates.partition(_.isConstructorArgument)
      val (bodyVals, accessorsAndGetters) = rest.partition(isBodyVal)
      val sortedArgVals = argVals.sortBy(m => ctorParamOrder.getOrElse(m.name.trim, Int.MaxValue))

      // Hearth may expose the same member twice (e.g. val + its accessor def) - keep the first occurrence.
      val seen = scala.collection.mutable.Set.empty[String]
      val deduplicated = (sortedArgVals ++ bodyVals ++ accessorsAndGetters).filter(m => seen.add(m.name.trim))

      val methodBasedGetters = deduplicated.map { method =>
        val name = method.name.trim
        val returned: ?? = method.knownReturning.getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Expected known return type of ${Type.prettyPrint[A]}'s member $name")
          // $COVERAGE-ON$
        }
        import returned.Underlying as Tpe
        def conformToIsGetters = !name.take(2).equalsIgnoreCase("is") || (Tpe <:< BooleanType)
        name -> Existential[Product.Getter[A, *], Tpe](
          Product.Getter[A, Tpe](
            sourceType =
              if (method.isConstructorArgument) Product.Getter.SourceType.ConstructorArgVal
              else if (ProductTypes.BeanAware.isGetterName(name) && conformToIsGetters)
                Product.Getter.SourceType.JavaBeanGetter
              else if (isBodyVal(method)) Product.Getter.SourceType.ConstructorBodyVal
              else Product.Getter.SourceType.AccessorMethod,
            isInherited = method.isInherited,
            get = (in: Expr[A]) => invokeNullaryInstanceMethod[A, Tpe](method)(in)
          )
        )
      }

      ListMap.from(methodBasedGetters)
    }

    private def namedTupleGetters[A: Type](namedTuple: NamedTuple[A]): Product.Getters[A] =
      ListMap.from(namedTuple.fields.zipWithIndex.map { case ((name, tpe), idx) =>
        import tpe.Underlying as Elem
        name -> Existential[Product.Getter[A, *], Elem](
          Product.Getter[A, Elem](
            sourceType = Product.Getter.SourceType.ConstructorArgVal,
            isInherited = false,
            get = (in: Expr[A]) => namedTupleGetter[A, Elem](in, idx)
          )
        )
      })

    private def setterCandidatesOf[A: Type]: List[(String, Method)] = {
      val seen = scala.collection.mutable.Set.empty[String]
      // Filter-first pattern (see `methodGetters`): setter order is user-observable (it is the order the generated
      // code calls the setters in, and the order they are reported in), so the filtered subset is re-sorted into
      // `methods`' stable declaration order.
      val candidates = Method.sort(
        (Type[A].unsortedMethods: List[Method]).iterator
          .collect { case oi: Method.OnInstance => (oi: Method) }
          .filter(_.isAvailable(Everywhere))
          .filter(_.isUnary)
          .filterNot(hasTypeParameters)
          .filterNot(method => ProductTypes.isGarbageName(method.name.trim))
          .filter { method =>
            val n = method.name.trim
            ProductTypes.BeanAware.isSetterName(n) || n.endsWith("_=") || n.endsWith("_$eq")
          }
          .toList
      )
      candidates.iterator
        .map { method =>
          // Scala 3's JB setters _are_ methods ending with _= due to change in @BeanProperty behavior.
          // We have to drop that suffix to align names, so that comparing is possible.
          val n = method.name.trim
          val name = n.stripSuffix("_$eq").stripSuffix("_=")
          name -> method
        }
        .filter { case (name, _) => seen.add(name) }
        .toList
    }

    private type CachedConstructor[A] = Option[Product.Constructor[A]]
    private val constructorCache = new Type.Cache[CachedConstructor]
    def parseConstructor[A](implicit A0: Type[A]): Option[Product.Constructor[A]] =
      // dealiasedType: alias-transparent parsing (e.g. NamedTuple aliases at override-path positions - see above).
      constructorCache.getOrPut(A0)(parseConstructorImpl(dealiasedType(A0)))
    private def parseConstructorImpl[A](implicit A: Type[A]): Option[Product.Constructor[A]] = {
      val singleton = parseSingleton[A]
      if (singleton.isDefined) {
        singleton.map(s => Product.Constructor[A](ListMap.empty, _ => s.singletonExpr))
      } else if (isPOJO[A]) {
        val unambiguousConstructor = unambiguousConstructorOf[A].getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Expected public constructor of ${Type.prettyPrint[A]}")
          // $COVERAGE-ON$
        }

        val ctorParams: List[(String, Parameter)] = unambiguousConstructor.totalParameters.flatten
        val ctorParamNames: Set[String] = ctorParams.map(_._1).toSet

        val constructorParameters: Product.Parameters = ListMap.from(ctorParams.map { case (name, param) =>
          import param.tpe.Underlying as ParamType
          name -> Existential[Product.Parameter, ParamType](
            Product.Parameter[ParamType](
              Product.Parameter.TargetType.ConstructorParameter,
              if (param.hasDefault) Some(parameterDefaultValue[A, ParamType](name, param)) else None
            )
          )
        })

        val setters: List[(String, Method, Existential[Product.Parameter])] =
          setterCandidatesOf[A]
            .filter { case (name, _) => !ctorParamNames.contains(name) } // _exact_ name match!
            .map { case (name, setter) =>
              val (_, param) = setter.totalParameters.flatten.head
              import param.tpe.Underlying as ParamType
              val returned: ?? = setter.knownReturning.getOrElse {
                // $COVERAGE-OFF$should never happen unless we messed up
                assertionFailed(s"Expected known return type of ${Type.prettyPrint[A]}'s setter $name")
                // $COVERAGE-ON$
              }
              (
                name,
                setter,
                Existential[Product.Parameter, ParamType](
                  Product.Parameter[ParamType](
                    targetType = Product.Parameter.TargetType.SetterParameter(returned),
                    defaultValue = None
                  )
                )
              )
            }

        val setterParameters: Product.Parameters = ListMap.from(setters.map { case (name, _, param) => name -> param })
        val setterCalls: Map[String, (Expr[A], ExistentialExpr) => Expr[Unit]] = setters.map { case (name, setter, _) =>
          val realParamName = setter.totalParameters.flatten.head._1
          name -> { (exprA: Expr[A], exprArg: ExistentialExpr) =>
            invoke(setter)(Some(exprA.as_??), Map(realParamName -> exprArg)).fold(
              error => assertionFailed(s"Failed to call setter $name of ${Type.prettyPrint[A]}: $error"),
              result => {
                import result.{Underlying as Returned, value as callExpr}
                discardToUnitExpr[Returned](callExpr)
              }
            )
          }
        }.toMap

        val parameters: Product.Parameters = constructorParameters ++ setterParameters

        val constructor: Product.Arguments => Expr[A] = arguments => {
          val (constructorArguments, setterArguments) = checkArguments[A](parameters, arguments)

          def newExpr: Expr[A] = invoke(unambiguousConstructor)(None, constructorArguments).fold(
            error => assertionFailed(s"Failed to call constructor of ${Type.prettyPrint[A]}: $error"),
            result => result.value.asInstanceOf[Expr[A]]
          )

          if (setterArguments.isEmpty) {
            newExpr
          } else {
            ValDefs.createVal[A](newExpr, FreshName.FromType).use { exprA =>
              setterArguments.toList.foldRight(exprA) { case ((name, exprArg), acc) =>
                val call = setterCalls(name)(exprA, exprArg)
                Expr.quote {
                  Expr.splice(call)
                  Expr.splice(acc)
                }
              }
            }
          }
        }

        Some(Product.Constructor(parameters, constructor))
      } else if (Type.isNamedTuple[A]) {
        NamedTuple
          .unapply(Type[A])
          .map { namedTuple =>
            val ctor = namedTuple.primaryConstructor
            val parameters: Product.Parameters = ListMap.from(ctor.totalParameters.flatten.map { case (name, param) =>
              import param.tpe.Underlying as ParamType
              name -> Existential[Product.Parameter, ParamType](
                Product.Parameter[ParamType](Product.Parameter.TargetType.ConstructorParameter, defaultValue = None)
              )
            })
            val constructor: Product.Arguments => Expr[A] = arguments => {
              val (constructorArguments, _) = checkArguments[A](parameters, arguments)
              invoke(ctor)(None, constructorArguments).fold(
                error => assertionFailed(s"Failed to construct named tuple ${Type.prettyPrint[A]}: $error"),
                result => result.value.asInstanceOf[Expr[A]]
              )
            }
            Product.Constructor(parameters, constructor)
          }
      } else None
    }

    final def parse[A: Type]: Option[Product[A]] = parseExtraction[A].zip(parseConstructor[A]).headOption.map {
      case (getters, constructor) => Product(getters, constructor)
    }
    final def unapply[A](tpe: Type[A]): Option[Product[A]] = parse(using tpe)

    private def parameterDefaultValue[A: Type, ParamType: Type](name: String, param: Parameter): Expr[ParamType] =
      param.defaultValue
        .map { defaultMethod =>
          invoke(defaultMethod)(None, Map.empty).fold(
            error =>
              // $COVERAGE-OFF$should never happen unless we messed up
              assertionFailed(
                s"Expected that ${Type.prettyPrint[A]}'s constructor parameter `$name` would have an obtainable default value: $error"
              ),
            // $COVERAGE-ON$
            result => result.value.asInstanceOf[Expr[ParamType]]
          )
        }
        .getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(
            s"Expected that ${Type.prettyPrint[A]}'s constructor parameter `$name` would have default value"
          )
          // $COVERAGE-ON$
        }

    private def discardToUnitExpr[R: Type](expr: Expr[R]): Expr[Unit] =
      Expr.quote {
        Expr.splice(expr)
        ()
      }

    def exprAsInstanceOfMethod[A: Type](args: List[ListMap[String, ??]])(expr: Expr[Any]): Product.Constructor[A] = {
      val parameters: Product.Parameters = ListMap.from(for {
        list <- args
        pair <- list.toList
        (paramName, paramType) = pair
      } yield {
        import paramType.Underlying as ParamType
        paramName -> Existential[Product.Parameter, ParamType](
          Product.Parameter(Product.Parameter.TargetType.ConstructorParameter, None)
        )
      })

      val constructor: Product.Arguments => Expr[A] = arguments => {
        val (constructorArguments, _) = checkArguments[A](parameters, arguments)

        val methodType: ?? = args.foldRight[??](Type[A].as_??) { (paramList, resultType) =>
          // TODO: handle FunctionXXL
          if (paramList.sizeIs > 22) {
            // $COVERAGE-OFF$should never happen unless we messed up
            assertionFailed(s"Expected arity between 0 and 22 into ${Type.prettyPrint[A]}, got: ${paramList.size}")
            // $COVERAGE-ON$
          }
          val fnCtorUntyped =
            UntypedType.typeConstructor(UntypedType.fromClassName(s"scala.Function${paramList.size}"))
          val paramTypes = paramList.view.values.map(_.asUntyped).toList
          UntypedType.as_??(UntypedType.applyTypeArgs(fnCtorUntyped, paramTypes :+ resultType.asUntyped))
        }

        import methodType.Underlying as MethodType
        val initial: ExistentialExpr = {
          implicit val Any: Type[Any] = AnyType
          Existential[Expr, MethodType](castToExpr[Any, MethodType](expr))
        }
        val result = args.foldLeft(initial) { (current, paramList) =>
          applyFunctionExpr(current, paramList.toList.map { case (paramName, _) => constructorArguments(paramName) })
        }
        result.value.asInstanceOf[Expr[A]]
      }

      Product.Constructor[A](parameters, constructor)
    }

    private def applyFunctionExpr(fn: ExistentialExpr, arguments: List[ExistentialExpr]): ExistentialExpr = {
      import fn.{Underlying as Fn, value as fnExpr}
      val applyMethod = (Type[Fn].unsortedMethods: List[Method]) // order-independent: unique `apply` by name + arity
        .collectFirst {
          case oi: Method.OnInstance if oi.name == "apply" && oi.isNAry(arguments.size) => (oi: Method)
        }
        .getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Expected ${Type.prettyPrint[Fn]} to have an apply method of arity ${arguments.size}")
          // $COVERAGE-ON$
        }
      val paramNames = applyMethod.totalParameters.flatten.map(_._1)
      invoke(applyMethod)(Some(fnExpr.as_??), paramNames.zip(arguments).toMap).fold(
        error =>
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Failed to apply ${Type.prettyPrint[Fn]}: $error"),
        // $COVERAGE-ON$
        identity
      )
    }

    // skipping on setter should not create a invalid expression, whether or not is should be called depends on caller
    private val settersCanBeIgnored: ((String, Existential[Product.Parameter])) => Boolean =
      _._2.value.targetType == Product.Parameter.TargetType.ConstructorParameter

    protected def checkArguments[A: Type](
        parameters: Product.Parameters,
        arguments: Product.Arguments
    ): (Product.Arguments, Product.Arguments) = {
      val missingArguments = parameters.filter(settersCanBeIgnored).keySet diff arguments.keySet
      if (missingArguments.nonEmpty) {
        // $COVERAGE-OFF$should never happen unless we messed up
        val missing = missingArguments.mkString(", ")
        val provided = arguments.keys.mkString(", ")
        assertionFailed(
          s"Constructor of ${Type.prettyPrint[A]} expected arguments: $missing but they were not provided, what was provided: $provided"
        )
        // $COVERAGE-ON$
      }

      parameters.foreach { case (name, param) =>
        import param.Underlying as Param
        // setter might be absent, so we cannot assume that argument for it is in a map
        arguments.get(name).foreach { argument =>
          if (!(argument.Underlying <:< Param)) {
            // $COVERAGE-OFF$should never happen unless we messed up
            assertionFailed(
              s"Constructor of ${Type.prettyPrint[A]} expected expr for parameter $param of type ${Type
                  .prettyPrint[param.Underlying]}, instead got ${Expr.prettyPrint(argument.value)} ${Type
                  .prettyPrint(using argument.Underlying)}"
            )
            // $COVERAGE-ON$
          }
        }
      }

      val (params, setters) =
        parameters.partition(_._2.value.targetType == Product.Parameter.TargetType.ConstructorParameter)

      val constructorParameters = params.keySet
      val constructorArguments = ListMap.from(arguments.view.filterKeys(constructorParameters))

      val setterParameters = setters.keySet
      val setterArguments = ListMap.from(arguments.view.filterKeys(setterParameters))

      constructorArguments -> setterArguments
    }
  }
}
object ProductTypes {

  object BeanAware {

    implicit private class RegexpOps(regexp: scala.util.matching.Regex) {

      def isMatching(value: String): Boolean = regexp.pattern.matcher(value).matches() // 2.12 doesn't have .matches
    }

    private val getAccessor = raw"(?i)get(.)(.*)".r
    private val isAccessor = raw"(?i)is(.)(.*)".r
    val isGetterName: String => Boolean = name => getAccessor.isMatching(name) || isAccessor.isMatching(name)

    val dropGetIs: String => String = {
      case getAccessor(head, tail) => head.toLowerCase + tail
      case isAccessor(head, tail)  => head.toLowerCase + tail
      case other                   => other
    }

    private val setAccessor = raw"(?i)set(.)(.*)".r
    val isSetterName: String => Boolean = name => setAccessor.isMatching(name)

    val dropSet: String => String = {
      case setAccessor(head, tail) => head.toLowerCase + tail
      case other                   => other
    }
  }

  // methods we can drop from searching scope
  private val garbage = Set(
    // constructor
    "<init>",
    "$init$",
    // case class generated
    "copy",
    // scala.Product methods
    "##",
    "canEqual",
    "productArity",
    "productElement",
    "productElementName",
    "productElementNames",
    "productIterator",
    "productPrefix",
    // java.lang.Object methods
    "equals",
    "finalize",
    "hashCode",
    "toString",
    "clone",
    "synchronized",
    "wait",
    "notify",
    "notifyAll",
    "getClass",
    "asInstanceOf",
    "isInstanceOf"
  )
  // default arguments has name method$default$index
  private val defaultElement = raw"$$default$$"
  val isGarbageName: String => Boolean = name => garbage(name) || name.contains(defaultElement)
}
