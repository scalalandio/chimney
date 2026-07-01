package io.scalaland.chimney.internal.compiletime2.datatypes

import io.scalaland.chimney.internal.compiletime2.ChimneyDefinitions

import scala.collection.immutable.ListMap

/** Hearth-based port of `io.scalaland.chimney.internal.compiletime.datatypes.ProductTypes` - merges the shared trait
  * and both platform implementations into a single cross-quoted source built on Hearth's `Method`/`Parameter` API.
  *
  * The `Product`/`ProductType` API shape is preserved 1:1 with macro-commons so that rule code can be ported
  * mechanically. Semantic judgment calls (macro-commons semantics preserved unless noted):
  *   - predicates (`isPOJO`/`isCaseClass`/...) are re-implemented on Hearth primitives following the macro-commons
  *     Scala 3 platform formulas (macro-commons' own Scala 2/3 platforms diverged slightly - e.g. Scala 2 `isPOJO`
  *     also excluded `isJavaEnum` symbols - we follow the Scala 3 shape as canonical),
  *   - Hearth's `Type[A].methods` already merges class+companion members, dedups val/def duplicates and sorts:
  *     constructor arguments first (by constructor position), then declared members (by position), then the rest
  *     (by name) - which matches macro-commons' argVals ++ bodyVals ++ accessors intent; we re-partition so that
  *     body vals come before plain accessor defs like in macro-commons,
  *   - `Getter.get`/`Constructor.constructor` are built with Hearth's `Method` builder chain
  *     (`OnInstance`/`ApplyValues`/`Result.build()`) instead of hand-written per-platform trees,
  *   - `exprAsInstanceOfMethod` (powers `.withConstructor` DSL) is implemented by building `FunctionN[...] => ...`
  *     types with the untyped API, casting the expr to them (runtime `.asInstanceOf`) and applying the `apply`
  *     methods via the `Method` chain (arguments re-keyed positionally to `v1..vN`),
  *   - named tuples (Scala 3.7+): construction goes through Hearth's `NamedTuple` view; extraction uses
  *     `productElement(idx)` + cast for ALL arities where macro-commons used `_N` selection below 23 fields
  *     (semantically identical, slightly less direct bytecode). TODO(hearth-migration): validate with Scala 3.7 tests
  *     once rules are ported.
  *   - `ProductTypeOps`/`SealedHierarchyOps` implicit classes are NOT ported: Hearth's built-in `TypeMethods` already
  *     provides `tpe.isCaseClass`/`tpe.isCaseObject`/`tpe.isJavaBean`/`tpe.isSealed` (with slightly different, Hearth
  *     semantics) and a second implicit class with the same member names would make call sites ambiguous. Ported
  *     rules should call `ProductType.isX(tpe)` explicitly when macro-commons semantics matter (no shared-code rule
  *     uses the ops syntax today - verified by grep).
  */
private[compiletime2] trait ProductTypes { this: ChimneyDefinitions & hearth.MacroCommons =>

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

  protected object ProductType {

    private[datatypes] lazy val AnyType: Type[Any] = Type.of[Any]
    private lazy val StringType: Type[String] = Type.of[String]
    private lazy val BooleanType: Type[Boolean] = Type.of[Boolean]
    // Wildcards inside cross-quotes `Type.of` are only safe in members without type parameters (Hearth 0.4.0 bug,
    // see MacroCommonsCompat.reapplyLeadingTypeArgsCompat) - this lazy val has none, so it is fine.
    private lazy val JavaLangEnumType: Type[java.lang.Enum[?]] = Type.of[java.lang.Enum[?]]

    /** macro-commons `Type.platformSpecific.publicPrimaryOrOnlyPublicConstructor` counterpart. */
    private[datatypes] def unambiguousConstructorOf[A: Type]: Option[Method] =
      Type[A].primaryConstructor.filter(_.isAvailable(Everywhere)).orElse {
        Type[A].constructors.filter(_.isAvailable(Everywhere)) match {
          case ctor :: Nil => Some(ctor)
          case _           => None
        }
      }

    /** Runs a Hearth `Method` builder chain to completion, providing the instance (if needed) and the arguments.
      *
      * `arguments` are passed to every `ApplyValues` step - each step selects the parameters it needs by name.
      * A `NeedsTypes` step is unexpected (Hearth resolves class type parameters against the - applied - instance
      * type when listing methods/constructors), so it is reported as an error.
      */
    private[datatypes] def invokeMethodChain(
        initial: Method
    )(instance: Option[UntypedExpr], arguments: Product.Arguments): Either[String, Expr_??] = {
      @scala.annotation.tailrec
      def loop(current: Method, instanceLeft: Option[UntypedExpr]): Either[String, Expr_??] = current match {
        case oi: Method.OnInstance =>
          instanceLeft match {
            case Some(i) => loop(oi.applyUntyped(i), None)
            case None    => Left(s"Method ${initial.name} unexpectedly requires an instance")
          }
        case _: Method.ApplyTypes =>
          Left(s"Method ${initial.name} unexpectedly requires explicit type arguments")
        case av: Method.ApplyValues =>
          loop(av.apply(arguments), instanceLeft)
        case r: Method.Result[?] =>
          import r.Returned
          r.build().map(_.as_??)
      }
      loop(initial, instance)
    }

    /** Invokes a nullary instance method (`val`, nullary `def`, `def foo()`, ...) on the given instance. */
    private[datatypes] def invokeNullaryInstanceMethod[A, R](method: Method)(in: Expr[A]): Expr[R] =
      invokeMethodChain(method)(Some(UntypedExpr.fromTyped(in)), Map.empty).fold(
        error => assertionFailed(s"Failed to call ${method.name}: $error"),
        result => result.value.asInstanceOf[Expr[R]]
      )

    private def hasTypeParameters(method: Method): Boolean = method.expectations.exists {
      case MethodExpectation.NeedsTypes(_) => true
      case _                               => false
    }

    /** Any class with a public constructor... explicitly excluding: primitives, String and Java enums */
    def isPOJO[A](implicit A: Type[A]): Boolean =
      !Type.isPrimitive[A] && !(A <:< StringType) && Type.isClass[A] && !Type.isAbstract[A] &&
        unambiguousConstructorOf[A].isDefined

    /** Class defined with "case class" */
    def isCaseClass[A](implicit A: Type[A]): Boolean =
      Type.isCaseClass[A] && !Type.isAbstract[A] && Type[A].primaryConstructor.exists(_.isAvailable(Everywhere))

    /** Class defined with "case object" */
    def isCaseObject[A](implicit A: Type[A]): Boolean =
      Type.isCaseObject[A] && Type.isAvailable[A](Everywhere)

    /** Scala 3 enum's case without parameters (a "val" under the hood, NOT an "object") */
    def isCaseVal[A](implicit A: Type[A]): Boolean =
      Type.isCaseVal[A] && Type.isAvailable[A](Everywhere)

    /** Java enum value - not the abstract enum type, but the concrete enum value */
    def isJavaEnumValue[A](implicit A: Type[A]): Boolean =
      (A <:< JavaLangEnumType) && !Type.isAbstract[A]

    /** Any POJO with a public DEFAULT constructor... and at least 1 setter or var */
    def isJavaBean[A](implicit A: Type[A]): Boolean =
      isPOJO[A] && Type[A].defaultConstructor.exists(_.isAvailable(Everywhere)) && setterCandidatesOf[A].nonEmpty

    private type CachedExtraction[A] = Option[Product.Extraction[A]]
    private val extractionCache = new TypeCache[CachedExtraction]
    def parseExtraction[A: Type]: Option[Product.Extraction[A]] = extractionCache(Type[A])(
      Some(Product.Extraction(NamedTuple.unapply(Type[A]) match {
        case Some(namedTuple) => namedTupleGetters[A](namedTuple)
        case None             => methodGetters[A]
      }))
    )

    private def methodGetters[A: Type]: Product.Getters[A] = {
      val candidates = (Type[A].methods: List[Method]).iterator
        // Instance methods only - Hearth's `methods` also lists companion-object members.
        .collect { case oi: Method.OnInstance => (oi: Method) }
        .filter(_.isAvailable(Everywhere))
        .filter(_.isNullary)
        .filterNot(hasTypeParameters) // remove methods with type parameters
        .filterNot(method => ProductTypes.isGarbageName(method.name.trim))
        .filterNot(method => method.name.endsWith("_=") || method.name.endsWith("_$eq")) // Scala var setters
        .toList

      // macro-commons ordering: constructor arg vals (in ctor order) ++ body vals ++ accessors and getters.
      val ctorParamOrder: Map[String, Int] = Type[A].primaryConstructor.fold(Map.empty[String, Int]) { ctor =>
        ctor.totalParameters.flatten.map(_._1.trim).zipWithIndex.toMap
      }
      val (argVals, rest) = candidates.partition(_.isConstructorArgument)
      val (bodyVals, accessorsAndGetters) = rest.partition(m => m.isVal || m.isVar || m.isLazy)
      val sortedArgVals = argVals.sortBy(m => ctorParamOrder.getOrElse(m.name.trim, Int.MaxValue))

      // Hearth may expose the same member twice (e.g. val + its accessor def) - keep the first occurrence.
      val seen = scala.collection.mutable.Set.empty[String]
      val deduplicated = (sortedArgVals ++ bodyVals ++ accessorsAndGetters).filter(m => seen.add(m.name.trim))

      ListMap.from(deduplicated.map { method =>
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
              else if (method.isVal || method.isVar || method.isLazy) Product.Getter.SourceType.ConstructorBodyVal
              else Product.Getter.SourceType.AccessorMethod,
            isInherited = method.isInherited,
            get = (in: Expr[A]) => invokeNullaryInstanceMethod[A, Tpe](method)(in)
          )
        )
      })
    }

    private def namedTupleGetters[A: Type](namedTuple: NamedTuple[A]): Product.Getters[A] =
      ListMap.from(namedTuple.fields.zipWithIndex.map { case ((name, tpe), idx) =>
        import tpe.Underlying as Elem
        name -> Existential[Product.Getter[A, *], Elem](
          Product.Getter[A, Elem](
            sourceType = Product.Getter.SourceType.ConstructorArgVal,
            isInherited = false,
            get = (in: Expr[A]) => productElementExpr[A, Elem](in, idx)
          )
        )
      })

    // Named tuples are plain tuples at runtime - unlike macro-commons we use `productElement` for every arity,
    // not just above 22 (semantically identical, avoids platform-specific `_N` selection trees).
    private def productElementExpr[A: Type, Elem: Type](in: Expr[A], idx: Int): Expr[Elem] = {
      val idxExpr = Expr.IntExprCodec.toExpr(idx)
      Expr.quote {
        Expr.splice(in).asInstanceOf[scala.Product].productElement(Expr.splice(idxExpr)).asInstanceOf[Elem]
      }
    }

    private def setterCandidatesOf[A: Type]: List[(String, Method)] = {
      val seen = scala.collection.mutable.Set.empty[String]
      (Type[A].methods: List[Method]).iterator
        .collect { case oi: Method.OnInstance => (oi: Method) }
        .filter(_.isAvailable(Everywhere))
        .filter(_.isUnary)
        .filterNot(hasTypeParameters)
        .filterNot(method => ProductTypes.isGarbageName(method.name.trim))
        .filter { method =>
          val n = method.name.trim
          ProductTypes.BeanAware.isSetterName(n) || n.endsWith("_=") || n.endsWith("_$eq")
        }
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
    private val constructorCache = new TypeCache[CachedConstructor]
    def parseConstructor[A: Type]: Option[Product.Constructor[A]] = constructorCache(Type[A]) {
      if (isCaseObject[A] || isCaseVal[A] || isJavaEnumValue[A]) {
        val singleton = SingletonValue.unapply(Type[A]).getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Expected ${Type.prettyPrint[A]} to be a singleton (case object/case val/Java enum value)")
          // $COVERAGE-ON$
        }
        Some(Product.Constructor[A](ListMap.empty, _ => singleton.singletonExpr))
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
            invokeMethodChain(setter)(Some(UntypedExpr.fromTyped(exprA)), Map(realParamName -> exprArg)).fold(
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

          def newExpr: Expr[A] = invokeMethodChain(unambiguousConstructor)(None, constructorArguments).fold(
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
        NamedTuple.unapply(Type[A]).map { namedTuple =>
          val ctor = namedTuple.primaryConstructor
          val parameters: Product.Parameters = ListMap.from(ctor.totalParameters.flatten.map { case (name, param) =>
            import param.tpe.Underlying as ParamType
            name -> Existential[Product.Parameter, ParamType](
              Product.Parameter[ParamType](Product.Parameter.TargetType.ConstructorParameter, defaultValue = None)
            )
          })
          val constructor: Product.Arguments => Expr[A] = arguments => {
            val (constructorArguments, _) = checkArguments[A](parameters, arguments)
            invokeMethodChain(ctor)(None, constructorArguments).fold(
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
          invokeMethodChain(defaultMethod)(None, Map.empty).fold(
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
          assertionFailed(s"Expected that ${Type.prettyPrint[A]}'s constructor parameter `$name` would have default value")
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
          val fnCtorUntyped = fnUntypedByArity.getOrElse(
            paramList.size,
            // TODO: handle FunctionXXL
            // $COVERAGE-OFF$should never happen unless we messed up
            assertionFailed(s"Expected arity between 0 and 22 into ${Type.prettyPrint[A]}, got: ${paramList.size}")
            // $COVERAGE-ON$
          )
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
      val applyMethod = (Type[Fn].methods: List[Method]).collectFirst {
        case oi: Method.OnInstance if oi.name == "apply" && oi.isNAry(arguments.size) => (oi: Method)
      }.getOrElse {
        // $COVERAGE-OFF$should never happen unless we messed up
        assertionFailed(s"Expected ${Type.prettyPrint[Fn]} to have an apply method of arity ${arguments.size}")
        // $COVERAGE-ON$
      }
      val paramNames = applyMethod.totalParameters.flatten.map(_._1)
      invokeMethodChain(applyMethod)(Some(UntypedExpr.fromTyped(fnExpr)), paramNames.zip(arguments).toMap).fold(
        error =>
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Failed to apply ${Type.prettyPrint[Fn]}: $error"),
        // $COVERAGE-ON$
        identity
      )
    }

    // The whole applied `Type.of[FunctionN[Any, ..., Any]]` only serves as a way to obtain the untyped FunctionN
    // type constructor in shared code (mirrors macro-commons' per-platform `TypeRepr.of[FunctionN]` table).
    private lazy val fnUntypedByArity: Map[Int, UntypedType] = Map(
      0 -> UntypedType.typeConstructor(Type.of[scala.Function0[Any]].asUntyped),
      1 -> UntypedType.typeConstructor(Type.of[scala.Function1[Any, Any]].asUntyped),
      2 -> UntypedType.typeConstructor(Type.of[scala.Function2[Any, Any, Any]].asUntyped),
      3 -> UntypedType.typeConstructor(Type.of[scala.Function3[Any, Any, Any, Any]].asUntyped),
      4 -> UntypedType.typeConstructor(Type.of[scala.Function4[Any, Any, Any, Any, Any]].asUntyped),
      5 -> UntypedType.typeConstructor(Type.of[scala.Function5[Any, Any, Any, Any, Any, Any]].asUntyped),
      6 -> UntypedType.typeConstructor(Type.of[scala.Function6[Any, Any, Any, Any, Any, Any, Any]].asUntyped),
      7 -> UntypedType.typeConstructor(Type.of[scala.Function7[Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped),
      8 -> UntypedType.typeConstructor(
        Type.of[scala.Function8[Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      9 -> UntypedType.typeConstructor(
        Type.of[scala.Function9[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      10 -> UntypedType.typeConstructor(
        Type.of[scala.Function10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      11 -> UntypedType.typeConstructor(
        Type.of[scala.Function11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      12 -> UntypedType.typeConstructor(
        Type.of[scala.Function12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      13 -> UntypedType.typeConstructor(
        Type.of[scala.Function13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      14 -> UntypedType.typeConstructor(
        Type.of[scala.Function14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]].asUntyped
      ),
      15 -> UntypedType.typeConstructor(
        Type
          .of[scala.Function15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]]
          .asUntyped
      ),
      16 -> UntypedType.typeConstructor(
        Type
          .of[scala.Function16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]]
          .asUntyped
      ),
      17 -> UntypedType.typeConstructor(
        Type
          .of[
            scala.Function17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]
          ]
          .asUntyped
      ),
      18 -> UntypedType.typeConstructor(
        Type
          .of[
            scala.Function18[
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any
            ]
          ]
          .asUntyped
      ),
      19 -> UntypedType.typeConstructor(
        Type
          .of[
            scala.Function19[
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any
            ]
          ]
          .asUntyped
      ),
      20 -> UntypedType.typeConstructor(
        Type
          .of[
            scala.Function20[
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any
            ]
          ]
          .asUntyped
      ),
      21 -> UntypedType.typeConstructor(
        Type
          .of[
            scala.Function21[
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any
            ]
          ]
          .asUntyped
      ),
      22 -> UntypedType.typeConstructor(
        Type
          .of[
            scala.Function22[
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any
            ]
          ]
          .asUntyped
      )
    )

    // defaults methods are 1-indexed
    protected def classNewDefaultScala2(idx: Int): String = "<init>$default$" + idx
    protected def caseClassApplyDefaultScala2(idx: Int): String = "apply$default$" + idx
    protected def caseClassApplyDefaultScala3(idx: Int): String = "$lessinit$greater$default$" + idx

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
                  .prettyPrint[param.Underlying]}, instead got ${Expr.prettyPrint(argument.value)} ${Type.prettyPrint(using argument.Underlying)}"
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
