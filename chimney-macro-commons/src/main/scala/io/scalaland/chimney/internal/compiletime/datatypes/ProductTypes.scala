package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.Definitions

import scala.collection.compat.*
import scala.collection.immutable.ListMap

trait ProductTypes { this: Definitions =>

  /** Describes all types which could be considered products in a very loose way.
    *
    * For type to be considered "product" it has to be:
    * - non abstract
    * - have a public (primary) constructor
    *
    * If it's a "product" then we are able to provide both a way to construct it as well as a way to extract its
    * properties. This is rather unrestricted since:
    * - our "constructor" allows passing arguments to Java Bean setters
    * - our properties include: `def`s without arguments, Java Bean getters
    * and it's the code using the extractors and constructors that should check the type of getter/constructor argument.
    *
    * In case we don't need a "product" per se, but rather any instantiable type to instantiate or any type to obtain
    * its methods, we can use `unapply` from `Extraction` or `Construction`.
    */
  final protected case class Product[A](extraction: Product.Extraction[A], construction: Product.Constructor[A])
  protected object Product {

    final case class Getter[From, A](sourceType: Getter.SourceType, isLocal: Boolean, get: Expr[From] => Expr[A])
    object Getter {

      /** Let us decide whether or now we can use the getter based on configuration */
      sealed trait SourceType extends scala.Product with Serializable
      object SourceType {

        /** `val`/`lazy val` initialized by constructor - either as val parameter or in body */
        case object ConstructorVal extends SourceType

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
        ProductType.parseExtraction(From).map(getters => getters.extraction)
    }

    final case class Parameter[A](targetType: Parameter.TargetType, defaultValue: Option[Expr[A]])
    object Parameter {
      sealed trait TargetType extends scala.Product with Serializable
      object TargetType {

        /** When constructing, value will be passed as constructor argument */
        case object ConstructorParameter extends TargetType

        /** When constructing, value will be passed as setter argument */
        case object SetterParameter extends TargetType
      }
    }
    final type Parameters = ListMap[String, Existential[Parameter]]

    final type Arguments = Map[String, ExistentialExpr]

    /** Let us obtain a list of primary constructor's parameters as well as setter parameters, as well as a method
      * of taking all computed arguments and turning it into constructed value.
      */
    final case class Constructor[To](parameters: Parameters, constructor: Arguments => Expr[To])
    object Constructor {
      def unapply[To](To: Type[To]): Option[(Parameters, Arguments => Expr[To])] =
        ProductType.parseConstructor(To).map(constructor => constructor.parameters -> constructor.constructor)

      def exprAsInstanceOfMethod[To: Type](args: List[ListMap[String, ??]])(expr: Expr[Any]): Constructor[To] =
        ProductType.exprAsInstanceOfMethod[To](args)(expr)
    }
  }

  protected val ProductType: ProductTypesModule
  protected trait ProductTypesModule { this: ProductType.type =>

    def isPOJO[A](implicit A: Type[A]): Boolean
    def isCaseClass[A](implicit A: Type[A]): Boolean
    def isCaseObject[A](implicit A: Type[A]): Boolean
    def isJavaBean[A](implicit A: Type[A]): Boolean

    def parseExtraction[A: Type]: Option[Product.Extraction[A]]
    def parseConstructor[A: Type]: Option[Product.Constructor[A]]
    final def parse[A: Type]: Option[Product[A]] = parseExtraction[A].zip(parseConstructor[A]).headOption.map {
      case (getters, constructor) => Product(getters, constructor)
    }
    final def unapply[A](tpe: Type[A]): Option[Product[A]] = parse(tpe)

    def exprAsInstanceOfMethod[A: Type](args: List[ListMap[String, ??]])(expr: Expr[Any]): Product.Constructor[A]

    // cached in companion (regexps are expensive to initialize)
    def areNamesMatching(fromName: String, toName: String): Boolean = ProductTypes.areNamesMatching(fromName, toName)
    def isGarbage(name: String): Boolean = ProductTypes.isGarbage(name)
    def isGetterName(name: String): Boolean = ProductTypes.isGetterName(name)
    def isSetterName(name: String): Boolean = ProductTypes.isSetterName(name)
    def dropGetIs(name: String): String = ProductTypes.dropGetIs(name)
    def dropSet(name: String): String = ProductTypes.dropSet(name)
    def normalize(name: String): String = ProductTypes.normalize(name)

    // defaults methods are 1-indexed
    protected def caseClassApplyDefaultScala2(idx: Int): String = "apply$default$" + idx
    protected def caseClassApplyDefaultScala3(idx: Int): String = "$lessinit$greater$default$" + idx

    // skipping on setter should not create a invalid expression, whether or not is should be called depends on caller
    private val settersCanBeIgnored: ((String, Existential[Product.Parameter])) => Boolean =
      _._2.value.targetType != Product.Parameter.TargetType.SetterParameter

    protected def checkArguments[A: Type](
        parameters: Product.Parameters,
        arguments: Product.Arguments
    ): (Product.Arguments, Product.Arguments) = {
      val missingArguments = parameters.filter(settersCanBeIgnored).keySet diff arguments.keySet
      if (missingArguments.nonEmpty) {
        val missing = missingArguments.mkString(", ")
        val provided = arguments.keys.mkString(", ")
        assertionFailed(
          s"Constructor of ${Type.prettyPrint[A]} expected arguments: $missing but they were not provided, what was provided: $provided"
        )
      }

      parameters.foreach { case (name, param) =>
        import param.Underlying as Param
        // setter might be absent, so we cannot assume that argument for it is in a map
        arguments.get(name).foreach { argument =>
          if (!(argument.Underlying <:< Param)) {
            assertionFailed(
              s"Constructor of ${Type.prettyPrint[A]} expected expr for parameter $param of type ${Type
                  .prettyPrint[param.Underlying]}, instead got ${Expr.prettyPrint(argument.value)} ${Type.prettyPrint(argument.Underlying)}"
            )
          }
        }
      }

      val constructorParameters =
        parameters.filter(_._2.value.targetType == Product.Parameter.TargetType.ConstructorParameter).keySet
      val constructorArguments = ListMap.from(arguments.view.filterKeys(constructorParameters))

      val setterParameters =
        parameters.filter(_._2.value.targetType == Product.Parameter.TargetType.SetterParameter).keySet
      val setterArguments = ListMap.from(arguments.view.filterKeys(setterParameters))

      constructorArguments -> setterArguments
    }
  }

  implicit class ProductTypeOps[A](private val tpe: Type[A]) {

    def isCaseClass: Boolean = ProductType.isCaseClass(tpe)
    def isCaseObject: Boolean = ProductType.isCaseObject(tpe)
    def isJavaBean: Boolean = ProductType.isJavaBean(tpe)
    def isPOJO: Boolean = ProductType.isPOJO(tpe)
  }
}
object ProductTypes {

  implicit private class RegexpOps(regexp: scala.util.matching.Regex) {

    def isMatching(value: String): Boolean = regexp.pattern.matcher(value).matches() // 2.12 doesn't have .matches
  }

  def areNamesMatching(fromName: String, toName: String): Boolean =
    fromName == toName || normalize(fromName) == normalize(toName)

  private val getAccessor = raw"(?i)get(.)(.*)".r
  private val isAccessor = raw"(?i)is(.)(.*)".r
  val dropGetIs: String => String = {
    case getAccessor(head, tail) => head.toLowerCase + tail
    case isAccessor(head, tail)  => head.toLowerCase + tail
    case other                   => other
  }
  val isGetterName: String => Boolean = name => getAccessor.isMatching(name) || isAccessor.isMatching(name)

  private val setAccessor = raw"(?i)set(.)(.*)".r
  val dropSet: String => String = {
    case setAccessor(head, tail) => head.toLowerCase + tail
    case other                   => other
  }
  val isSetterName: String => Boolean = name => setAccessor.isMatching(name)

  val normalize: String => String = dropGetIs.andThen(dropSet)

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
  val isGarbage: String => Boolean = name => garbage(name) || name.contains(defaultElement)
}
