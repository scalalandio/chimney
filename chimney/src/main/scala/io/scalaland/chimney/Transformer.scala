package io.scalaland.chimney

import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

/** Maps data from one type `From` into another `To`.
  *
  * Extraction of `From => Into` logic into a separate type class allow
  * us to define logic once in one place and refer to it
  * in mutliple places:
  *
  * {{{
  * class Foo(...)
  * class Bar(...)
  * object Bar {
  *   implicit val fromBar: Transformer[Foo, Bar] = ...
  * }
  * implicit[Transformer[Foo, Bar]].transform(foo) // use predefined instance
  * }}}
  *
  * and generate implementation using information from the type:
  *
  * {{{
  * case class Foo(int: Int, str: String)
  * case class Bar(int: Int, str: String)
  * implicitly[Transformer[Foo, Bar]] // derive value
  * }}}
  *
  * If the default setting of generator are not good enough, you can provide
  * it with settings to customize the behavior:
  *
  * {{{
  * implicit val fooToBar: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
  *   .setting1
  *   .setting2
  *   .buildTransformer
  * }}}
  *
  * For the list of available settings and their meaning consult
  * [[io.scalaland.chimney.dsl.TransformerDefinition]].
  *
  *
  * If transformation is derived ad hoc in place it is used, you can use
  * convenient DSL:
  *
  * {{{
  * import io.scalaland.chimney.dsl._ // enables DSL
  * val foo: Foo = ...
  * val bar = foo.into[Bar].modifier1.modifier2.transform
  * }}}
  *
  * If there are no modifiers needed (or if implicit `Transformer` is already
  * defined in scope) you can use shorter syntax:
  *
  * {{{
  * import io.scalaland.chimney.dsl._ // enables DSL
  * val foo: Foo = ...
  * val bar = foo.transformInto[Bar]
  * }}}
  *
  * @see [[io.scalaland.chimney.Transformer#derive]] for information how default generation works
  * @see [[io.scalaland.chimney.Transformer#define]] for information how to start building customized Transformer
  * @see [[io.scalaland.chimney.dsl.TransformerOps]] for syntax that starts building Transformer and applying it in the same time
  * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for avilable settings of Transformer derivation
  * @see [[io.scalaland.chimney.dsl.TransformerInto]] for availabe settings for building Transformer and applying it in thr same time
  *
  * @tparam From data type that will be used as input
  * @tparam To   data type that will be used as output
  */
trait Transformer[From, To] {

  def transform(src: From): To
}

/** The companion of [[io.scalaland.chimney.Transformer]].
  *
  * Used to provide implicit with default [[io.scalaland.chimney.Transformer]]
  * implementation as well as DSL for building [[io.scalaland.chimney.Transformer]]
  * using [[io.scalaland.chimney.dsl.TransformerDefinition]].
  */
object Transformer {

  /** Provides [[io.scalaland.chimney.Transformer]] derived with the default settings.
    *
    * If derivation with defaults is not possible, macro expansion will fail
    * and the instance should be define manually to solve e.g. field renames or
    * missing values (it can be done using provided DSL).
    *
    * @tparam From data type that will be used as input
    * @tparam Into data type that will be used as output
    * @return default implementation (if posible)
    */
  implicit def derive[From, To]: Transformer[From, To] =
    macro ChimneyBlackboxMacros.deriveTransformerImpl[From, To]

  /** Convenient utility to create an empty [[io.scalaland.chimney.dsl.TransformerDefinition]].
    *
    * {{{
    * implicit val fooToBar: Transformer[Foo, Bar] = Transformer.define[Foo, Bar]
    *   .modifier1
    *   .modifier2
    *   .buildTransformer
    * }}}
    *
    * @see [[io.scalaland.chimney.dsl.TransformerDefinition]] for available settings
    *
    * @tparam From data type that will be used as input
    * @tparam Into data type that will be used as output
    * @return [[io.scalaland.chimney.dsl.TransformerDefinition]] with defaults
    */
  def define[From, To]: TransformerDefinition[From, To, TransformerCfg.Empty] =
    new TransformerDefinition[From, To, TransformerCfg.Empty](Map.empty, Map.empty)
}
