/**
  * Aliases for easier access to the Chimney API.
  * */
package object chimney {
  import io.scalaland.{chimney => ch}

  val dsl: ch.dsl.type = ch.dsl

  type Patcher[T, Patch] = ch.Patcher[T, Patch]
  type Transformer[From, To] = ch.Transformer[From, To]
}
