package io.scalaland.chimney.internal.runtime

/** Efficient prepend-oriented store for DSL override data.
  *
  * During DSL chain construction (`.withFieldConst`, `.withFieldComputed`, etc.) values are accumulated via
  * [[prepended]], which builds a cons-cell linked list in O(1) per step. On the first indexed [[apply]] call (emitted
  * by the macro into the derived transformer body), the list is materialized into an `Array` for O(1) random access.
  * This replaces the previous `Vector[Any]` backing which allocated a new collection on every prepend.
  *
  * Thread safety: materialization uses a benign race -- two threads may both materialize, but the result is identical
  * and the write to `materialized` is to a `@volatile` field, so subsequent reads on any thread see a consistent array.
  *
  * @since 2.0.0
  */
final class RuntimeDataStore private (
    // --- cons-cell build phase ---
    private val head: Any,
    private val tail: RuntimeDataStore,
    val size: Int,
    // --- materialized read phase ---
    @volatile private var materialized: Array[Any]
) {

  /** Retrieve the value at logical index `index` (0 = most recently prepended). On the first call the cons-cell chain
    * is materialized into a flat array; subsequent calls are O(1).
    */
  def apply(index: Int): Any = {
    var arr = materialized
    if (arr eq null) {
      arr = new Array[Any](size)
      var current = this
      var i = 0
      while (i < size) {
        arr(i) = current.head
        current = current.tail
        i += 1
      }
      materialized = arr
    }
    arr(index)
  }

  /** O(1) prepend -- allocates a single cons-cell object. */
  def prepended(value: Any): RuntimeDataStore =
    new RuntimeDataStore(value, this, size + 1, null)

  override def toString: String = {
    val sb = new java.lang.StringBuilder("RuntimeDataStore(")
    var current = this
    var i = 0
    while (i < size) {
      if (i > 0) { val _ = sb.append(", ") }
      val _ = sb.append(current.head)
      current = current.tail
      i += 1
    }
    sb.append(")").toString
  }
}

object RuntimeDataStore {
  val empty: RuntimeDataStore = new RuntimeDataStore(null, null, 0, new Array[Any](0))
}
