package io.scalaland.chimney.internal.runtime

/** Efficient prepend-oriented store for DSL override data.
  *
  * During DSL chain construction (`.withFieldConst`, `.withFieldComputed`, etc.) values are accumulated via
  * [[prepended]], which builds a cons-cell linked list in O(1) per step. On the first indexed [[apply]] call (emitted
  * by the macro into the derived transformer body), the list is materialized into an `Array` for O(1) random access.
  * This replaces the previous `Vector[Any]` backing which allocated a new collection on every prepend.
  *
  * When the terminal macro (e.g. `buildTransformer`) can prove that the DSL chain is a linear expression (no `val`
  * captures), it emits [[RuntimeDataStore.wrap]] to construct a pre-materialized store in a single allocation, skipping
  * the cons-cell chain entirely.
  *
  * Thread safety: materialization uses a benign race -- two threads may both materialize, but the result is identical.
  * The `materialized` field is intentionally non-volatile: a thread that does not see a prior write simply
  * re-materializes (producing the same array contents), avoiding the overhead of a volatile read on every `apply()`.
  *
  * @since 2.0.0
  */
final class RuntimeDataStore private (
    // --- cons-cell build phase ---
    private val head: Any,
    private val tail: RuntimeDataStore,
    val size: Int,
    // --- materialized read phase ---
    private var materialized: Array[Any]
) {

  /** Retrieve the value at logical index `index` (0 = most recently prepended). On the first call the cons-cell chain
    * is materialized into a flat array; subsequent calls are O(1). If the tail (or any node in the chain) is already
    * materialized (e.g. created via [[RuntimeDataStore.wrap]]), its array is bulk-copied via `System.arraycopy`.
    */
  def apply(index: Int): Any = {
    var arr = materialized
    if (arr eq null) {
      arr = new Array[Any](size)
      var current = this
      var i = 0
      while (i < size) {
        val currentMat = current.materialized
        if (currentMat ne null) {
          System.arraycopy(currentMat, 0, arr, i, current.size)
          i = size
        } else {
          arr(i) = current.head
          current = current.tail
          i += 1
        }
      }
      materialized = arr
    }
    arr(index)
  }

  /** O(1) prepend -- allocates a single cons-cell object. */
  def prepended(value: Any): RuntimeDataStore =
    new RuntimeDataStore(value, this, size + 1, null)

  /** Bulk-prepend: creates a single pre-materialized store containing `values` followed by this store's data. Used by
    * macro-generated code for partial chain optimization -- when the DSL chain is split by a `val`, the continuation's
    * data is grouped into one `prependedAll` call instead of N individual [[prepended]] calls.
    *
    * The caller must not mutate `values` after this call.
    */
  def prependedAll(values: Array[Any]): RuntimeDataStore = {
    val n = values.length
    if (n == 0) this
    else if (size == 0) RuntimeDataStore.wrap(values)
    else {
      val total = n + size
      val arr = new Array[Any](total)
      System.arraycopy(values, 0, arr, 0, n)
      apply(0)
      System.arraycopy(materialized, 0, arr, n, size)
      new RuntimeDataStore(arr(0), null, total, arr)
    }
  }

  override def toString: String = {
    val sb = new java.lang.StringBuilder("RuntimeDataStore(")
    val arr = materialized
    var i = 0
    if (arr ne null) {
      while (i < size) {
        if (i > 0) { val _ = sb.append(", ") }
        val _ = sb.append(arr(i))
        i += 1
      }
    } else {
      var current = this
      while (i < size) {
        if (i > 0) { val _ = sb.append(", ") }
        val _ = sb.append(current.head)
        current = current.tail
        i += 1
      }
    }
    sb.append(")").toString
  }
}

object RuntimeDataStore {
  val empty: RuntimeDataStore = new RuntimeDataStore(null, null, 0, new Array[Any](0))

  /** Create a pre-materialized store from the given array. Index 0 in the array corresponds to logical index 0 (the
    * most recently prepended value). The caller must not mutate `values` after this call -- no defensive copy is made.
    * This is safe when the array is freshly constructed by macro-generated code.
    *
    * A store created via `wrap` supports [[prepended]]: the child node's materialization loop detects the
    * pre-materialized tail and bulk-copies via `System.arraycopy` instead of walking a cons-cell chain.
    */
  def wrap(values: Array[Any]): RuntimeDataStore = {
    val n = values.length
    if (n == 0) empty
    else new RuntimeDataStore(values(0), null, n, values)
  }
}
