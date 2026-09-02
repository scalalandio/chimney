package io.scalaland.chimney.internal

import io.scalaland.chimney.internal.runtime.RuntimeDataStore
import io.scalaland.chimney.ChimneySpec

class RuntimeDataStoreSpec extends ChimneySpec {

  test("RuntimeDataStore.empty has size 0") {
    RuntimeDataStore.empty.size ==> 0
  }

  test("RuntimeDataStore.prepended increments size") {
    val store0 = RuntimeDataStore.empty
    val store1 = store0.prepended("a")
    val store2 = store1.prepended("b")
    val store3 = store2.prepended("c")

    store0.size ==> 0
    store1.size ==> 1
    store2.size ==> 2
    store3.size ==> 3
  }

  test("RuntimeDataStore.apply returns values in prepend order (index 0 = most recent)") {
    val store = RuntimeDataStore.empty
      .prepended("first")
      .prepended("second")
      .prepended("third")

    store(0) ==> "third"
    store(1) ==> "second"
    store(2) ==> "first"
  }

  test("RuntimeDataStore preserves old snapshots after prepend") {
    val store1 = RuntimeDataStore.empty.prepended("a")
    val store2 = store1.prepended("b")

    // old snapshot is unchanged
    store1.size ==> 1
    store1(0) ==> "a"

    // new snapshot has both
    store2.size ==> 2
    store2(0) ==> "b"
    store2(1) ==> "a"
  }

  test("RuntimeDataStore handles branching (same parent, two children)") {
    val parent = RuntimeDataStore.empty.prepended("x")
    val childA = parent.prepended("a")
    val childB = parent.prepended("b")

    childA(0) ==> "a"
    childA(1) ==> "x"
    childB(0) ==> "b"
    childB(1) ==> "x"
  }

  test("RuntimeDataStore works with heterogeneous types") {
    val store = RuntimeDataStore.empty
      .prepended(42)
      .prepended("hello")
      .prepended(3.14)
      .prepended(List(1, 2, 3))

    store(0) ==> List(1, 2, 3)
    store(1) ==> 3.14
    store(2) ==> "hello"
    store(3) ==> 42
  }

  test("RuntimeDataStore works with null values") {
    val store = RuntimeDataStore.empty
      .prepended(null)
      .prepended("not null")
      .prepended(null)

    store(0) ==> null
    store(1) ==> "not null"
    store(2) ==> null
  }

  test("repeated apply reuses materialized array (fast path)") {
    val store = RuntimeDataStore.empty
      .prepended("a")
      .prepended("b")
      .prepended("c")

    store(0) ==> "c"
    store(1) ==> "b"
    store(2) ==> "a"
    // second round of reads exercises the cached array
    store(0) ==> "c"
    store(1) ==> "b"
    store(2) ==> "a"
  }

  test("materialize parent, then branch — children produce correct arrays") {
    val parent = RuntimeDataStore.empty.prepended("x")
    // force parent materialization
    parent(0) ==> "x"

    val childA = parent.prepended("a")
    val childB = parent.prepended("b")

    childA(0) ==> "a"
    childA(1) ==> "x"
    childB(0) ==> "b"
    childB(1) ==> "x"
    // parent still correct
    parent(0) ==> "x"
  }

  test("toString shows contents") {
    RuntimeDataStore.empty.toString ==> "RuntimeDataStore()"
    RuntimeDataStore.empty.prepended("a").toString ==> "RuntimeDataStore(a)"
    RuntimeDataStore.empty.prepended(1).prepended(2).toString ==> "RuntimeDataStore(2, 1)"
  }

  test("wrap creates a pre-materialized store with correct indexing") {
    val store = RuntimeDataStore.wrap(Array[Any]("c", "b", "a"))
    store.size ==> 3
    store(0) ==> "c"
    store(1) ==> "b"
    store(2) ==> "a"
  }

  test("wrap with empty array returns empty") {
    val store = RuntimeDataStore.wrap(Array.empty[Any])
    store.size ==> 0
  }

  test("wrap store supports prepended (branching from flat base)") {
    val base = RuntimeDataStore.wrap(Array[Any]("b", "a"))
    val child1 = base.prepended("c1")
    val child2 = base.prepended("c2")

    child1(0) ==> "c1"
    child1(1) ==> "b"
    child1(2) ==> "a"

    child2(0) ==> "c2"
    child2(1) ==> "b"
    child2(2) ==> "a"

    base(0) ==> "b"
    base(1) ==> "a"
  }

  test("wrap store toString uses materialized array") {
    val store = RuntimeDataStore.wrap(Array[Any](1, 2, 3))
    store.toString ==> "RuntimeDataStore(1, 2, 3)"
  }

  test("prependedAll combines values with existing store") {
    val base = RuntimeDataStore.empty.prepended("a").prepended("b")
    val result = base.prependedAll(Array[Any]("d", "c"))
    result.size ==> 4
    result(0) ==> "d"
    result(1) ==> "c"
    result(2) ==> "b"
    result(3) ==> "a"
  }

  test("prependedAll with empty array returns this") {
    val base = RuntimeDataStore.empty.prepended("a")
    val result = base.prependedAll(Array.empty[Any])
    result.size ==> 1
    result(0) ==> "a"
  }

  test("prependedAll on empty store returns wrap of values") {
    val result = RuntimeDataStore.empty.prependedAll(Array[Any]("c", "b", "a"))
    result.size ==> 3
    result(0) ==> "c"
    result(1) ==> "b"
    result(2) ==> "a"
  }

  test("prependedAll on wrapped store combines correctly") {
    val base = RuntimeDataStore.wrap(Array[Any]("b", "a"))
    val result = base.prependedAll(Array[Any]("d", "c"))
    result.size ==> 4
    result(0) ==> "d"
    result(1) ==> "c"
    result(2) ==> "b"
    result(3) ==> "a"
  }

  test("prependedAll preserves original store") {
    val base = RuntimeDataStore.wrap(Array[Any]("b", "a"))
    val result = base.prependedAll(Array[Any]("c"))
    base.size ==> 2
    base(0) ==> "b"
    base(1) ==> "a"
    result.size ==> 3
    result(0) ==> "c"
    result(1) ==> "b"
    result(2) ==> "a"
  }
}
