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
}
