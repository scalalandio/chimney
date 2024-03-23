package io.scalaland.chimney

import io.scalaland.chimney.dsl.TransformedNamesComparison

class TransformedNamesComparisonSpec extends ChimneySpec {

  group("TransformedNamesComparison.BeanAware") {

    test("should match identical names") {
      TransformedNamesComparison.BeanAware.namesMatch("someField", "someField") ==> true
    }

    test("should allows matching fields with Java Bean getters and setters") {
      TransformedNamesComparison.BeanAware.namesMatch("someField", "isSomeField") ==> true
      TransformedNamesComparison.BeanAware.namesMatch("isSomeField", "someField") ==> true
      TransformedNamesComparison.BeanAware.namesMatch("someField", "getSomeField") ==> true
      TransformedNamesComparison.BeanAware.namesMatch("getSomeField", "someField") ==> true
      TransformedNamesComparison.BeanAware.namesMatch("someField", "setSomeField") ==> true
      TransformedNamesComparison.BeanAware.namesMatch("setSomeField", "someField") ==> true
    }

    test("should not match names converted with different conventions") {
      TransformedNamesComparison.BeanAware.namesMatch("someField", "some-field") ==> false
      TransformedNamesComparison.BeanAware.namesMatch("some-field", "someField") ==> false
      TransformedNamesComparison.BeanAware.namesMatch("someField", "some_field") ==> false
      TransformedNamesComparison.BeanAware.namesMatch("some_field", "someField") ==> false
      TransformedNamesComparison.BeanAware.namesMatch("someField", "SOME_FIELD") ==> false
      TransformedNamesComparison.BeanAware.namesMatch("SOME_FIELD", "someField") ==> false
    }
  }

  group("TransformedNamesComparison.StrictEquality") {

    test("should match identical names") {
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "someField") ==> true
    }

    test("should not match names converted with different conventions") {
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "isSomeField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("isSomeField", "someField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "getSomeField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("getSomeField", "someField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "setSomeField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("setSomeField", "someField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "some-field") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("some-field", "someField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "some_field") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("some_field", "someField") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("someField", "SOME_FIELD") ==> false
      TransformedNamesComparison.StrictEquality.namesMatch("SOME_FIELD", "someField") ==> false
    }
  }

  group("TransformedNamesComparison.CaseInsensitiveEquality") {

    test("should match identical names") {
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "someField") ==> true
    }

    test("should match names which differ only in letter capitalisation") {
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "SomeField") ==> true
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("SomeField", "someField") ==> true
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "SOMEFIELD") ==> true
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("SOMEFIELD", "someField") ==> true
    }

    test("should not match names converted with different conventions") {
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "isSomeField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("isSomeField", "someField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "getSomeField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("getSomeField", "someField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "setSomeField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("setSomeField", "someField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "some-field") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("some-field", "someField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "some_field") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("some_field", "someField") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("someField", "SOME_FIELD") ==> false
      TransformedNamesComparison.CaseInsensitiveEquality.namesMatch("SOME_FIELD", "someField") ==> false
    }
  }
}
