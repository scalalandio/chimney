# Supported Transformations

## Into `case class`

TODO

```scala
// TODO example
caseClass.transformInto[CaseClass]
caseClass.into[CaseClass].transform
```

## Reading from Java Bean properties

```scala
// TODO example
bean.into[CaseClass].enableBeanGetters.transform
```

## Writing to Java Bean properties

```scala
// TODO example
bean.into[CaseClass].enableBeanSetters.transform
```

## Allowing constructor's defaults

## Wiring constructor's parameter to its source field

TODO mentions how it works in Java Beans

## Wiring constructor's parameter to raw value 

TODO mentions how it works in Java Beans

## Wiring constructor's parameter to computed value

TODO mentions how it works in Java Beans

## From/To `AnyVal`

TODO

## Between `sealed`/`enum`s

TODO

TODO java enums

TODO flags

TODO overrides

TODO java enums limitations

## From/To `Option`

TODO

TODO option unwrapping in partial

TODO refer Optional support in java-collections

TODO flags

## Between `Either`s

## Between Scala's collections

TODO

TODO mention Factory

TODO mention java-collections

## Custom transformations

TODO

TODO total -> partial

TODO implicit conflict resolution
