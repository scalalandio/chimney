package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ProductTypesPlatform extends ProductTypes { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected object ProductType extends ProductTypesModule {

    object platformSpecific {

      def isDefaultConstructor(ctor: Symbol): Boolean =
        ctor.isPublic && ctor.isConstructor && ctor.asMethod.paramLists.flatten.isEmpty

      def isCaseClassField(field: Symbol): Boolean =
        field.isMethod && field.asMethod.isGetter && field.asMethod.isCaseAccessor

      def isJavaGetter(getter: Symbol): Boolean =
        getter.isPublic && getter.isMethod && getter.asMethod.paramLists.flatten.isEmpty && isGetterName(
          getter.asMethod.name.toString
        )

      def isJavaSetter(setter: Symbol): Boolean =
        setter.isPublic && setter.isMethod && setter.asMethod.paramLists.flatten.size == 1 && isSetterName(
          setter.asMethod.name.toString
        )

      def isVar(setter: Symbol): Boolean =
        setter.isPublic && setter.isTerm && setter.asTerm.name.toString.endsWith("_$eq")

      def isJavaSetterOrVar(setter: Symbol): Boolean =
        isJavaSetter(setter) || isVar(setter)
    }

    import platformSpecific.*

    def isCaseClass[A](A: Type[A]): Boolean = {
      val sym = A.typeSymbol
      sym.isClass && sym.asClass.isCaseClass && !sym.isAbstract && sym.asClass.primaryConstructor.isPublic
    }
    def isCaseObject[A](A: Type[A]): Boolean = {
      val sym = A.typeSymbol
      def isScala2Enum = sym.asClass.isCaseClass
      def isScala3Enum = sym.isStatic && sym.isFinal // paramless case in S3 cannot be checked for "case"
      sym.isPublic && sym.isModuleClass && (isScala2Enum || isScala3Enum)
    }
    def isJavaBean[A](A: Type[A]): Boolean = {
      val sym = A.typeSymbol
      val mem = A.members
      sym.isClass && !sym.isAbstract && mem.exists(isDefaultConstructor) && mem.exists(isJavaSetterOrVar)
    }

    def parse[A: Type]: Option[Product[A]] = ???
  }
}
