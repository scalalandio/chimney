package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait ProductTypesPlatform extends ProductTypes { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object ProductType extends ProductTypesModule {

    object platformSpecific {

      private val nonPrivateFlags = Flags.Private | Flags.PrivateLocal | Flags.Protected

      def isPublic(sym: Symbol): Boolean = (sym.flags & nonPrivateFlags).is(Flags.EmptyFlags)

      def isDefaultConstructor(ctor: Symbol): Boolean =
        isPublic(ctor) && ctor.isClassConstructor && ctor.paramSymss.filterNot(_.exists(_.isType)).flatten.isEmpty

      def isJavaGetter(getter: Symbol): Boolean =
        getter.isDefDef && isPublic(getter) && getter.paramSymss.flatten.isEmpty && isGetterName(getter.name)

      def isJavaSetter(setter: Symbol): Boolean =
        isPublic(setter) && setter.isDefDef && setter.paramSymss.flatten.size == 1 && isSetterName(setter.name)

      def isVar(setter: Symbol): Boolean =
        isPublic(setter) && setter.isValDef && setter.flags.is(Flags.Mutable)

      def isJavaSetterOrVar(setter: Symbol): Boolean =
        isJavaSetter(setter) || isVar(setter)
    }

    import platformSpecific.*

    def isCaseClass[A](A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      sym.isClassDef && sym.flags.is(Flags.Case) && !sym.flags.is(Flags.Abstract) && isPublic(sym.primaryConstructor)
    }
    def isCaseObject[A](A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      def isScala2Enum = sym.flags.is(Flags.Case | Flags.Module)
      def isScala3Enum = sym.flags.is(Flags.Case | Flags.Enum | Flags.JavaStatic)
      isPublic(sym) && (isScala2Enum || isScala3Enum)
    }
    def isJavaBean[A](A: Type[A]): Boolean = {
      val sym = TypeRepr.of(using A).typeSymbol
      val mem = sym.declarations
      sym.isClassDef && !sym.flags.is(Flags.Abstract) && mem.exists(isDefaultConstructor) && mem.exists(
        isJavaSetterOrVar
      )
    }

    def parse[A: Type]: Option[Product[A]] = ???
  }
}
