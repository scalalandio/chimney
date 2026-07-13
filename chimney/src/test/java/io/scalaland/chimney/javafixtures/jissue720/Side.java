package io.scalaland.chimney.javafixtures.jissue720;

// #720: a Java enum declared WITH utility methods - javac emits an abstract enum class with per-constant
// anonymous subclasses - used to crash Chimney's Scala 3 derivation with
// `expected a term symbol, but received class Side`.
public enum Side {
    LEFT {
        @Override
        public boolean isLeft() {
            return true;
        }
    },
    RIGHT {
        @Override
        public boolean isLeft() {
            return false;
        }
    };

    public abstract boolean isLeft();
}
