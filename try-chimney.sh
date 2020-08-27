#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.1:2.0.4 \
  org.typelevel:cats-core_2.13:2.1.1 \
  io.scalaland:chimney_2.13:0.5.3 \
  io.scalaland:chimney-cats_2.13:0.5.3 \
  -- --predef-code 'import $plugin.$ivy.`org.typelevel:kind-projector_2.13.1:0.11.0`;import io.scalaland.chimney.dsl._;import io.scalaland.chimney.cats._;import io.scalaland.chimney.derivation.auto._' < /dev/tty
