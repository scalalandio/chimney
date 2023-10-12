#!/bin/sh
COURSIER_URL=https://github.com/coursier/launchers/raw/master/coursier

test -e ~/.coursier/coursier || (mkdir -p ~/.coursier && curl -fLo ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)

~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.3:2.5.9 \
  org.typelevel:cats-core_2.13:2.9.0 \
  io.scalaland:chimney_2.13:0.8.0 \
  io.scalaland:chimney-cats_2.13:0.8.0 \
  -- --predef-code 'import $plugin.$ivy.`org.typelevel:kind-projector_2.13.10:0.13.2`;import io.scalaland.chimney.dsl._;import io.scalaland.chimney.cats._' < /dev/tty
