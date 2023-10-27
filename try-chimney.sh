#!/bin/sh
COURSIER_URL=https://github.com/coursier/launchers/raw/master/coursier

test -e ~/.coursier/coursier || (mkdir -p ~/.coursier && curl -fLo ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)

~/.coursier/coursier launch --java-opt -Dfoo=bar --java-opt -Xmx2g --java-opt -Xss16m com.lihaoyi:ammonite_3.3.1:latest.release -M ammonite.Main \
  -- --predef-code 'import $ivy.`io.scalaland::chimney-cats:latest.release`;import io.scalaland.chimney.dsl._;import io.scalaland.chimney.cats._' < /dev/tty
