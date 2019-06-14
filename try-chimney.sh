#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.0:1.6.8 \
  io.scalaland:chimney_2.13:0.3.2 \
  -- --predef-code 'import io.scalaland.chimney.dsl._' < /dev/tty
