#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.12.7:1.4.4 \
  io.scalaland:chimney_2.12:0.3.0 \
  -- --predef-code 'import io.scalaland.chimney.dsl._' < /dev/tty
