#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.12.8:1.6.4 \
  io.scalaland:chimney_2.12:0.3.1 \
  -- --predef-code 'import io.scalaland.chimney.dsl._' < /dev/tty
