#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P -M ammonite.Main \
  com.lihaoyi:ammonite_2.13.1:1.8.1 \
  io.scalaland:chimney_2.13:0.3.4 \
  -- --predef-code 'import io.scalaland.chimney.dsl._' < /dev/tty
