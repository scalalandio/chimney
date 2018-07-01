#!/bin/sh
COURSIER_URL=https://git.io/vgvpD
test -e ~/.coursier/coursier || \
  (mkdir -p ~/.coursier && curl -L -s --output ~/.coursier/coursier $COURSIER_URL && chmod +x ~/.coursier/coursier)
~/.coursier/coursier launch -q -P \
  com.lihaoyi:ammonite_2.12.4:1.1.0 \
  io.scalaland:chimney_2.12:0.2.1 \
  -- --predef-code 'import io.scalaland.chimney.dsl._' < /dev/tty
