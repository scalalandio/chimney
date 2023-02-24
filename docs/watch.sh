#!/bin/sh

ag -l . -g "\.rst$" | entr sh -c 'make clean html'
