#!/bin/zsh

ocamlfind opt \
    -ccopt "$(freetype-config --cflags)" \
    -linkpkg \
    -syntax camlp4o \
    -package sexplib \
    -package sexplib.syntax \
    ft2.ml \
    freetype-stubs.c \
    -ccopt "$(freetype-config --libs)"
