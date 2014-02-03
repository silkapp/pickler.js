#!/bin/bash

plain=dist/out/pickler.js
min=dist/out/pickler.min.js

build="fay \
  -O \
  --include=src \
  -o$plain \
  --package=fay-text \
  --pretty \
  --library \
  --strict Pickler \
  src/Pickler.hs"

crypt="java -jar \
   $SILK/code/dev/closure/compiler.jar \
   --compilation_level=SIMPLE_OPTIMIZATIONS \
   --externs=dist/externs.js \
   $plain"

mkdir -p dist/out
$build && $crypt > $min

