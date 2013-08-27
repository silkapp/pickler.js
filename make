#!/bin/bash

mkdir -p dist
fay \
  -O \
  --include=src \
  -odist/pickler.js \
  --html-wrapper \
  --package=fay-text \
  --pretty \
  src/Pickler.hs

java -jar \
  $SILK/code/dev/closure/compiler.jar \
  --compilation_level=ADVANCED_OPTIMIZATIONS \
  dist/pickler.js > \
  dist/pickler-crypted.js

