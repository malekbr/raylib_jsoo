#!/usr/bin/env bash

mkdir -p package/example
cp _build/default/example/{index.html,test.bc.js} package/example/
mkdir -p package/lib
cp _build/default/lib/raylib.js package/lib/