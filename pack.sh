#!/usr/bin/env bash

mkdir -p package/example
cp -f _build/default/example/*.png package/example/
cp -f _build/default/example/{index.html,test.bc.js} package/example/
mkdir -p package/lib
cp -f _build/default/lib/raylib.js package/lib/