#include "raylib.h"
#include "raymath.h"
#include <emscripten/bind.h>

using namespace emscripten;

EMSCRIPTEN_BINDINGS(emcc_types) {
  function("InitWindow", &InitWindow);
  value_object<Vector2>("Vector2")
      .field("x", &Vector2::x)
      .field("y", &Vector2::y);
  function("Vector2Add", &Vector2Add);
  function("Vector2Zero", &Vector2Zero);
  function("Vector2Zero", &Vector2Zero);
  function("Vector2One", &Vector2One);
  function("Vector2Add", &Vector2Add);
  function("Vector2AddValue", &Vector2AddValue);
  function("Vector2Subtract", &Vector2Subtract);
  function("Vector2SubtractValue", &Vector2SubtractValue);
  function("Vector2Length", &Vector2Length);
  function("Vector2LengthSqr", &Vector2LengthSqr);
  function("Vector2DotProduct", &Vector2DotProduct);
  function("Vector2Distance", &Vector2Distance);
  function("Vector2DistanceSqr", &Vector2DistanceSqr);
  function("Vector2Angle", &Vector2Angle);
  function("Vector2Scale", &Vector2Scale);
  function("Vector2Multiply", &Vector2Multiply);
  function("Vector2Negate", &Vector2Negate);
  function("Vector2Divide", &Vector2Divide);
  function("Vector2Normalize", &Vector2Normalize);
  function("Vector2Transform", &Vector2Transform);
  function("Vector2Lerp", &Vector2Lerp);
  function("Vector2Reflect", &Vector2Reflect);
  function("Vector2Rotate", &Vector2Rotate);
  function("Vector2MoveTowards", &Vector2MoveTowards);
  function("Vector2Invert", &Vector2Invert);
  function("Vector2Clamp", &Vector2Clamp);
  function("Vector2ClampValue", &Vector2ClampValue);
  function("Vector2Equals", &Vector2Equals);
}