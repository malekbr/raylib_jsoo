#include "raylib.h"
#include "raymath.h"
#include <emscripten/bind.h>
#include <string>

using namespace emscripten;

void InitWindowWrapped(int width, int height, std::string title) {
  InitWindow(width, height, title.c_str());
}

EMSCRIPTEN_BINDINGS(emcc_types) {
  function("InitWindow", &InitWindowWrapped);

  // Vector2
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

  // Color
  value_object<Color>("Color")
      .field("r", &Color::r)
      .field("g", &Color::g)
      .field("b", &Color::b)
      .field("a", &Color::a);
  constant("LIGHTGRAY", LIGHTGRAY);
  constant("GRAY", GRAY);
  constant("DARKGRAY", DARKGRAY);
  constant("YELLOW", YELLOW);
  constant("GOLD", GOLD);
  constant("ORANGE", ORANGE);
  constant("PINK", PINK);
  constant("RED", RED);
  constant("MAROON", MAROON);
  constant("GREEN", GREEN);
  constant("LIME", LIME);
  constant("DARKGREEN", DARKGREEN);
  constant("SKYBLUE", SKYBLUE);
  constant("BLUE", BLUE);
  constant("DARKBLUE", DARKBLUE);
  constant("PURPLE", PURPLE);
  constant("VIOLET", VIOLET);
  constant("DARKPURPLE", DARKPURPLE);
  constant("BEIGE", BEIGE);
  constant("BROWN", BROWN);
  constant("DARKBROWN", DARKBROWN);
  constant("WHITE", WHITE);
  constant("BLACK", BLACK);
  constant("BLANK", BLANK);
  constant("MAGENTA", MAGENTA);
  constant("RAYWHITE", RAYWHITE);

  function("BeginDrawing", &BeginDrawing);
  function("EndDrawing", &EndDrawing);

  // rshape
  function("DrawPixel", &DrawPixel);
  function("DrawPixelV", &DrawPixelV);
  function("DrawLine", &DrawLine);
  function("DrawLineV", &DrawLineV);
  function("DrawLineEx", &DrawLineEx);
  function("DrawLineBezier", &DrawLineBezier);
  function("DrawLineBezierQuad", &DrawLineBezierQuad);
  function("DrawLineBezierCubic", &DrawLineBezierCubic);
  function("DrawLineStrip", &DrawLineStrip);
  function("DrawCircle", &DrawCircle);
  function("DrawCircleSector", &DrawCircleSector);
  function("DrawCircleSectorLines", &DrawCircleSectorLines);
  function("DrawCircleGradient", &DrawCircleGradient);
  function("DrawCircleV", &DrawCircleV);
  function("DrawCircleLines", &DrawCircleLines);
  function("DrawEllipse", &DrawEllipse);
  function("DrawEllipseLines", &DrawEllipseLines);
  function("DrawRing", &DrawRing);
  function("DrawRingLines", &DrawRingLines);
  function("DrawRectangle", &DrawRectangle);
  function("DrawRectangleV", &DrawRectangleV);
  function("DrawRectangleRec", &DrawRectangleRec);
  function("DrawRectanglePro", &DrawRectanglePro);
  function("DrawRectangleGradientV", &DrawRectangleGradientV);
  function("DrawRectangleGradientH", &DrawRectangleGradientH);
  function("DrawRectangleGradientEx", &DrawRectangleGradientEx);
  function("DrawRectangleLines", &DrawRectangleLines);
  function("DrawRectangleLinesEx", &DrawRectangleLinesEx);
  function("DrawRectangleRounded", &DrawRectangleRounded);
  function("DrawRectangleRoundedLines", &DrawRectangleRoundedLines);
  function("DrawTriangle", &DrawTriangle);
  function("DrawTriangleLines", &DrawTriangleLines);
  function("DrawTriangleFan", &DrawTriangleFan);
  function("DrawTriangleStrip", &DrawTriangleStrip);
  function("DrawPoly", &DrawPoly);
  function("DrawPolyLines", &DrawPolyLines);
  function("DrawPolyLinesEx", &DrawPolyLinesEx);
}