import Color exposing (..) -- colors
import Graphics.Element exposing (..)
import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (..) --to get fps
import WebGL exposing (..)
import Randfloat
import Primitives exposing (cube)
import Mouse
import Task exposing (Task)
import Window

-- Main

main : Signal Element -- A changing drawable element over time.
main = -- map our scene onto webgl
  Signal.map3 view texture.signal update resolution

-- Model

perspective : Mat4
perspective =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

scaling : Mat4
scaling =
  Math.Matrix4.scale (vec3 1 1 1) Math.Matrix4.identity

texture : Signal.Mailbox (Maybe Texture)
texture =
  Signal.mailbox Nothing

port textureFetcher : Task WebGL.Error ()
port textureFetcher =
  loadTexture "/textures/fractal-texture2.png"
    `Task.andThen` \tex -> Signal.send texture.address (Just tex)

-- Update

update : Signal Float
update = --Signal.map toFloat <| Signal.map fst Mouse.position
  Signal.foldp (\dt time -> time + dt/2) 0 (fps 30)

resolution : Signal (Int, Int)
resolution = Window.dimensions

-- View

view : Maybe Texture -> Float -> (Int, Int) -> Element
view maybeTexture rotation (winx, winy) =
  case maybeTexture of
    Nothing ->
      webgl (winx, winy)
        []
    Just tex ->
      webgl (winx, winy)
          [ render vertexShader fragmentShader
            Primitives.rectangle
              { perspective = perspective
              , rotation = rotation
              , scaling = scaling
              , tex = tex
              , resolution = (vec2 (toFloat winx) (toFloat winy))
              }
          ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3}
  { unif | perspective:Mat4, rotation:Float, scaling: Mat4 }
  {}
vertexShader = [glsl|

attribute vec3 a_position;

uniform mat4 perspective;
uniform mat4 scaling;

void main () {
  gl_Position = perspective * scaling * vec4(a_position, 1.0);
}

|]

fragmentShader : Shader {} { u | rotation:Float, tex:Texture, resolution:Vec2} {}
fragmentShader = [glsl|
precision mediump float;

//thanks to Oren Shoham.

uniform sampler2D tex;
uniform float rotation;
uniform vec2 resolution;

const int num_steps =  100;

vec2 cx_mul(vec2 a, vec2 b) {
  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

void main () {
  vec2 current_coord;

  //dividing by resolution puts it into clipspace
  //multiplying it by 2 and subtracting mirrors
  vec2 prev_coord = (gl_FragCoord.xy * 2.0 - resolution)/
    min(resolution.x, resolution.y);

  const float r = 0.7885;
  float t = rotation *0.00005;
  vec2 c = vec2(cos(t)*r, 1.0);

  float num_steps_applied = 0.0;

  for (int i = 0; i < num_steps; i++) {
    num_steps_applied = float(i);

    //generate the lambda fractal
    current_coord = cx_mul(c , cx_mul(prev_coord, vec2(1.0, 0.0) - prev_coord));

    if (dot(current_coord, current_coord) > 4.0)
       break;

    prev_coord = current_coord;
  }

  float tex_x_val = (num_steps_applied == (float(num_steps) - 1.0) ? 0.0 :
  num_steps_applied) /50.0;

  gl_FragColor = texture2D(tex, vec2(tex_x_val, tex_x_val));
}

|]
