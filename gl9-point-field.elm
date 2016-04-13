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
  Signal.map2 view update resolution

-- Model

perspective : Mat4
perspective =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

scaling : Mat4
scaling =
  Math.Matrix4.scale (vec3 0.1 0.1 0.1) Math.Matrix4.identity

pointcube : Drawable Primitives.Vertex
pointcube =
  let range = [-10..10] in
    let mapped_range = List.map vec3 range in
      let foldrange = List.foldl (\x acc -> List.map x range :: acc) [] in
        Points <| List.map tovertex <| List.concat <| foldrange <| List.concat <| foldrange mapped_range

tovertex : Vec3 -> Primitives.Vertex
tovertex vec =
  { a_position = vec, a_color = vec3 0.0 0.0 1.0 }

-- Update
update : Signal Mat4
update = Signal.map genRotate angle

genRotate : Float -> Mat4
genRotate angle =
  rotate angle (vec3 1 1 1) Math.Matrix4.identity

angle : Signal Float
angle =
  Signal.foldp (\dt time -> time + dt/5000) 0 (fps 30)

resolution : Signal (Int, Int)
resolution = Window.dimensions

-- View
view : Mat4 -> (Int, Int) -> Element
view rotation (winx, winy) =
      webgl (winx, winy)
          [ render vertexShader fragmentShader
            pointcube
              { perspective = perspective
              , rotation = rotation
              , scaling = scaling
              , resolution = (vec2 (toFloat winx) (toFloat winy))
              }
          ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3, a_color:Vec3 }
  { unif | perspective:Mat4, rotation:Mat4, scaling: Mat4 }
  { v_color : Vec3 }
vertexShader = [glsl|

attribute vec3 a_position;
attribute vec3 a_color;

uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;
varying vec3 v_color;

void main () {
  gl_Position = perspective * scaling * rotation * vec4(a_position, 1.0);
  v_color = gl_Position.xyz;
}

|]

fragmentShader : Shader {} { u | rotation:Mat4, resolution:Vec2}
  { v_color : Vec3 }
fragmentShader = [glsl|

precision mediump float;

uniform vec2 resolution;
varying vec3 v_color;

void main () {
  gl_FragColor = vec4(v_color, 1.0);
}

|]
