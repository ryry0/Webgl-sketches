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

-- Main

main : Signal Element -- A changing drawable element over time.
main = -- map our scene onto webgl
         Signal.map view update

-- Model

perspective : Mat4
perspective =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

scaling : Mat4
scaling =
  Math.Matrix4.scale (vec3 0.1 0.1 0.1) Math.Matrix4.identity

-- Update

update : Signal Mat4
update = Signal.map genRotate Mouse.position

angle : Signal Float
angle =
  Signal.foldp (\dt angle -> angle + dt/1000) 0 (fps 30)


genRotate : (Int, Int) -> Mat4
genRotate (anglex, angley) =
  mul (rotate (toFloat anglex|> (*)0.001) (vec3 0 1 0) Math.Matrix4.identity)
    (rotate (toFloat angley|>  (*)0.001) (vec3 1 0 0) Math.Matrix4.identity)

-- View

view : Mat4 -> Element
view rotation =
  webgl (400, 400)
    [ render vertexShader fragmentShader
    cube { perspective = perspective, rotation = rotation, scaling = scaling }
    ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3, color:Vec3 }
  { unif | perspective:Mat4, rotation:Mat4, scaling: Mat4 }
  { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 a_position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;
varying vec3 vcolor;

void main () {
  gl_Position = perspective * rotation * scaling * vec4(a_position, 1.0);
  vcolor = gl_Position.xyz;
}

|]

fragmentShader : Shader {} a { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
  gl_FragColor = vec4(3.0 * vcolor, 1.0);
}

|]
