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

-- Main

main : Signal Element -- A changing drawable element over time.
main = -- map our scene onto webgl
         Signal.map2 view texture.signal update

-- Model

perspective : Mat4
perspective =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

scaling : Mat4
scaling =
  Math.Matrix4.scale (vec3 0.1 0.1 0.1) Math.Matrix4.identity

texture : Signal.Mailbox (Maybe Texture)
texture =
  Signal.mailbox Nothing

port textureFetcher : Task WebGL.Error ()
port textureFetcher =
  loadTexture "./textures/leaf.jpg"
    `Task.andThen` \tex -> Signal.send texture.address (Just tex)

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

view : Maybe Texture -> Mat4 -> Element
view maybeTexture rotation =
  case maybeTexture of
    Nothing ->
      webgl (400, 400)
        []
    Just tex ->
      webgl (400, 400)
          [ render vertexShader fragmentShader
          Primitives.rectangleTextured
          { perspective = perspective, rotation = rotation, scaling = scaling, tex = tex}
          ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3, a_coord:Vec3 }
  { unif | perspective:Mat4, rotation:Mat4, scaling: Mat4 }
  { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 a_position;
attribute vec3 a_coord;

uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;

varying vec2 vcoord;

void main () {
  gl_Position = perspective * rotation * scaling * vec4(a_position, 1.0);
  vcoord = a_coord.xy;
}

|]

fragmentShader : Shader {} { u | tex:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;

uniform sampler2D tex;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(tex, vcoord);
}

|]
