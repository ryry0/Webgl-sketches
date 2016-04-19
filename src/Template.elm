import Color exposing (..) -- colors
import Graphics.Element exposing (..)
import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (..) --to get fps
import WebGL exposing (..)
import Randfloat
import Primitives
import Mouse
import Task exposing (Task)
import Window

-- Main

main : Signal Element -- A changing drawable element over time.
main = -- map our scene onto webgl
    let model  = Signal.foldp update initial_model action in
      Signal.map view model

-- Model
type alias Model =
  { drawable : Drawable Primitives.ColoredVertex
  , rotation : Mat4
  , perspective : Mat4
  , resolution : (Int, Int)
  , mouse_position : (Int, Int)
  }

initial_model : Model
initial_model =
    { drawable = Primitives.cube
    , rotation = Math.Matrix4.identity
    , perspective = genperspective 1 1
    , resolution = (1000, 1000)
    , mouse_position = (0, 0)
    }

scaling : Mat4
scaling =
  let scale = 0.1 in
    Math.Matrix4.scale (vec3 scale scale scale) Math.Matrix4.identity

-- Update
type Action = MouseMove (Int, Int) | Resolution (Int, Int) | DeltaTime Float

action : Signal Action
action =
  Signal.mergeMany
    [ Signal.map Resolution Window.dimensions
    , Signal.map DeltaTime <| fps 30
    , Signal.map MouseMove Mouse.position
    ]

update : Action -> Model -> Model
update action model =
  case action of
    Resolution (x, y) ->
      { model
      | perspective = genperspective (toFloat x) (toFloat y)
      , resolution = (x, y)
      }
    DeltaTime dt ->
      model
    MouseMove pos ->
      { model
      | mouse_position = pos
      }

genrotate : Float -> Float -> Float-> Float -> Mat4
genrotate angle x y z =
  rotate angle (vec3 x y z) Math.Matrix4.identity

genperspective : Float -> Float  -> Mat4
genperspective winx winy =
  mul (makePerspective 45 (winx/winy) 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

-- View
view : Model -> Element
view model =
  webgl (fst model.resolution, snd model.resolution)
      [ render vertexShader fragmentShader
        (model.drawable)
        { u_perspective = model.perspective
        , u_rotation = model.rotation
        , u_scaling = scaling
        , u_resolution = vec2 (toFloat <| fst model.resolution)
            (toFloat <| snd model.resolution)
        , u_mouse_position =
            vec2 (toFloat <| fst model.mouse_position) (toFloat <| snd model.mouse_position)
        }
      ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3, a_color:Vec3 }
  { unif | u_perspective:Mat4, u_rotation:Mat4, u_scaling: Mat4, u_mouse_position:Vec2 }
  { v_color:Vec3 }
vertexShader = [glsl|

precision mediump float;

attribute vec3 a_position;
attribute vec3 a_color;

uniform mat4 u_perspective;
uniform mat4 u_scaling;
uniform mat4 u_rotation;
uniform vec2 u_mouse_position;

varying vec3 v_color;

void main () {
  gl_Position = perspective * scaling * rotation *
    vec4(a_position.xy + u_translation, a_position.z, 1.0);
  v_color = a_color;
}

|]

fragmentShader : Shader {} { unif | u_mouse_position:Vec2 } { v_color:Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform vec2 u_mouse_position;
varying vec3 v_color;

const float PI = 3.14159265359;

void main () {
  gl_FragColor = vec4(v_color, 1.0);
}

|]
