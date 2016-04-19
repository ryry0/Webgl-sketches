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
  { renderable1 : Drawable Primitives.ColoredVertex
  , renderable2 : Drawable Primitives.ColoredVertex
  , rotation1 : Mat4
  , rotation2 : Mat4
  , perspective : Mat4
  , resolution : (Int, Int)
  , mouse_position : (Int, Int)
  , translation1 : Vec2
  , translation2 : Vec2
  }

initial_model : Model
initial_model =
    { renderable1 = Primitives.cube
    , renderable2 = Primitives.cube
    , rotation1 = Math.Matrix4.identity
    , rotation2 = Math.Matrix4.identity
    , translation1 = vec2 0.0 0.0
    , translation2 = vec2 2.0 2.0
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
      { model
      | rotation1 = mul model.rotation1 <| genrotate 0.01 1.0 1.0 1.0
      , rotation2 = mul model.rotation2 <| genrotate 0.01 0.0 1.0 0.0
      }
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
        (model.renderable1)
        { perspective = model.perspective
        , rotation = model.rotation1
        , scaling = scaling
        , resolution = vec2 (toFloat <| fst model.resolution)
            (toFloat <| snd model.resolution)
        , u_mouse_position =
            vec2 (toFloat <| fst model.mouse_position) (toFloat <| snd model.mouse_position)
        , u_translation = model.translation1
        }
        , render vertexShader fragmentShader
          (model.renderable2)
          { perspective = model.perspective
          , rotation = model.rotation2
          , scaling = scaling
          , resolution = vec2 (toFloat <| fst model.resolution)
              (toFloat <| snd model.resolution)
          , u_mouse_position =
              vec2 (toFloat <| fst model.mouse_position) (toFloat <| snd model.mouse_position)
          , u_translation = model.translation2
          }
      ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3, a_color:Vec3 }
  { unif | perspective:Mat4, rotation:Mat4, scaling: Mat4, u_mouse_position:Vec2, u_translation:Vec2}
  { v_color:Vec3 }
vertexShader = [glsl|

precision mediump float;

attribute vec3 a_position;
attribute vec3 a_color;

uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;
uniform vec2 u_mouse_position;
uniform vec2 u_translation;

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
