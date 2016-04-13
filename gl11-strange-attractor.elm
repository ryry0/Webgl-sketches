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
  let initial_model =
    { renderable = [initialpoint]
    , rotation = Math.Matrix4.identity
    , perspective = perspective 1 1
    , resolution = (1000, 1000)
    }
    in
    let model  = Signal.foldp update initial_model action in
      Signal.map view model

-- Model

type alias Vertex =
  { a_position : Vec3
  , a_color : Vec3
  , a_time : Float
  }

type alias Model =
  { renderable : List Vertex
  , rotation : Mat4
  , perspective : Mat4
  , resolution : (Int, Int)
  }

perspective : Float -> Float  -> Mat4
perspective winx winy =
  mul (makePerspective 45 (winx/winy) 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

scaling : Mat4
scaling =
  Math.Matrix4.scale (vec3 0.1 0.1 0.1) Math.Matrix4.identity


genrotate : Float -> Mat4
genrotate angle =
  rotate angle (vec3 1 1 1) Math.Matrix4.identity

genpoint : Maybe (Vertex) -> Vertex
genpoint maybevertex =
  case maybevertex of
    Nothing ->
      initialpoint
    Just vertex ->
      { vertex | a_time = vertex.a_time + 0.1 }

initialpoint : Vertex
initialpoint =
  { a_position = vec3 3.0 8.0 7.0
   , a_color = vec3 0.0 0.0 1.0
   , a_time = 0.0
   }

-- Update
type Action = Resolution (Int, Int) | DeltaTime Float
action : Signal Action
action =
  Signal.merge
    (Signal.map Resolution Window.dimensions)
    (Signal.map DeltaTime <| fps 30)

update : Action -> Model -> Model
update action model =
  case action of
    Resolution (x, y) ->
    { model
    | perspective = perspective (toFloat x) (toFloat y)
    , resolution = (x, y)
    }
    DeltaTime dt ->
    { model
    | renderable = genpoint (List.head model.renderable) :: model.renderable
    , rotation = mul model.rotation <| genrotate 0.01
    }

-- View
view : Model -> Element
view model =
      webgl (fst model.resolution, snd model.resolution)
          [ render vertexShader fragmentShader
            (LineStrip model.renderable)
            { perspective = model.perspective
            , rotation = model.rotation
            , scaling = scaling
            , resolution = vec2 (toFloat <| fst model.resolution)
                (toFloat <| snd model.resolution)
            }
          ]

-- Shaders
vertexShader : Shader { attr | a_position:Vec3, a_color:Vec3, a_time : Float}
  { unif | perspective:Mat4, rotation:Mat4, scaling: Mat4 }
  { v_color : Vec3 }
vertexShader = [glsl|

attribute vec3 a_position;
attribute vec3 a_color;
attribute float a_time;

uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;
varying vec3 v_color;

void main () {

  gl_Position = perspective * scaling * rotation * vec4(a_position, 1.0);
  v_color = a_color;
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
