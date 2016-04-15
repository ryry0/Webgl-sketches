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
  { drawable : Drawable Primitives.TexturedVertex
  , rotation : Mat4
  , perspective : Mat4
  , resolution : (Int, Int)
  , mouse_position : (Int, Int)
  }

initial_model : Model
initial_model =
    { drawable = Primitives.rectangleTextured
    , rotation = Math.Matrix4.identity
    , perspective = genperspective 1 1
    , resolution = (1000, 1000)
    , mouse_position = (0, 0)
    }

scaling : Mat4
scaling =
  let scale = 1 in
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
vertexShader : Shader { attr | a_position:Vec3, a_coord:Vec3 }
  { unif | u_perspective:Mat4, u_rotation:Mat4, u_scaling: Mat4, u_mouse_position:Vec2 }
  {}
vertexShader = [glsl|

precision mediump float;

attribute vec3 a_position;
attribute vec3 a_coord;

uniform mat4 u_perspective;
uniform mat4 u_scaling;
uniform mat4 u_rotation;
uniform vec2 u_mouse_position;

void main () {
  gl_Position = vec4(a_position, 1.0);
}

|]

fragmentShader : Shader {} { unif | u_mouse_position:Vec2, u_resolution:Vec2 } {}
fragmentShader = [glsl|
precision mediump float;
uniform vec2 u_resolution;


//to generate perspective matrices
const float epsilon = 0.1;
const float radius = 0.5;
const int max_steps = 32;
const vec3 eye = vec3(0, 0, -1);
const vec3 up = vec3(0, 1, 0);
const vec3 right = vec3(1, 0, 0);

float sdSphere ( vec3 point, float radius ) {
  return length(point)-radius;
}

vec4 raymarch(vec3 ray_origin, vec3 ray_dir) {
  float t = 0.0;

  for(int i = 0; i < max_steps; ++i) {
    float dist = sdSphere(ray_origin + ray_dir*t, 0.5);
    if (dist < epsilon) {
      return vec4(1.0, 1.0, 1.0, 1.0);
    }
    t+=dist;
  }

  return vec4(0.0, 0.0, 0.0, 1.0);
}

void main () {

  float u = gl_FragCoord.x * 2.0/min(u_resolution.x, u_resolution.y) - 1.0;
  float v = gl_FragCoord.y * 2.0/min(u_resolution.x, u_resolution.y) - 1.0;
  vec3 ray_origin = right * u + up * v;
  vec3 ray_dir = normalize(cross(right, up));

  vec4 color = raymarch(ray_origin, ray_dir);

  gl_FragColor = color;
}

|]
