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
    , attractor = aizawa
    }
    in
    let model  = Signal.foldp update initial_model action in
      Signal.map view model

-- Model

type alias Vertex =
  { a_position : Vec3
  , a_time : Float
  }

type alias Attractor =
  ( Float -> Float -> Float -> (Float, Float, Float) )

type alias Model =
  { renderable : List Vertex
  , rotation : Mat4
  , perspective : Mat4
  , resolution : (Int, Int)
  , attractor : Attractor
  }

scale : Float
scale = 0.20
--0.01 for lorenzj
--0.25 for 

num_points : Int
num_points = 8000

perspective : Float -> Float  -> Mat4
perspective winx winy =
  mul (makePerspective 45 (winx/winy) 0.01 100)
      (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))

scaling : Mat4
scaling =
  Math.Matrix4.scale (vec3 scale scale scale) Math.Matrix4.identity

genrotate : Float -> Mat4
genrotate angle =
  rotate angle (vec3 0 1 0) Math.Matrix4.identity

genpoint : Attractor -> Maybe (Vertex) -> Vertex
genpoint attractor maybevertex =
  case maybevertex of
    Nothing ->
      initialpoint
    Just vertex ->
    let x = Math.Vector3.getX vertex.a_position
        y = Math.Vector3.getY vertex.a_position
        z = Math.Vector3.getZ vertex.a_position in
      let (newx, newy, newz) = attractor x y z in
        { a_position = vec3 newx newy newz, a_time = vertex.a_time + 0.1 }

lorenz : Attractor
lorenz x y z =
  let p = 10.0
      r = 28.0
      b = 8.0/3.0
      dt = 0.01
      dx = p * (y - x)
      dy = (r*x) - y - (x*z)
      dz = (x*y) - (b*z)
      newx = x + dx*dt
      newy = y + dy*dt
      newz = z + dz*dt in
      ( newx, newy, newz )

aizawa : Attractor
aizawa x y z =
  let alpha = 0.95
      beta = 0.7
      gamma = 0.6
      delta =3.5
      zeta = 0.1
      epsilon = 0.25
      dt = 0.01
      dx = (z - beta) * x - delta*y
      dy = delta*x + ((z - beta) * y)
      dz = gamma + alpha*z - ((z^3.0)/3.0) - (x^2+y^2)*(1+epsilon*z)+
        zeta * z * x^3
      newx = x + dx*dt
      newy = y + dy*dt
      newz = z + dz*dt in
      ( newx, newy, newz )

anishchenko_astakhov : Attractor
anishchenko_astakhov x y z =
  let
    i n = if n > 0 then 1 else 0
    mu = 1.2
    eta = 0.5
    dt = 0.01
    dy = mu*x + y - x*z
    dx = -x
    dz = -eta * z + eta * (i x) * (x^2)
    newx = x + dx*dt
    newy = y + dy*dt
    newz = z + dz*dt in
    ( newx, newy, newz )

-- bouali : Attractor
-- bouali

initialpoint : Vertex
initialpoint =
  { a_position = vec3 -1.1 0.0 0.0
   , a_time = 0.0
   }

queue : List Vertex -> List Vertex
queue list =
  if (List.length list > num_points) then
    List.take num_points list
  else
    list

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
      | renderable = queue <| genpoint model.attractor (List.head model.renderable) :: model.renderable
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
vertexShader : Shader { attr | a_position:Vec3, a_time : Float }
  { unif | perspective:Mat4, rotation:Mat4, scaling: Mat4 }
  { time:Float }
vertexShader = [glsl|

attribute vec3 a_position;
attribute float a_time;
varying float time;

uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;

void main () {
  gl_Position = perspective * scaling * rotation * vec4(a_position, 1.0);
  time = a_time;
}

|]

fragmentShader : Shader {} u { time:Float }
fragmentShader = [glsl|


precision mediump float;
varying float time;

const float PI = 3.14159265359;
const float shade = 0.8;

void main () {
  float time_scaled = time/100.0;
  gl_FragColor = shade * vec4(sin(time_scaled+PI), sin(time_scaled), cos(time_scaled), 1.0);
}

|]
