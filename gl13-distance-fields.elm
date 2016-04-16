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
-- Vertex Shader
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


-- Fragment Shader
fragmentShader : Shader {} { unif | u_mouse_position:Vec2, u_resolution:Vec2 } {}
fragmentShader = [glsl|

//based on lightbits' tutorial

precision mediump float;
uniform vec2 u_resolution;
uniform vec2 u_mouse_position;

//tests if ray hit object
bool rayMarch(vec3 ray_origin , vec3 ray_dir,
  out int num_iter , out float dist_traveled);

vec3 compNormal(vec3 point);
vec3 lambertLight(vec3 point, vec3 light_pos, vec3 light_color);
float castShadow(vec3 point, vec3 light_pos, float shadow_intensity);

float mapScene(vec3 point); //function that fully describes scene in distance
float sdSphere ( vec3 point, float radius );
float sdPlane (vec3 point, vec4 normal);
float sdBox( vec3 p, vec3 b );

void main () {
  //to generate perspective matrices
  const vec3 cam_eye = vec3(1.5, 1.0, -2.0); //vec3(0, 0, -2);
  const vec3 cam_forward = normalize(-cam_eye); //vec3(0, 0, 1);
  const vec3 cam_right = normalize(cross(vec3(0, 1, 0), cam_forward)); //vec3(1, 0, 0);
  const vec3 cam_up = normalize(cross(cam_forward, cam_right));//vec3(0, 1, 0);
  const float focal_length = 2.0;

  float u = gl_FragCoord.x * 2.0/min(u_resolution.x, u_resolution.y) - 1.0;
  float v = gl_FragCoord.y * 2.0/min(u_resolution.x, u_resolution.y) - 1.0;
  float mouse_x = u_mouse_position.x*2.0/
    min(u_resolution.x, u_resolution.y) -1.0;
  float mouse_y = -u_mouse_position.y*2.0/
    min(u_resolution.x, u_resolution.y) +1.0;


  vec3 ray_origin = cam_eye;
  vec3 ray_dir = normalize((cam_forward * focal_length) + cam_right * u + cam_up * v);

  int num_iter; //number of iterations to hit an object
  float dist_traveled; //distance ray has gone to hit object
  bool ray_hit;

  //determine distance and num_iter
  ray_hit = rayMarch(ray_origin, ray_dir, num_iter, dist_traveled);

  vec3 color = vec3(0.0, 0.0, 0.0);

  if (ray_hit) {
    vec3 ray_loc = ray_origin + ray_dir*dist_traveled;
    color = (lambertLight(ray_loc, vec3(mouse_x, mouse_y, -2.0), vec3(0.0, 1.0, 1.0)) +
      lambertLight(ray_loc, vec3(0.5, 0.5, 0.5), vec3(1.0, 0.0, 0.0)))/2.0;
  }

  gl_FragColor = vec4(color, 1.0);
} //end main

bool rayMarch(vec3 ray_origin
             , vec3 ray_dir
             , out int num_iter
             , out float dist_traveled) {

  const float epsilon = 0.001;
  const float z_far_limit = 30.0;
  const int max_steps = 64;
  bool hit = false;

  dist_traveled = 0.0;

  for(int i = 0; i < max_steps; ++i) {
    float dist_to_object = mapScene(ray_origin + ray_dir*dist_traveled);

    if (dist_to_object < epsilon) {
      hit = true;
      break;
    }
    else if (dist_traveled > z_far_limit) {
      hit = false;
      break;
    }

    dist_traveled+=dist_to_object;
    num_iter = i;
  } //end for

  return hit;
} //end raymarch

//perform numerical differentiation to get the normal vector.
vec3 compNormal(vec3 point) {
  float delta = 0.0001;

  float dx = mapScene(point + vec3(delta, 0.0, 0.0)) - mapScene(point - vec3(delta, 0.0, 0.0));
  float dy = mapScene(point + vec3(0.0, delta, 0.0)) - mapScene(point - vec3(0.0, delta, 0.0));
  float dz = mapScene(point + vec3(0.0, 0.0, delta)) - mapScene(point - vec3(0.0, 0.0, delta));
  return normalize(vec3(dx, dy, dz));
}

vec3 lambertLight(vec3 point, vec3 light_pos, vec3 light_color) {
  const vec3 ambient_light = vec3(0);// vec3(0.01, 0.01, 0.01);
  float light_intensity = 0.0;

  float shadow = castShadow(point, light_pos, 16.0);
  if (shadow > 0.0) {
    vec3 normal = compNormal(point);
    vec3 light_dir = normalize(light_pos - point);
    light_intensity = shadow*clamp(dot(normal, light_dir), 0.0, 1.0);
  }

  return light_color*light_intensity + ambient_light*(1.0 - light_intensity);
}

//reverse trace the shadow from the surface to the light
//returns 0.0 if completely in shadow, else returns a higher value clamped @ 1.0.

float castShadow(vec3 point, vec3 light_pos, float shadow_intensity) {
  const float epsilon = 0.001;
  const int max_steps = 64;

  //should not travel farther than source
  float max_dist = length(light_pos - point);

  vec3 ray_dir = normalize(light_pos - point);

  float result = 1.0;
  float dist_traveled = 10.0 * epsilon;

  for (int i = 0; i < max_steps; i++) {
    float dist = mapScene(point + ray_dir*dist_traveled);

    //we hit a surface before we hit light
    if (dist < epsilon) {
      result = 0.0;
      break;
    }

    //calculate penumbra factor using how close we are to an adjacent surface
    result = min(result, shadow_intensity * dist/dist_traveled);
    dist_traveled += dist;

    if (dist_traveled >= max_dist)
       break;

  } //end for
  return result;
}

float sdSphere (vec3 point, float radius) {
  return length(point)-radius;
}

float sdBox(vec3 point, vec3 box_dim) {
  vec3 dist = abs(point) - box_dim;
  return min(max(dist.x,max(dist.y,dist.z)),0.0) +
         length(max(dist,0.0));
}

float udRoundBox(vec3 point, vec3 dim, float radius) {
  return length(max(abs(point) - dim, 0.01)) - radius;
}

float sdPlane (vec3 point, vec4 normal) {
  return dot(point, normal.xyz) + normal.w;
}

//function that fully describes the scene in distances
float mapScene(vec3 point) {
  const float radius = 0.5;

  float o1 = sdSphere(point + vec3 (-0.5, 0.2, 0.0), radius);
  float o2 =
    udRoundBox(point + vec3 (0.5, 0.3, 0.0), vec3(0.25, 0.25, 0.25), 0.05);
  float o3 = sdPlane(point + vec3(0.0, 0.5, 0.0), vec4(0.0, 1.0, 0.0, 1.0));
  return min(min(o1, o2), o3);;
}

|]
