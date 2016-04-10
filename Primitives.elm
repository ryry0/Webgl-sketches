module Primitives where

import Color exposing (..) -- colors
import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias Vertex a =
  { a | position : Vec3 }

type alias Colored a =
  { a | color : Vec3 }

type alias Normaled a =
  { a | normal : Vec3 }

type alias Textured a =
  { a | coord : Vec3 }

cubeColor : Color -> Drawable (Vertex s)
cubeColor color =
  Triangle <| List.map (triangleColor color) cube

{--
cubeTextured : Drawable VertexCoord
cubeTextured =

cubeNormal : Drawable VertexCoord
cubeNormal =
  --}

cube : Drawable (Vertex s)
cube =
  let
    rft = vec3  1  1  1   -- right, front, top
    lft = vec3 -1  1  1   -- left,  front, top
    lbt = vec3 -1 -1  1
    rbt = vec3  1 -1  1
    rbb = vec3  1 -1 -1
    rfb = vec3  1  1 -1
    lfb = vec3 -1  1 -1
    lbb = vec3 -1 -1 -1
  in
    Triangle
      [ triangle rft lft lbt--top face
      , triangle rft lbt rbt
      , triangle rfb lbb lfb--bottom face
      , triangle rfb rbb lbb
      , triangle lbt lft lfb --north face
      , triangle lbt lfb lbb
      , triangle rbb rbt lbt --east face
      , triangle rbb lbt lbb
      , triangle rft rfb lfb --west face
      , triangle rft lfb lft
      , triangle rfb rft rbt --south face
      , triangle rfb rbt rbb
      ]

rectangleNormal : Vec3 -> Drawable (Vertex s)
rectangleNormal normal =
  Triangle <| List.map (triangleNormal normal) rectangle

rectangleColor : Color -> Drawable (Vertex s)
rectangleColor color =
  Triangle <| List.map (triangleColor color) rectangle

rectangle : Drawable (Vertex s)
rectangle =
  let
    bot_left = vec3 0 0 0
    top_left = vec3 0 1 0
    bot_right = vec3 1 0 0
    top_right = vec3 1 1 0
  in
  Triangle
  [ triangle bot_left top_left top_right
  , triangle bot_left top_right bot_right
  ]

triangleTextured : Vec3 -> Vec3 -> Vec3 -> (Vertex {}, Vertex {}, Vertex {}) ->
  (Textured (Vertex {}), Textured (Vertex {}), Textured (Vertex {}))
triangleTextured  a b c (ta, tb, tc) =
  let vertexcoord vert coord =
    { position = vert.position, coord = coord }
  in
    (vertexcoord ta a, vertexcoord tb b, vertexcoord tc c)

triangleNormal : Vec3 -> Vec3 -> Vec3 -> (Vertex {}, Vertex {}, Vertex {}) ->
  (Normaled (Vertex {}), Normaled (Vertex {}), Normaled (Vertex {}))
triangleNormal a b c (ta, tb, tc) =
  let vertexnormal vert norm =
    { vert | position = vert.position, normal = norm }
  in
    (vertexnormal ta a, vertexnormal tb b, vertexnormal tc c)

triangleColor : Color -> (Vertex s, Vertex s, Vertex s) ->
  (Colored (Vertex s), Colored (Vertex s), Colored (Vertex s))
triangleColor rawColor (a, b, c) =
  let
    color =
      let c = toRgb rawColor in
      vec3
          (toFloat c.red /255)
          (toFloat c.green /255)
          (toFloat c.blue /255)

    vertexcolor vert =
      { vert , color = color}
  in
     (vertexcolor a, vertexcolor b, vertexcolor c)

triangle : Vec3 -> Vec3 -> Vec3 -> (Vertex {}, Vertex {}, Vertex {})
triangle a b c =
  let vertex pos = { position = pos } in
    (vertex a, vertex b, vertex c)

