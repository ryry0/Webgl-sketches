module Primitives where

import Color exposing (..) -- colors
import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias Point = Vec3

type alias Coord = Vec3

type alias Vertex =
  { a_position : Vec3
  , color : Vec3
  }


type alias TexturedVertex =
  { a_position : Vec3
  , a_coord : Vec3
  }

cube : Drawable Vertex
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
      [ triangle green rft lft lbt--top face
      , triangle green rft lbt rbt
      , triangle blue rfb lbb lfb--bottom face
      , triangle blue rfb rbb lbb
      , triangle orange lbt lft lfb --north face
      , triangle orange lbt lfb lbb
      , triangle purple rbb rbt lbt --east face
      , triangle purple rbb lbt lbb
      , triangle yellow rft rfb lfb --west face
      , triangle yellow rft lfb lft
      , triangle red rfb rft rbt --south face
      , triangle red rfb rbt rbb
      ]

rectangle : Drawable Vertex
rectangle =
  let
      bot_left = vec3 -1 -1 0
      top_left = vec3 -1 1 0
      bot_right = vec3 1 -1 0
      top_right = vec3 1 1 0
  in
  Triangle
  [ triangle blue bot_left top_left top_right
  , triangle red  bot_left top_right bot_right
  ]

rectangleTextured : Drawable TexturedVertex
rectangleTextured =
  let
      bot_left = vec3 -1 -1 0
      top_left = vec3 -1 1 0
      bot_right = vec3 1 -1 0
      top_right = vec3 1 1 0
      bot_left_tex = vec3 0 0 0
      top_left_tex = vec3 0 1 0
      bot_right_tex = vec3 1 0 0
      top_right_tex = vec3 1 1 0
      vert a b = { a_position = a, a_coord = b }
  in
  Triangle
  [ (vert top_left top_left_tex, vert top_right top_right_tex
  , vert bot_left bot_left_tex)
  , (vert bot_left bot_left_tex, vert top_right top_right_tex
  , vert bot_right bot_right_tex)
  ]

triangle : Color -> Vec3 -> Vec3 -> Vec3 -> (Vertex, Vertex, Vertex)
triangle rawColor a b c =
  let
    color =
      let c = toRgb rawColor in
      vec3
          (toFloat c.red /255)
          (toFloat c.green /255)
          (toFloat c.blue /255)

    vertex position =
      Vertex position color
  in
     (vertex a, vertex b, vertex c)
