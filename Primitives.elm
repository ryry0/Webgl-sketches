module Primitives where

import Color exposing (..) -- colors
import Math.Vector3 exposing (..)
import Math.Vector2 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)

type alias AttrPosition = Vec3
type alias AttrColor = Vec3
type alias AttrNormal = Vec3
type alias AttrCoord = Vec3

type VertexUnion
  = PureVert AttrPosition
  | ColoredVert AttrPosition AttrColor
  | TexturedVert AttrPosition AttrCoord
  | NormaledVert AttrPosition AttrNormal
  | TexNorVert  AttrPosition AttrCoord AttrNormal
  | ColNorVert AttrPosition AttrColor AttrNormal

constructVertexAttr : VertexUnion -> { a_position:Vec3 }
constructVertexAttr vertex =
  case vertex of
    PureVert pos ->
      {c |  a_position = pos }
    ColoredVert pos color ->
      { c | a_position = pos, a_color = color }

    TexturedVert pos coord ->
      { c | a_position = pos, a_coord = coord }

    NormaledVert pos norm ->
      { c | a_position = pos, a_normal = norm }

    TexNorVert pos coord norm ->
      { c | a_position = pos, a_coord = coord, a_norm = norm }

    ColNorVert pos color norm ->
      { c | a_position = pos, a_color = color, a_norm = norm }

{--
cubeTextured : Drawable VertexCoord
cubeTextured =

cubeNormal : Drawable VertexCoord
cubeNormal =

cubeColor : Color -> Drawable Vertex
cubeColor color =
  Triangle <| List.map (triangleColor color) cube
  --}

{--
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

rectangleNormal : Vec3 -> Drawable Vertex
rectangleNormal normal =
  Triangle <| List.map (triangleNormal normal) rectangle

rectangleColor : Color -> Drawable Vertex
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

triangleTextured : Vec3 -> Vec3 -> Vec3 -> (Vertex, Vertex, Vertex) ->
  (Vertex, Vertex, Vertex)
  triangleTextured  a b c (ta, tb, tc) =
    let vertexcoord vert coord =
      { position = vert.position, coord = coord }
    in
       (vertexcoord ta a, vertexcoord tb b, vertexcoord tc c)

triangleNormal : Vec3 -> Vec3 -> Vec3 -> (Vertex, Vertex, Vertex) ->
  (Vertex, Vertex, Vertex)
  triangleNormal a b c (ta, tb, tc) =
    let vertexnormal vert norm =
      { vert | position = vert.position, normal = norm }
    in
       (vertexnormal ta a, vertexnormal tb b, vertexnormal tc c)

triangleColor : Color -> (Vertex, Vertex, Vertex) ->
  (Vertex, Vertex, Vertex)
  triangleColor rawColor (a, b, c) =
    let
        color =
          let c = toRgb rawColor in
                                    vec3
                                    (toFloat c.red /255)
                                    (toFloat c.green /255)
                                    (toFloat c.blue /255)

vertexcolor vert =
  { vert | color = color}
          in
             (vertexcolor a, vertexcolor b, vertexcolor c)

getposition : Vertex s -> Vec3
getposition a = a.position a

                                           --}
triangle : Vec3 -> Vec3 -> Vec3 -> (VertexUnion, VertexUnion, VertexUnion)
triangle a b c =
  let vertex pos = PureVert pos in
    (vertex a, vertex b, vertex c)
