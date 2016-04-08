module Randfloat where

import Graphics.Element exposing (show)
import Signal
import Time exposing (second)
import Random

{--
main : Signal Graphics.Element.Element
main = Signal.map show <| genrandfloat 0 10
--}

genrandfloat : Float -> Float -> Signal Float
genrandfloat low high =
  Signal.map (randfloat low high) timeseed

randfloat : Float -> Float -> Random.Seed -> Float
randfloat low high seed =
  seed |> (Random.generate <| Random.float low high) |> fst

timeseed : Signal Random.Seed
timeseed =
  Signal.map Random.initialSeed <| Signal.map round <| Time.every second
