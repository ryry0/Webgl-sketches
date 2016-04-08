module Randfloat where

import Graphics.Element exposing (show)
import Signal
import Time exposing (second)
import Random

{--
main : Signal Graphics.Element.Element
main = Signal.map show <| genrandfloat 0 10
--}

genrandfloat : Float -> Float -> Signal Time.Time -> Signal Float
genrandfloat low high time =
  Signal.map (randfloat low high) (timeseed time)

randfloat : Float -> Float -> Random.Seed -> Float
randfloat low high seed =
  seed |> (Random.generate <| Random.float low high) |> fst

timeseed : Signal Time.Time -> Signal Random.Seed
timeseed time =
  Signal.map Random.initialSeed <| Signal.map round <| time
