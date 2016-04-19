module Randfloat where

import Graphics.Element exposing (show)
import Signal
import Time exposing (second)
import Random

{--
main : Signal Graphics.Element.Element
main = Signal.map show <| genrandfloat 0 10
--}

genrandfloatlist : Int -> Float -> Float -> Signal Time.Time -> Signal (List Float)
genrandfloatlist numelem low high time =
  Signal.map (randfloatlist numelem low high) (timeseed time)

randfloatlist : Int -> Float -> Float -> Random.Seed -> List Float
randfloatlist numelem low high seed=
  seed
    |> ( Random.generate <| Random.list numelem (Random.float low high))
    |> fst

genrandfloatpair : Float -> Float -> Signal Time.Time -> Signal (Float, Float)
genrandfloatpair low high time =
  Signal.map (randfloatpair low high) (timeseed time)

randfloatpair : Float -> Float -> Random.Seed -> (Float, Float)
randfloatpair low high seed =
  seed
    |> (Random.generate
    <| Random.pair (Random.float low high) (Random.float low high))
    |> fst

genrandfloat : Float -> Float -> Signal Time.Time -> Signal Float
genrandfloat low high time =
  Signal.map (randfloat low high) (timeseed time)

randfloat : Float -> Float -> Random.Seed -> Float
randfloat low high seed =
  seed |> (Random.generate <| Random.float low high) |> fst

timeseed : Signal Time.Time -> Signal Random.Seed
timeseed time =
  Signal.map Random.initialSeed <| Signal.map round <| time
