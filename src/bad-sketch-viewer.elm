-- bad because I am completely sidestepping problem of signals of signals
import Graphics.Element exposing (..)
import Graphics.Input
import Task exposing (Task)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Window

-- Main

main : Signal Html -- A changing drawable element over time.
main = -- map our scene onto webgl
     let model  = Signal.foldp update initial_model action in
      Signal.map view model

-- Model
type alias Model =
  { source :String
  , resolution: (Int, Int)
  }

initial_model : Model
initial_model = { source = "gl1-rect.html", resolution = (1000, 1000) }

main_dropdown : Element
main_dropdown =
  Graphics.Input.dropDown (Signal.message dropdownaction.address)
  [ ("" , Nothing)
  , ("Rectangle", Just "gl1-rect.html")
  , ("Randomly Moving Rectangle", Just "gl2-random-rect.html")
  , ("Randomly Moving Rectangle via Uniforms", Just "gl3-random-rect-uniform.html")
  , ("Pretty Cube", Just "gl4-prettycube.html")
  , ("Cube Mouse Control", Just "gl5-cube-control.html")
  , ("Textured Rectangle", Just "gl6-textured-rec.html")
  , ("Lambda Fractal", Just "gl7-lambda-fractal.html")
  , ("4th Power Julia Fractal", Just "gl8-julia4-fractal.html")
  , ("Colored Point Field", Just "gl9-point-field.html")
  , ("Point Cube", Just "gl10-point-cube.html")
  , ("Strange Attractors", Just "gl11-strange-attractor.html")
  , ("Signed Distance Fields", Just "gl13-distance-fields.html")
  , ("Multiple Render Objects", Just "gl14-multi-cube.html")
  ]

-- Update
type Action = Select (Maybe String) | Resolution (Int, Int)

dropdownaction : Signal.Mailbox (Maybe String)
dropdownaction = Signal.mailbox Nothing

action : Signal Action
action = Signal.merge
  (Signal.map Select dropdownaction.signal)
  (Signal.map Resolution  Window.dimensions)

update : Action -> Model -> Model
update action model =
  case action of
    Select maybestring ->
      case maybestring of
        Nothing ->
         { model | source = initial_model.source }

        Just string ->
         { model | source = string }

    Resolution a ->
      { model | resolution = a }

-- View
view : Model -> Html
view model =
  div []
  [ fromElement main_dropdown
  , iframe [ Html.Attributes.src model.source
           , Html.Attributes.width <| (fst model.resolution) - 10
           , Html.Attributes.height <| (snd model.resolution) - 40
           ] []
  ]
