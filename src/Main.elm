module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (style)
import Task
import Time exposing (Weekday(..))
import Date
import Svg exposing (Svg)
import Svg.Attributes as Attr

main : Program () Model Msg
main =
  Browser.document
  { init = always init
  , update = update
  , view = view
  , subscriptions = always subscriptions
  }



-- MODEL --

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }

init : ( Model, Cmd Msg )
init =
  ( Model Time.utc (Time.millisToPosix 0)
  , Task.perform Adjust <| Task.map2 Tuple.pair Time.here Time.now
  )



-- UPDATE --

type Msg
  = Tick Time.Posix
  | Adjust (Time.Zone, Time.Posix)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick posix ->
      ( { model | time = posix }
      , Cmd.none
      )

    Adjust (zone, time) ->
      ( Model zone time
      , Cmd.none
      )



-- VIEW --

view : Model -> Browser.Document Msg
view model =
  Browser.Document
  "週末時計"
  [ Html.div
    [ style "margin" "auto"
    , style "width" "200px"
    ]
    [ drawClock model
    , Html.p
      [ style "font-family" "sans-serif"
      , style "font-size" "28px"
      ]
      [ Html.text <| "IT IS " ++ leftString model ]
    ]
  ]

leftString : Model -> String
leftString model =
  case model |> leftDays of
    0 -> "WEEKEND TODAY!!"
    1 -> "1 DAY TO WEEKEND"
    n -> String.fromInt n ++ " DAYS TO WEEKEND"

drawClock : Model -> Html msg
drawClock model =
  Html.div []
    [ Svg.svg
      [ Attr.width "200", Attr.height "200", Attr.viewBox "0 0 200 200" ]
      [ drawDial
      , drawHand <| leftDays model
      ]
    ]

drawDial : Svg msg
drawDial =
  let
    dot limit =
      Svg.circle
        [ Attr.cx <| String.fromFloat (center + 120.0 * cos (leftMinuteToAngle limit))
        , Attr.cy <| String.fromFloat (center - 120.0 * sin (leftMinuteToAngle limit))
        , Attr.r "15"
        , Attr.fill "black"
        ] []
  in
    Svg.g []
      [ Svg.circle
        [ Attr.cx <| String.fromFloat center
        , Attr.cy <| String.fromFloat center
        , Attr.r "160"
        , Attr.strokeWidth "20px"
        , Attr.stroke "black"
        , Attr.fill "none"
        ] []
      , Svg.g []
          ( List.range 0 3
              |> List.map ((*) 5)
              |> List.map dot
          )
      ]

drawHand : Int -> Svg msg
drawHand limit =
  Svg.g []
    [ Svg.line
      [ Attr.x1 <| String.fromFloat center
      , Attr.y1 <| String.fromFloat center
      , Attr.x2 <| String.fromFloat (center + 100.0 * cos (leftMinuteToAngle limit))
      , Attr.y2 <| String.fromFloat (center - 100.0 * sin (leftMinuteToAngle limit))
      , Attr.strokeWidth "3px"
      , Attr.stroke "black"
      ] []
    , Svg.line
      [ Attr.x1 <| String.fromFloat center
      , Attr.y1 <| String.fromFloat center
      , Attr.x2 <| String.fromFloat (center + 70.0 * cos (leftMinuteToAngle 0))
      , Attr.y2 <| String.fromFloat (center - 70.0 * sin (leftMinuteToAngle 0))
      , Attr.strokeWidth "3px"
      , Attr.stroke "black"
      ] []
    ]

leftMinuteToAngle : Int -> Float
leftMinuteToAngle int =
  toFloat int / 60.0 * 2 * pi + pi / 2.0

center : Float
center =
  180.0

leftDays : Model -> Int
leftDays { zone, time } =
  let
    date = Date.fromPosix zone time
  in
    case date |> Date.weekday of
      Mon -> 5
      Tue -> 4
      Wed -> 3
      Thu -> 2
      Fri -> 1
      Sat -> 0
      Sun -> 0



-- SUBSCRIPTIONS --

subscriptions : Sub Msg
subscriptions =
  let
    second = 1000
  in
    Time.every (1 * second) Tick
