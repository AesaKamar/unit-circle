module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes as HTMLA
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, field, float, map)
import Random
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes as SVGA
import Task exposing (perform)
import Tuple exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { screenSize : Size2d
    , unitCircle :
        { center : Coord2D
        , boundingBoxTopLeft : Coord2D
        , radius : Float
        }
    , activePoint : Coord2D
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { height = 0, width = 0 }
      , unitCircle =
            { center = { x = 0, y = 0 }
            , boundingBoxTopLeft = { x = 0, y = 0 }
            , radius = 0
            }
      , activePoint = { x = 0, y = 0 }
      }
    , perform
        (\x ->
            ScreenResize
                { height = x.scene.height
                , width = x.scene.width
                }
        )
        getViewport
    )



-- UPDATE


type alias Coord2D =
    { x : Float, y : Float }


type alias Size2d =
    { height : Float, width : Float }


type Msg
    = ScreenResize Size2d
    | MouseHover Coord2D


constrainToSquare : Size2d -> Size2d
constrainToSquare size2d =
    let
        lowerBound =
            min size2d.height size2d.width
    in
    { height = lowerBound, width = lowerBound }


calculateCenter : Size2d -> Coord2D
calculateCenter size2d =
    { y = size2d.height / 2, x = size2d.width / 2 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenResize resizedScreen ->
            let
                constrainingSquare =
                    constrainToSquare resizedScreen

                center =
                    calculateCenter resizedScreen

                radius =
                    constrainingSquare.height / 2
            in
            ( { screenSize = resizedScreen
              , unitCircle =
                    { center = center
                    , boundingBoxTopLeft =
                        { y = center.y - radius
                        , x = center.x - radius
                        }
                    , radius = radius
                    }
              , activePoint = model.activePoint
              }
            , Cmd.none
            )

        MouseHover coord ->
            ( { screenSize = model.screenSize
              , unitCircle = model.unitCircle
              , activePoint = coord
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize
            (\x y ->
                ScreenResize
                    { height = y |> toFloat
                    , width = x |> toFloat
                    }
            )
        , onMouseMove
            (Json.Decode.map
                MouseHover
                (Json.Decode.map2 Coord2D
                    (field "clientX" float)
                    (field "clientY" float)
                )
            )
        ]


type Node
    = Document
    | Window


snapToUnitCircle : Coord2D -> Float -> Coord2D -> Coord2D
snapToUnitCircle center radius coord2D =
    let
        normalized =
            { x = coord2D.x - center.x
            , y = coord2D.y - center.y
            }

        hypotenuse =
            sqrt ((normalized.x ^ 2) + (normalized.y ^ 2))
    in
    if hypotenuse == 0 then
        { x = center.x + radius, y = center.y }

    else
        { x = (normalized.x * radius / hypotenuse) + center.x
        , y = (normalized.y * radius / hypotenuse) + center.y
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        unitCircleShrunkRadius =
            model.unitCircle.radius / 3
    in
    div []
        [ svg
            [ SVGA.height (model.screenSize.height |> fromFloat)
            , SVGA.width (model.screenSize.width |> fromFloat)
            ]
            [ viewBoundingBox model.unitCircle.boundingBoxTopLeft
                model.unitCircle.radius
            , viewUnitCircle model.unitCircle.center
                unitCircleShrunkRadius
            , viewXAxis model.unitCircle.center model.screenSize.width
            , viewYAxis model.unitCircle.center model.screenSize.height
            , viewHypotenuse model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewSin model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewCos model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewSec model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewCsc model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewTan model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewCot model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            , viewLocatedPointOnUnitCircle model.unitCircle.center
                unitCircleShrunkRadius
                model.activePoint
            ]
        ]


viewHypotenuse center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos
    in
    line
        [ SVGA.x1 (center.x |> fromFloat)
        , SVGA.y1 (center.y |> fromFloat)
        , SVGA.x2 (snapped.x |> fromFloat)
        , SVGA.y2 (snapped.y |> fromFloat)
        , SVGA.stroke "black"
        , SVGA.strokeWidth "3"
        ]
        []


viewSin center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos
    in
    line
        [ SVGA.x1 (snapped.x |> fromFloat)
        , SVGA.y1 (snapped.y |> fromFloat)
        , SVGA.x2 (snapped.x |> fromFloat)
        , SVGA.y2 (center.y |> fromFloat)
        , SVGA.stroke "red"
        , SVGA.strokeWidth "3"
        ]
        []


viewCos center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos
    in
    line
        [ SVGA.x1 (snapped.x |> fromFloat)
        , SVGA.y1 (snapped.y |> fromFloat)
        , SVGA.x2 (center.x |> fromFloat)
        , SVGA.y2 (snapped.y |> fromFloat)
        , SVGA.stroke "blue"
        , SVGA.strokeWidth "3"
        ]
        []


viewSec center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos

        centeredX =
            snapped.x - center.x

        hypotenuse =
            sqrt (((snapped.x - center.x) ^ 2) + ((snapped.y - center.y) ^ 2))

        farX =
            if centeredX == 0 then
                0

            else
                (1 / (centeredX / hypotenuse))
                    * hypotenuse
                    + center.x
    in
    line
        [ SVGA.x1 (center.x |> fromFloat)
        , SVGA.y1 (center.y |> fromFloat)
        , SVGA.x2 (farX |> fromFloat)
        , SVGA.y2 (center.y |> fromFloat)
        , SVGA.stroke "teal"
        , SVGA.strokeWidth "3"
        ]
        []


viewCsc center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos

        centeredY =
            snapped.y - center.y

        hypotenuse =
            sqrt (((snapped.x - center.x) ^ 2) + ((snapped.y - center.y) ^ 2))

        farY =
            if centeredY == 0 then
                0

            else
                (1 / (centeredY / hypotenuse))
                    * hypotenuse
                    + center.y
    in
    line
        [ SVGA.x1 (center.x |> fromFloat)
        , SVGA.y1 (center.y |> fromFloat)
        , SVGA.x2 (center.x |> fromFloat)
        , SVGA.y2 (farY |> fromFloat)
        , SVGA.stroke "pink"
        , SVGA.strokeWidth "3"
        ]
        []


viewTan center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos

        centeredX =
            snapped.x - center.x

        hypotenuse =
            sqrt (((snapped.x - center.x) ^ 2) + ((snapped.y - center.y) ^ 2))

        farX =
            if centeredX == 0 then
                0

            else
                (1 / (centeredX / hypotenuse))
                    * hypotenuse
                    + center.x
    in
    line
        [ SVGA.x1 (snapped.x |> fromFloat)
        , SVGA.y1 (snapped.y |> fromFloat)
        , SVGA.x2 (farX |> fromFloat)
        , SVGA.y2 (center.y |> fromFloat)
        , SVGA.stroke "tan"
        , SVGA.strokeWidth "3"
        ]
        []


viewCot center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos

        centeredY =
            snapped.y - center.y

        hypotenuse =
            sqrt (((snapped.x - center.x) ^ 2) + ((snapped.y - center.y) ^ 2))

        farY =
            if centeredY == 0 then
                10000000

            else
                (1 / (centeredY / hypotenuse))
                    * hypotenuse
                    + center.y
    in
    line
        [ SVGA.x1 (snapped.x |> fromFloat)
        , SVGA.y1 (snapped.y |> fromFloat)
        , SVGA.x2 (center.x |> fromFloat)
        , SVGA.y2 (farY |> fromFloat)
        , SVGA.stroke "orange"
        , SVGA.strokeWidth "3"
        ]
        []


viewLocatedPointOnUnitCircle center radius currentPos =
    let
        snapped =
            snapToUnitCircle center radius currentPos
    in
    circle
        [ SVGA.cx (snapped.x |> fromFloat)
        , SVGA.cy (snapped.y |> fromFloat)
        , SVGA.r "6"
        ]
        []


viewYAxis center height =
    line
        [ SVGA.x1 (center.x |> fromFloat)
        , SVGA.y1 (0 |> fromFloat)
        , SVGA.x2 (center.x |> fromFloat)
        , SVGA.y2 (height |> fromFloat)
        , SVGA.strokeWidth "1"
        , SVGA.stroke "lightgray"
        , SVGA.strokeDasharray "5, 5"
        ]
        []


viewXAxis center width =
    line
        [ SVGA.x1 (0 |> fromFloat)
        , SVGA.y1 (center.y |> fromFloat)
        , SVGA.x2 (width |> fromFloat)
        , SVGA.y2 (center.y |> fromFloat)
        , SVGA.strokeWidth "1"
        , SVGA.stroke "lightgray"
        , SVGA.strokeDasharray "5, 5"
        ]
        []


viewBoundingBox topLeftPoint radius =
    rect
        [ SVGA.y (topLeftPoint.y |> fromFloat)
        , SVGA.x (topLeftPoint.x |> fromFloat)
        , SVGA.height (radius * 2 |> fromFloat)
        , SVGA.width (radius * 2 |> fromFloat)
        , SVGA.stroke "black"
        , SVGA.fillOpacity "0"
        ]
        []


viewCenter center =
    circle
        [ SVGA.cx (center.x |> fromFloat)
        , SVGA.cy (center.y |> fromFloat)
        , SVGA.r "6"
        ]
        []


viewUnitCircle center radius =
    circle
        [ SVGA.cx (center.x |> fromFloat)
        , SVGA.cy (center.y |> fromFloat)
        , SVGA.r (radius |> fromFloat)
        , SVGA.fillOpacity "0"
        , SVGA.stroke "black"
        ]
        []
