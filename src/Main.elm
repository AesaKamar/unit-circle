module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes as HTMLA
import Html.Events exposing (..)
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
        { center : Coord
        , boundingBoxTopLeft : Coord
        , radius : Float
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { height = 0, width = 0 }
      , unitCircle =
            { center = ( 0, 0 )
            , boundingBoxTopLeft = ( 0, 0 )
            , radius = 0
            }
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


type alias Coord =
    ( Float, Float )


type alias Size2d =
    { height : Float, width : Float }


type Msg
    = ScreenResize Size2d


constrainToSquare : Size2d -> Size2d
constrainToSquare size2d =
    let
        lowerBound =
            min size2d.height size2d.width
    in
    { height = lowerBound, width = lowerBound }


calculateCenter : Size2d -> Coord
calculateCenter size2d =
    ( size2d.height / 2, size2d.width / 2 )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenResize resizedScreen ->
            let
                constrainingSquare =
                    constrainToSquare resizedScreen

                ( cy, cx ) =
                    calculateCenter resizedScreen

                radius =
                    constrainingSquare.height / 2
            in
            ( { screenSize = resizedScreen
              , unitCircle =
                    { center = ( cy, cx )
                    , boundingBoxTopLeft =
                        ( cy - radius
                        , cx - radius
                        )
                    , radius = radius
                    }
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize
        (\x y ->
            ScreenResize
                { height = y |> toFloat
                , width = x |> toFloat
                }
        )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ SVGA.height (model.screenSize.height |> fromFloat)
            , SVGA.width (model.screenSize.width |> fromFloat)
            ]
            [ viewBoundingBox model.unitCircle.boundingBoxTopLeft
                model.unitCircle.radius
            , viewCenter model.unitCircle.center
            , viewUnitCircle model.unitCircle.center
                (model.unitCircle.radius / 5)
            , viewXAxis model.unitCircle.center model.screenSize.width
            , viewYAxis model.unitCircle.center model.screenSize.height
            ]
        ]


viewYAxis center height =
    line
        [ SVGA.x1 (center |> second |> fromFloat)
        , SVGA.y1 (0 |> fromFloat)
        , SVGA.x2 (center |> second |> fromFloat)
        , SVGA.y2 (height |> fromFloat)
        , SVGA.strokeWidth "1"
        , SVGA.stroke "red"
        , SVGA.strokeDasharray "5, 5"
        ]
        []


viewXAxis center width =
    line
        [ SVGA.x1 (0 |> fromFloat)
        , SVGA.y1 (center |> first |> fromFloat)
        , SVGA.x2 (width |> fromFloat)
        , SVGA.y2 (center |> first |> fromFloat)
        , SVGA.strokeWidth "1"
        , SVGA.stroke "red"
        , SVGA.strokeDasharray "5, 5"
        ]
        []


viewBoundingBox topLeftPoint radius =
    rect
        [ SVGA.y (topLeftPoint |> first |> fromFloat)
        , SVGA.x (topLeftPoint |> second |> fromFloat)
        , SVGA.height (radius * 2 |> fromFloat)
        , SVGA.width (radius * 2 |> fromFloat)
        , SVGA.stroke "black"
        , SVGA.fillOpacity "0"
        ]
        []


viewCenter center =
    circle
        [ SVGA.cx (center |> second |> fromFloat)
        , SVGA.cy (center |> first |> fromFloat)
        , SVGA.r "6"
        ]
        []


viewUnitCircle center radius =
    circle
        [ SVGA.cx (center |> second |> fromFloat)
        , SVGA.cy (center |> first |> fromFloat)
        , SVGA.r (radius |> fromFloat)
        , SVGA.fillOpacity "0"
        , SVGA.stroke "black"
        ]
        []
