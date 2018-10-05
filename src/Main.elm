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



-- import Svg.Attributes as SvgA
-- MAIN


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
            [ rect
                [ SVGA.y (model.unitCircle.boundingBoxTopLeft |> first |> fromFloat)
                , SVGA.x (model.unitCircle.boundingBoxTopLeft |> second |> fromFloat)
                , SVGA.height (model.unitCircle.radius * 2 |> fromFloat)
                , SVGA.width (model.unitCircle.radius * 2 |> fromFloat)
                , SVGA.stroke "black"
                , SVGA.fillOpacity "0"
                ]
                []
            , circle
                [ SVGA.cx (model.unitCircle.center |> second |> fromFloat)
                , SVGA.cy (model.unitCircle.center |> first |> fromFloat)
                , SVGA.r "6"
                ]
                []
            , circle
                [ SVGA.cx (model.unitCircle.center |> second |> fromFloat)
                , SVGA.cy (model.unitCircle.center |> first |> fromFloat)
                , SVGA.r (model.unitCircle.radius |> fromFloat)
                , SVGA.fillOpacity "0"
                , SVGA.stroke "black"
                ]
                []
            ]
        ]
