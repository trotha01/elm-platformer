module Main exposing (..)

import AnimationFrame exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Extra as Keyboard exposing (..)
import Time exposing (Time)


type alias Model =
    { player : Player
    , pressedKeys : List Key
    }


type alias Player =
    { x : Int
    , y : Int
    }


initPlayer : Player
initPlayer =
    { x = 10
    , y = 100
    }


init : ( Model, Cmd Msg )
init =
    ( { player = initPlayer
      , pressedKeys = []
      }
    , Cmd.none
    )


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                arrows =
                    Keyboard.arrows model.pressedKeys
            in
            ( { model | player = updatePlayer delta arrows model.player }, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )


speed : Float
speed =
    0.2


updatePlayer : Time -> Arrows -> Player -> Player
updatePlayer delta arrows player =
    let
        x =
            player.x + round (delta * speed * toFloat arrows.x)

        y =
            player.y + round (delta * speed * toFloat -arrows.y)
    in
    { x = x, y = y }


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "relative" )
            , ( "width", "600px" )
            , ( "height", "400px" )
            , ( "border", "1px solid black" )
            ]
        ]
        [ viewPlayer model.player ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "width", "10px" )
            , ( "height", "10px" )
            , ( "border", "1px solid blue" )
            , ( "background", "lightblue" )
            , ( "left", px player.x )
            , ( "top", px player.y )
            ]
        ]
        []


px : Int -> String
px x =
    toString x ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]
