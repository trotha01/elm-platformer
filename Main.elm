module Main exposing (..)

import AnimationFrame exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard.Extra as Keyboard exposing (..)
import Time exposing (Time)


type alias Model =
    { player : Player
    , pressedKeys : List Key
    , collidables : List Rect
    }


type alias Rect =
    -- (x,y) = top-left corner
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type alias Player =
    { dim : Rect
    , yVel : Float
    }


initPlayer : Player
initPlayer =
    { dim =
        { x = 10
        , y = 100
        , width = 10
        , height = 10
        }
    , yVel = 0
    }


( viewportHeight, viewportWidth ) =
    ( 400, 600 )


wallThickness =
    10


ground : Rect
ground =
    { x = 0
    , y = viewportHeight
    , width = viewportWidth
    , height = wallThickness
    }


leftWall : Rect
leftWall =
    { x = 0
    , y = 0
    , width = wallThickness
    , height = viewportHeight
    }


rightWall : Rect
rightWall =
    { x = viewportWidth
    , y = 0
    , width = wallThickness
    , height = viewportHeight
    }


init : ( Model, Cmd Msg )
init =
    ( { player = initPlayer
      , pressedKeys = []
      , collidables = [ ground, leftWall, rightWall ]
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

                player =
                    updatePlayer delta arrows model.collidables model.player
            in
            ( { model | player = player }, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Keyboard.update keyMsg model.pressedKeys }, Cmd.none )


speed : Float
speed =
    0.2


gravity : Float
gravity =
    0.005


updatePlayer : Time -> Arrows -> List Rect -> Player -> Player
updatePlayer delta arrows rects player =
    let
        playerDim =
            player.dim

        desiredX =
            playerDim.x + round (delta * speed * toFloat arrows.x)

        desiredYVel =
            if arrows.y == 1 && round player.yVel == 0 then
                -3
            else
                player.yVel + (delta * gravity)

        desiredY =
            playerDim.y + round desiredYVel

        -- |> (\y -> y + round (delta * speed * toFloat -arrows.y))
        desiredDim =
            { playerDim | x = desiredX, y = desiredY }

        desiredPlayer =
            { player | dim = desiredDim, yVel = desiredYVel }

        isColliding =
            List.any (collidesWith desiredDim) rects
    in
    if isColliding then
        { player | yVel = 0 }
    else
        desiredPlayer


collidesWith : Rect -> Rect -> Bool
collidesWith rect1 rect2 =
    (rect1.x < rect2.x + rect2.width)
        && (rect1.x + rect1.width > rect2.x)
        && (rect1.y < rect2.y + rect2.height)
        && (rect1.height + rect1.y > rect2.y)


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
        [ viewPlayer model.player
        , div [] (List.map viewCollidable model.collidables)
        ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "width", "10px" )
            , ( "height", "10px" )
            , ( "border", "1px solid blue" )
            , ( "background", "lightblue" )
            , ( "left", px player.dim.x )
            , ( "top", px player.dim.y )
            ]
        ]
        []


viewCollidable : Rect -> Html Msg
viewCollidable rect =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "width", px rect.width )
            , ( "height", px rect.height )
            , ( "border", "1px solid black" )
            , ( "background", "gray" )
            , ( "left", px rect.x )
            , ( "top", px rect.y )
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
