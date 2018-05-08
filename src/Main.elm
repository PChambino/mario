module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import AnimationFrame
import Keyboard exposing (KeyCode)
import Time exposing (Time)


---- MODEL ----


type alias Entity =
    { x : Float
    , y : Float
    , direction : Direction
    }


type Direction
    = Left
    | Right


type alias Tile =
    { x : Int
    , y : Int
    , code : Int
    }


type alias Model =
    { charactersPath : String
    , tilesPath : String
    , elapsedTime : Float
    , mario : Entity
    , keyPressed : String
    }


type alias Flags =
    { charactersPath : String
    , tilesPath : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { charactersPath = flags.charactersPath
      , tilesPath = flags.tilesPath
      , elapsedTime = 0
      , mario = { x = 0, y = 144, direction = Right }
      , keyPressed = "Nothing pressed"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | TimeUpdate Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            let
                updatedModel =
                    { model | elapsedTime = model.elapsedTime + (dt / 1000) }
            in
                ( { updatedModel | mario = moveMario dt model.keyPressed model.mario }, Cmd.none )

        KeyDown keyCode ->
            ( { model | keyPressed = toString keyCode }, Cmd.none )

        KeyUp keyCode ->
            ( { model | keyPressed = "Nothing pressed" }, Cmd.none )



---- VIEW ----


lvl1 =
    List.concat
        [ List.range 0 15 |> List.concatMap (\i -> [ { code = 0, x = i, y = 10 }, { code = 0, x = i, y = 11 } ])
        , [ { code = 693, x = 4, y = 1 }, { code = 660, x = 4, y = 0 }, { code = 694, x = 5, y = 1 }, { code = 661, x = 5, y = 0 }, { code = 695, x = 6, y = 1 }, { code = 662, x = 6, y = 0 } ]
        , [ { code = 24, x = 1, y = 6 }, { code = 1, x = 5, y = 6 }, { code = 24, x = 6, y = 6 }, { code = 1, x = 7, y = 6 }, { code = 24, x = 8, y = 6 }, { code = 1, x = 9, y = 6 }, { code = 24, x = 7, y = 3 } ]
        , [ { code = 272, x = 1, y = 9 }, { code = 307, x = 2, y = 9 }, { code = 274, x = 3, y = 9 }, { code = 273, x = 2, y = 8 } ]
        , [ { code = 308, x = 8, y = 9 }, { code = 309, x = 9, y = 9 }, { code = 310, x = 10, y = 9 } ]
        , [ { code = 297, x = 13, y = 9 }, { code = 298, x = 14, y = 9 }, { code = 264, x = 13, y = 8 }, { code = 265, x = 14, y = 8 } ]
        ]


view : Model -> Html Msg
view model =
    let
        frame =
            (round model.elapsedTime) % 10
    in
        Html.div []
            [ svg
                [ width "800px", viewBox "0 0 256 192" ]
                (List.concat
                    [ [ rect [ width "100%", height "100%", fill "#529AFF" ] [] ]
                    , (drawLevel lvl1 model.tilesPath)
                    , [ drawMario model.mario model.charactersPath ]
                    ]
                )
            , Html.div [] [ text model.keyPressed ]
            ]


moveMario : Time -> String -> Entity -> Entity
moveMario dt keyPressed mario =
    let
        leftArrow =
            "37"

        rightArrow =
            "39"
    in
        if keyPressed == leftArrow then
            { mario | x = mario.x - dt / 10, direction = Left }
        else if keyPressed == rightArrow then
            { mario | x = mario.x + dt / 10, direction = Right }
        else
            mario


drawLevel : List Tile -> String -> List (Svg Msg)
drawLevel lvl spritesPath =
    List.map (\tile -> drawTile tile.code tile.x tile.y spritesPath) lvl


drawTile : Int -> Int -> Int -> String -> Svg Msg
drawTile code gridX gridY spritesPath =
    let
        size =
            16

        imageWidth =
            528

        spritesPerLine =
            imageWidth // size

        screenX =
            gridX * size

        screenY =
            gridY * size

        tileX =
            (code % spritesPerLine) * size

        tileY =
            (code // spritesPerLine) * size

        tileBox =
            [ tileX, tileY, size, size ] |> List.map toString |> String.join " "

        px n =
            (toString n) ++ "px"
    in
        svg
            [ x (toString screenX), y (toString screenY), width (px size), height (px size), viewBox tileBox, version "1.1" ]
            [ image [ x "0px", y "0px", width (px imageWidth), height "448px", xlinkHref spritesPath, imageRendering "pixelated" ] []
            ]


drawMario : Entity -> String -> Svg Msg
drawMario mario spritesPath =
    let
        spriteWidth =
            16

        spriteHeight =
            16

        marioLeftSprite =
            "222 44 16 16"

        marioRightSprite =
            "276 44 16 16"

        spritePosition =
            case mario.direction of
                Left ->
                    marioLeftSprite

                Right ->
                    marioRightSprite
    in
        svg [ x (toString mario.x), y (toString mario.y), width "16px", height "16px", viewBox spritePosition, version "1.1" ]
            [ image [ x "0px", y "0px", width "513px", height "401px", xlinkHref spritesPath, imageRendering "pixelated" ] []
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
