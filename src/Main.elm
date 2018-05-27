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
    , velocity : Float
    , direction : Direction
    , state : EntityState
    }


type Direction
    = Left
    | Right


type EntityState
    = Idle
    | InAir


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
    , keysPressed : List KeyCode
    , collided : String
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
      , mario = { x = 80, y = 32, velocity = 0, direction = Right, state = Idle }
      , keysPressed = []
      , collided = "Nope."
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
                updatedTime =
                    { model | elapsedTime = model.elapsedTime + (dt / 1000) }

                movedMario =
                    model.mario
                        |> updateMarioVelocity dt model.keysPressed
                        |> moveMario dt model.keysPressed
                        |> applyGravity

                collidedTiles =
                    calculateTilesCollisions movedMario

                collidedMario =
                    collideMario movedMario collidedTiles
            in
                ( { model
                    | elapsedTime = model.elapsedTime + (dt / 1000)
                    , mario = collidedMario
                    , collided = collidedTiles |> List.map (\t -> toString t.code) |> String.join ", "
                  }
                , Cmd.none
                )

        KeyDown keyCode ->
            ( { model | keysPressed = keyCode :: model.keysPressed }, Cmd.none )

        KeyUp keyCode ->
            ( { model | keysPressed = model.keysPressed |> List.filter (\k -> k /= keyCode) }, Cmd.none )


applyGravity : Entity -> Entity
applyGravity entity =
    { entity | y = entity.y + 1 }


calculateTilesCollisions : Entity -> List Tile
calculateTilesCollisions mario =
    let
        gridSize =
            16

        collidableTile : Tile -> Bool
        collidableTile t =
            List.member t.code [ 0, 1, 297, 298, 264, 265 ]

        collidesWithTile : Tile -> Bool
        collidesWithTile t =
            (mario.x + gridSize - 2 > toFloat t.x * gridSize && mario.x + 2 < toFloat (t.x + 1) * gridSize)
                && (mario.y + gridSize > toFloat t.y * gridSize && mario.y < toFloat (t.y + 1) * gridSize)
    in
        lvl1
            |> List.filter collidableTile
            |> List.filter collidesWithTile


collideMario : Entity -> List Tile -> Entity
collideMario mario tiles =
    let
        marioGridX =
            round (mario.x / 16)

        marioGridY =
            round (mario.y / 16)

        noCornerTiles =
            tiles |> List.filter (\t -> t.y == marioGridY || t.x == marioGridX)

        anyTileBelow =
            not (tiles |> List.filter (\t -> t.y > marioGridY) |> List.isEmpty)

        anyTileRight =
            not (noCornerTiles |> List.filter (\t -> t.x > marioGridX) |> List.isEmpty)

        anyTileLeft =
            not (noCornerTiles |> List.filter (\t -> t.x < marioGridX) |> List.isEmpty)
    in
        case tiles of
            [] ->
                mario

            tiles ->
                if anyTileBelow && anyTileRight then
                    { mario | x = toFloat marioGridX * 16 + 2, y = toFloat marioGridY * 16, state = Idle }
                else if anyTileBelow && anyTileLeft then
                    { mario | x = toFloat marioGridX * 16 - 2, y = toFloat marioGridY * 16, state = Idle }
                else if anyTileRight then
                    { mario | x = toFloat marioGridX * 16 + 2 }
                else if anyTileLeft then
                    { mario | x = toFloat marioGridX * 16 - 2 }
                else if anyTileBelow then
                    { mario | y = toFloat marioGridY * 16, state = Idle }
                else
                    mario



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
            , Html.div [] [ text (model.keysPressed |> List.map toString |> String.join ", ") ]
            , Html.div [] [ text model.collided ]
            ]


updateMarioVelocity : Time -> List KeyCode -> Entity -> Entity
updateMarioVelocity dt keysPressed mario =
    let
        leftArrow =
            37

        rightArrow =
            39

        velocity =
            0.1

        keyPressed =
            List.head keysPressed
    in
        if keyPressed == Just leftArrow then
            { mario | velocity = -velocity, direction = Left }
        else if keyPressed == Just rightArrow then
            { mario | velocity = velocity, direction = Right }
        else
            { mario | velocity = 0 }


moveMario : Time -> List KeyCode -> Entity -> Entity
moveMario dt keysPressed mario =
    let
        topArrow =
            38

        keyPressed =
            List.head keysPressed
    in
        if keyPressed == Just topArrow && mario.state /= InAir then
            { mario | y = mario.y - 32, state = InAir }
        else
            { mario | x = mario.x + mario.velocity * dt }


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

            -- , rect [ x "276px", y "44px", width "16px", height "16px", Svg.Attributes.style "fill-opacity:0;stroke:black;stroke-width:1;" ] []
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
