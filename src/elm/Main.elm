module Main exposing (..)

{- exposing (setColoredSquare) -}

import Browser
import Browser.Events
import Functions exposing (flip)
import Html exposing (Html, button, text)
import Html.Attributes as Attributes
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode
import Random exposing (Generator)
import Setters
import Time exposing (Posix)
import Update


{-| Got from JS side, and Model to modify
-}
type alias Flags =
    { now : Int }


type alias Snake =
    { row : Int, column : Int }


type alias Bonus =
    { row : Int, column : Int, value : Int }


type Direction
    = Left
    | Up
    | Right
    | Down


type alias Model =
    { gameStarted : Bool
    , lastUpdate : Int
    , time : Int
    , coloredSquare : Int
    , snake : List Snake
    , currentDirection : Direction
    , apple : Snake
    , eatenAppleList : List Snake
    , score : Int

    -- , bonus : Bonus
    }


initialSnake : List Snake
initialSnake =
    [ { row = 5, column = 5 }

    --bonusConstructor
    -- , { row = 6, column = 5 }
    -- , { row = 7, column = 5 }
    -- , { row = 8, column = 5 }
    ]


initialApple : Snake
initialApple =
    { row = 4, column = 8 }


initialEatenApple : List Snake
initialEatenApple =
    []


init : Flags -> ( Model, Cmd Msg )
init { now } =
    now
        |> (\time ->
                Model False time time 0 initialSnake Left initialApple initialEatenApple 0
                    |> Update.none
           )



-- [ { row = 5, column = 5 }, { row = 6, column = 5 } ]


{-| All your messages should go there
-}
type Key
    = ArrowUp
    | ArrowRight
    | ArrowDown
    | ArrowLeft
    | Space


type Msg
    = NextFrame Posix
    | ToggleGameLoop
    | KeyDown Key
      --| NewApple
    | GetApple Snake



-- | GenerateBonus
-- | NewBonus Snake
-- type IntOrSnake
--     = Int Int
--     | Snake { row : Int, column : Int }


{-| Manage all your updates here, from the main update function to each
-| subfunction. You can use the helpers in Update.elm to help construct
-| Cmds.
-}
generateApple : Random.Generator Snake
generateApple =
    -- Random.map2
    --     (\row column -> Snake row column)
    --     (Random.int 5 7)
    --     (Random.int 5 7)
    Random.map2 (\row column -> Snake row column) (Random.int 0 20) (Random.int 0 20)



-- newBonus : Cmd Msg
-- newBonus =
--     Random.generate NewBonus bonusConstructor
-- updateBonus : Bonus -> Int -> Bonus
-- updateBonus bonus random =
--     { bonus | row = random, column = random }


updateSquare : Model -> Model
updateSquare ({ coloredSquare } as model) =
    coloredSquare
        + 1
        |> modBy 30
        -- modBy is the operator modulo
        |> Setters.setColoredSquareIn model


executdeAppleCmd : Model -> ( Model, Cmd Msg )
executdeAppleCmd model =
    --Random.generate GetApple generateApple
    generateApple
        |> Random.generate GetApple
        |> flip Update.withCmd model


updateApple : Snake -> Model -> Model
updateApple value ({ coloredSquare } as model) =
    if coloredSquare == 0 then
        let
            newApple =
                value

            --{ row = 4, column = 8 }
            --randomApple
        in
        { model | apple = newApple }

    else if coloredSquare >= 20 then
        { model | apple = { row = -1, column = -1 } }

    else
        model


updateCell : Direction -> Snake -> Snake
updateCell direction snake =
    case direction of
        Left ->
            { snake
                | row = snake.row - 1
            }

        Right ->
            { snake
                | row = snake.row + 1
            }

        Up ->
            { snake
                | column = snake.column - 1
            }

        --snake
        Down ->
            { snake
                | column = snake.column + 1
            }


isAppleEaten : Snake -> Model -> Model
isAppleEaten apple ({ snake, eatenAppleList } as model) =
    case snake of
        [] ->
            model

        hd :: _ ->
            if hd /= apple then
                model

            else
                let
                    newEatenApple =
                        List.singleton apple
                            |> (++) eatenAppleList
                in
                { model
                    | eatenAppleList = newEatenApple
                    , apple = { row = -1, column = -1 }
                    , score = model.score + 1
                }


growthSnake : Model -> Model
growthSnake ({ snake, eatenAppleList } as model) =
    case eatenAppleList of
        [] ->
            model

        hd :: tl ->
            let
                lastCellSnake =
                    List.length snake
                        - 1
                        |> flip List.drop snake
            in
            if List.length snake == 1 then
                { model
                    | snake = snake ++ List.singleton hd
                    , eatenAppleList = tl
                }

            else if List.singleton hd == lastCellSnake then
                { model
                    | snake = snake ++ List.singleton hd
                    , eatenAppleList = tl
                }

            else
                model



-- let
--     newSnake =
--         if tl == [] then
--             List.singleton hd
--                 |> (::) hd
--         else
--             List.length snake
--                 - 1
--                 |> flip List.drop snake
--                 |> flip (++) snake
-- in
-- { model | snake = newSnake, apple = { row = -1, column = -1 } }


updateSnake : Model -> Model
updateSnake ({ snake, currentDirection } as model) =
    case snake of
        [] ->
            model

        hd :: _ ->
            let
                newSnake =
                    if List.length snake == 1 then
                        List.map (\a -> updateCell currentDirection a) snake

                    else
                        List.length snake
                            - 1
                            |> flip List.take snake
                            |> (::) (updateCell currentDirection hd)

                -- newSnake2 =
                --     List.length snake
                --         - 1
                --         |> flip List.take snake
                --         |> (::) (updateCell currentDirection hd)
            in
            -- if List.length snake == 1 then
            --     { model | snake = newSnake1 }
            -- else
            --     { model | snake = newSnake2 }
            { model | snake = newSnake }


toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
    not gameStarted
        |> Setters.setGameStartedIn model
        |> Update.none


changeDirection : Direction -> Model -> Model
changeDirection newDirection model =
    -- case model.currentDirection of
    --     Left -> if newDirection == Right then model
    case model.currentDirection of
        Left ->
            if newDirection == Right then
                model

            else
                { model | currentDirection = newDirection }

        Right ->
            if newDirection == Left then
                model

            else
                { model | currentDirection = newDirection }

        Up ->
            if newDirection == Down then
                model

            else
                { model | currentDirection = newDirection }

        Down ->
            if newDirection == Up then
                model

            else
                { model | currentDirection = newDirection }


endGame : Model -> Model
endGame ({ snake } as model) =
    let
        isHitWallOrItself =
            case snake of
                [] ->
                    True

                hd :: tail ->
                    if hd.row == -1 || hd.row == 20 || hd.column == -1 || hd.column == 20 then
                        True

                    else if List.member hd tail then
                        True

                    else
                        False
    in
    if isHitWallOrItself then
        { model
            | snake = initialSnake
            , gameStarted = False
            , apple = initialApple
            , eatenAppleList = []
            , currentDirection = Left
            , score = 0
        }

    else
        model


keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
    case Debug.log "key" key of
        Space ->
            update ToggleGameLoop model

        -- utiliser la boucle ciclyque pour continuer a bouger le snake
        ArrowLeft ->
            Update.none (changeDirection Left model)

        ArrowRight ->
            Update.none (changeDirection Right model)

        ArrowUp ->
            Update.none (changeDirection Up model)

        ArrowDown ->
            Update.none (changeDirection Down model)



-- _ ->
--     Update.none model

lauchDiceRoll : Model -> (Model, Cmd Msg)
lauchDiceRoll model =
  Update.withCmd newBonus model

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
    let
        time_ =
            Time.posixToMillis time
    in
    if time_ - model.lastUpdate >= 300 then
        --1000
        updateSquare model
            --|> updateApple
            --|> GetApple
            |> isAppleEaten model.apple
            |> growthSnake
            |> updateSnake
            |> endGame
            |> Setters.setTime time_
            |> Setters.setLastUpdate time_
            --|> Update.none
            |> executdeAppleCmd

    else
        time_
            |> Setters.setTimeIn model
            |> Update.none


{-| Main update function, mainly used as a router for subfunctions
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleGameLoop ->
            toggleGameLoop model

        KeyDown key ->
            keyDown key model

        NextFrame time ->
            nextFrame time model

        -- NewApple ->
        --     ( model, Random.generate GetApple generateApple )
        GetApple value ->
            updateApple value model |> Update.none



--{ model | apple = value } |> Update.none
-- NewApple row column ->
--     model |> Update.none
{- NewBonus generatedBonus ->
       -- let
       --     test =
       --         { row = 5, column = 5 }
       -- in
       if checkBonusInSnake (Debug.log "generated bonus" generatedBonus) model.snake then
           update GenerateBonus model

       else
           model |> Update.none

   GenerateBonus ->
       ( model, newBonus )
-}


checkBonusInSnake : Snake -> List Snake -> Bool
checkBonusInSnake bonus listSnake =
    List.member bonus listSnake


{-| Manage all your view functions here.
-}



-- transform : Snake -> Int
-- transform snakeElment =
--     --(snakeElment.row - 1) * 10 + snakeElment.column
--     0
-- cell : Int -> Int -> Html msg
-- cell index active =
--     let
--         class =
--             if active == index then
--                 "cell active"
--             else
--                 "cell"
--     in
--     Html.div [ Attributes.class class ] []
-- movingSnake : Model -> List (Html msg)
-- movingSnake model =
--     List.map (\a -> cell (transform a) model.coloredSquare) model.snake


cell : Snake -> String -> Html msg
cell snake nameClass =
    let
        depRow =
            snake.row
                * 20
                |> String.fromInt

        depHeight =
            snake.column
                * 20
                |> String.fromInt
    in
    Html.div
        [ {- Attributes.style "position" "absolute"
             , Attributes.style "width" "20px"
             , Attributes.style "height" "20px"
          -}
          Attributes.style "left" (depRow ++ "px")
        , Attributes.style "top" (depHeight ++ "px")
        , Attributes.class nameClass
        ]
        []


movingSnake : Model -> List (Html msg)
movingSnake { snake } =
    List.map (\a -> cell a "snake") snake



-- Html.div [] value


movingSquare : Model -> Html msg
movingSquare ({ apple } as model) =
    Html.div [ Attributes.class "grid" ]
        -- [ cell 1 coloredSquare
        -- , cell 1 coloredSquare
        -- , cell 2 coloredSquare
        -- , cell 3 coloredSquare
        -- , cell 4 coloredSquare
        -- , cell 5 coloredSquare
        -- , cell 6 coloredSquare
        -- , cell 7 coloredSquare
        -- , cell 8 coloredSquare
        -- , cell 9 coloredSquare
        -- ]
        -- [ cellSnake { row = 1, column = 5 }
        -- , cell 0 coloredSquare
        -- ]
        --(movingSnake model)
        -- if apple.row == -1 then cell apple "apple"
        -- else
        (cell apple "apple"
            |> flip (::) (movingSnake model)
        )


actualTime : Model -> Html Msg
actualTime { time } =
    Html.div [ Attributes.class "actual-time" ]
        [ Html.text "Actual time"

        --, button [ onClick GenerateBonus ] [ text "generate random" ]
        , time
            |> String.fromInt
            |> Html.text
            |> List.singleton
            |> Html.code []
        ]


explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
    let
        word =
            if gameStarted then
                "Stop"

            else
                "Start"
    in
    Html.div [ Attributes.class "separator" ]
        [ Html.h1 []
            [ Html.text "Welcome to the snake project!" ]
        , actualTime model
        , Html.button
            [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
            [ Html.text (String.join " " [ word, "game loop" ]) ]
        ]


displayScore : Model -> Html msg
displayScore { score } =
    Html.div [ Attributes.class "score" ]
        [ String.fromInt score
            |> Html.text
        ]


{-| Main view functions, composing all functions in one
-}
view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.img [ Attributes.src "/logo.svg" ] []
        , explanations model
        , movingSquare model
        , displayScore model
        ]


{-| Parts for the runtime. Get key presses and subscribe to
-| requestAnimationFrame for the game loop. You don't have to bother with
-| this.
-}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
    case value of
        "ArrowUp" ->
            Decode.succeed ArrowUp

        "ArrowLeft" ->
            Decode.succeed ArrowLeft

        "ArrowRight" ->
            Decode.succeed ArrowRight

        "ArrowDown" ->
            Decode.succeed ArrowDown

        " " ->
            Decode.succeed Space

        _ ->
            Decode.fail "Not an arrow"


decodeKey : Decode.Decoder Msg
decodeKey =
    Decode.field "key" Decode.string
        |> Decode.andThen decodeArrow
        |> Decode.map KeyDown


subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
    let
        aF =
            Browser.Events.onAnimationFrame NextFrame

        base =
            Browser.Events.onKeyDown decodeKey :: []
    in
    Sub.batch
        (if gameStarted then
            aF :: base

         else
            base
        )


{-| Entrypoint of your program
-}
main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
