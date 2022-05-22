module Main exposing (..)

{- exposing (setColoredSquare) -}

import Browser
import Browser.Events
import Functions exposing (flip)
import Html exposing (Html, button, text, select, option, input, span)
import Html.Attributes as Attributes exposing (checked, type_, value)
import Html.Events as Events exposing (onCheck, onClick, onInput)
import Json.Decode as Decode
import Random exposing (Generator)
import Setters
import Time exposing (Posix)
import Update
import Html exposing (select)


{-| Got from JS side, and Model to modify
-}
type alias Flags =
    { now : Int }


type alias Snake =
    { row : Int, column : Int }


type alias Apple =
    { row : Int, column : Int, value : Int }

type alias Settings =
    { borderWall : Bool, randomWall : Bool, gridGame : Int}

type alias HightScore = Int


type alias Model =
    { gameStarted : Bool
    , lastUpdate : Int
    , time : Int
    , coloredSquare : Int
    , snake : List Snake
    , currentDirection : Direction
    , bonusApple : Snake
    , eatenAppleList : List Snake
    , score : Int
    , settings : Settings
    , hightScore : List HightScore
    -- , bonus : Bonus
    }


initSnake : List Snake
initSnake =
    [ --{ row = 0, column = 0 }
        { row = 10, column = 5 }

    --bonusConstructor
    --, { row = 11, column = 5 }
    -- , { row = 7, column = 5 }
    -- , { row = 8, column = 5 }
    ]


initApple : Snake
initApple =
    { row = -1, column = -1 }


initEatenApple : List Snake
initEatenApple =
    []


defaultSettings : Settings
defaultSettings = 
    { borderWall = False
    , randomWall = False
    , gridGame = 16
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    now
        |> (\time ->
                Model False time time 0 initSnake Left initApple initEatenApple 0 
                        defaultSettings [10,13,15]
                    |> Update.none
           )



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
    | AddBorder Bool
    | AddRandomWall Bool
    | ChoiceGrid String
    | GetApple Snake


type Direction
    = Left
    | Up
    | Right
    | Down


type SettingGame
    = Border
    | RandomWall
    --| Grid

{-| Manage all your updates here, from the main update function to each
-| subfunction. You can use the helpers in Update.elm to help construct
-| Cmds.
-}
generateApple : Settings -> Random.Generator Snake
generateApple settings =
     let outValue = toFloat(settings.gridGame)
                            |> (/) 400.0
                            |>floor
                            |> flip (-) 1
    in

    --Random.map2 (\row column -> Snake row column) (Random.int 0 19) (Random.int 0 19)
    Random.map2 (\row column -> Snake row column) (Random.int 0 outValue) (Random.int 0 outValue)

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
    generateApple model.settings
        |> Random.generate GetApple
        |> flip Update.withCmd model


updateApple : Snake -> Model -> Model
updateApple value ({ coloredSquare } as model) =
    if coloredSquare == 0 then
        let
            newApple = value

            --{ row = 4, column = 8 }
            --randomApple
        in
        { model | bonusApple = newApple }

    else if coloredSquare >= 20 then
        { model | bonusApple = { row = -1, column = -1 } }

    else
        model


updateCell : Direction -> Snake -> Settings -> Snake
updateCell direction snake settings=
    let outValue = toFloat(settings.gridGame)
                            |> (/) 400.0
                            |>floor
            in
    case direction of
        Left ->
            if snake.row - 1 < 0 && not settings.borderWall then {snake | row = outValue - 1}

            else
                { snake
                    | row = snake.row - 1
                }

        Right ->
            if snake.row + 1 > outValue - 1 && not settings.borderWall then {snake | row = 0}

            else
            { snake
                | row = snake.row + 1
            }

        Up ->
            if snake.column - 1 < 0 && not settings.borderWall then {snake | column = outValue - 1}

            else
            { snake
                | column = snake.column - 1
            }

        --snake
        Down ->
            if snake.column + 1 > outValue - 1 && not settings.borderWall then {snake | column = 0}

            else
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
                    , bonusApple = { row = -1, column = -1 }
                    , score = model.score + 100
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



updateSnake : Model -> Model
updateSnake ({ snake, currentDirection, score, settings} as model) =
    case snake of
        [] ->
            model

        hd :: _ ->
            let
                newSnake =
                    if List.length snake == 1 then
                        List.map (\a -> updateCell currentDirection a settings) snake

                    else
                        List.length snake
                            - 1
                            |> flip List.take snake
                            |> (::) (updateCell currentDirection hd settings)

            in
            { model | snake = newSnake, score = score + 3 }



toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
    not gameStarted
        |> Setters.setGameStartedIn model
        |> Update.none



changeDirection : Direction -> Model -> Model
changeDirection newDirection ({ currentDirection, snake } as model) =
    if List.length snake == 1 then
        { model | currentDirection = newDirection }

    else
        case currentDirection of
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



isSnakeHitBorder : Model -> Bool
isSnakeHitBorder {snake, settings} =
    case snake of
        [] -> True
        hd::_ -> 
            let outValue = toFloat(settings.gridGame)
                            |> (/) 400.0
                            |>floor
            in
            if hd.row == -1 || hd.row == outValue || hd.column == -1 || hd.column == outValue then
                        True
            else 
                False 


isSnakeHitSelf : Model -> Bool
isSnakeHitSelf {snake} =
    case snake of 
        [] -> True
        hd::tail -> 
            if List.member hd tail then True

            else
                        False


restModel : Model -> Model
restModel model =
    { model
        | snake = initSnake
        , gameStarted = False
        , bonusApple = initApple
        , eatenAppleList = []
        , currentDirection = Left
        , score = 0
    }

sortedScores : Int -> Int -> Order
sortedScores val1 val2 =
    case compare val1 val2 of
        GT -> LT
        LT -> GT
        EQ -> EQ
        
        

updateScore : Int -> Model -> Model
updateScore score ({hightScore} as model)=
    case hightScore of
        [] -> {model | hightScore = List.singleton score}
        _::_ -> --{model | hightScore = List.sort}
            let scoreList = List.singleton score 
                        |> (++) hightScore
                        |> List.sortWith sortedScores 

            in
                if List.length hightScore < 5 then 
                    {model | hightScore = scoreList}
                else
                    model


endGame : Model -> Model
endGame ({ snake, settings} as model) =
    let
        isSnakeHitSomething =
            case snake of
                [] ->
                    True

                hd :: tail ->
                    if isSnakeHitBorder model && settings.borderWall then
                        True

                    else if List.member hd tail then
                        True

                    else
                        False
    in
    if isSnakeHitSomething then
        updateScore model.score model|> restModel
    else
        model


gamePlay : Model -> Model
gamePlay model =
    updateSquare model
        --|> updateApple
        --|> GetApple
        |> isAppleEaten model.bonusApple
        |> growthSnake
        |> updateSnake
        |> endGame


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
            |> updateSnake
            |> isAppleEaten model.bonusApple
            |> endGame
            |> growthSnake
            --gamePlay model
            |> Setters.setTime time_
            |> Setters.setLastUpdate time_
            --|> Update.none
            |> executdeAppleCmd

    else
        time_
            |> Setters.setTimeIn model
            |> Update.none


setSettings : SettingGame -> Bool -> Settings-> Settings
setSettings valuetype value settings=
    case valuetype of
        Border -> {settings | borderWall = value}
        RandomWall -> {settings | randomWall = value}


changeGrid : Int -> Settings-> Settings
changeGrid value settings=
    {settings | gridGame = value}
        

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

        GetApple value ->
            updateApple value model |> Update.none

        AddBorder value ->
            let
                settings = setSettings Border value model.settings
            in
            { model | settings = settings } 
            |> restModel
            |> Update.none
        
        AddRandomWall value ->
            let
                settings = setSettings RandomWall value model.settings
            in
            { model | settings = settings} 
            |> restModel
            |> Update.none
        
        ChoiceGrid value ->
            let
                setting = 
                    case String.toInt value of
                        Nothing -> model.settings
                        Just a -> changeGrid a model.settings
            in
            { model | settings = setting} 
            |> restModel
            |> Update.none
            --model |> Update.none
            

checkBonusInSnake : Snake -> List Snake -> Bool
checkBonusInSnake bonus listSnake =
    List.member bonus listSnake


{-| Manage all your view functions here.
-}


cell : Snake ->Int -> String -> Html msg
cell snake cellSize nameClass =
    let
        depRow =
            snake.row
                * cellSize
                |> String.fromInt

        depHeight =
            snake.column
                * cellSize
                |> String.fromInt
        
        size = (String.fromInt cellSize) ++ "px"
    in
    Html.div
        [ Attributes.style "width" size
        , Attributes.style "height" size
        , Attributes.style "left" (depRow ++ "px")
        , Attributes.style "top" (depHeight ++ "px")
        , Attributes.class nameClass
        ]
        []


movingSnake : Model -> List (Html msg)
movingSnake { snake, settings} =
    List.map (\a -> cell a settings.gridGame "snake") snake


displayGameOver : Model -> Html msg
displayGameOver model =
    Html.div [ Attributes.class "game-over" ]
        [ text "Game Over"
        , Html.br [] []
        , String.fromInt model.score |> text
        ]


movingSquare : Model -> Html msg
movingSquare ({ bonusApple, settings} as model) =
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
        (if bonusApple.row == -1 then
            movingSnake model

         else
            cell bonusApple settings.gridGame "apple"
                |> flip (::) (movingSnake model)
        )


actualTime : Model -> Html Msg
actualTime { time } =
    Html.div [ Attributes.class "actual-time" ]
        [ Html.text "Actual time"
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


wallCheckbox : Model -> Html Msg
wallCheckbox { settings } =
    Html.div []
        [ text "Add wall border"
        , Html.input [ onCheck AddBorder, type_ "checkbox", checked settings.borderWall ] []

        , if settings.borderWall then
            text "activated wall"
            
          else
            text "inactivated wall"
        ]


gridList : List Int
gridList = [16,20,25,40]


optionalList : Int -> Html Msg
optionalList grid =
    option[ value <|String.fromInt grid][text <|String.fromInt grid]

gridChoice : Model -> Html Msg
gridChoice model =
    let options = List.map(\a -> optionalList a) gridList
    in
    Html.div[]
        [ select[onInput ChoiceGrid]  options
        , text <|String.fromInt model.settings.gridGame
        ]


playerScore : (Int, Int) -> Html Msg
playerScore value=
    --nameclass player-score
    Html.div[Attributes.class "player-score"]
        [ span[Attributes.style "boder-right" "1px solid black"][text <|String.fromInt <|Tuple.first value]
        , span[][text <|String.fromInt <|Tuple.second value]
        ]   

--convertList : List Int -> List (Int)

displayHighScore : Model -> Html Msg
displayHighScore model =
    let display = List.indexedMap Tuple.pair model.hightScore
                |> List.map(\a -> playerScore a)
    in
    Html.div[Attributes.class "all-player-scores"] display

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
        , wallCheckbox model
        , gridChoice model
        , movingSquare model
        , displayScore model
        , displayHighScore model
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
