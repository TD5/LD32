module Game where
{-| This is my game! 

The player writes a program which then controls their unit on the battlefield.

Their unit fights on their behalf until there are no friendly unit left - the
goal is to keep as many friendly units alive for as long as possible.

- The unconventional weapon is the player's logical reasoning!
-}

import Array (..)
import Debug
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import Json.Decode as Json
import List
import Maybe
import Random
import Signal
import String
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Time
import Window


---- MODEL ----
type alias ProgramSource = String

{-
The player can store an array of numbers between executions of their code.
I thought having just a list of numbers forces the user to intelligently
encode any information that they want to keep.
-}
type alias ProgramMemory = Array Int

type alias Health = Int

type Weapon 
    = Unarmed 
    | Damage Int

type alias World = -- The origin is (0, 0) and it extends out to (width - 1, height - 1) in a rectangle
    { width  : Int
    , height : Int 
    }

type alias Position = -- Coods in the world
    { x : Int
    , y : Int 
    }

type alias Entity = -- An actor in the gameworld
    { position : Position
    , health   : Health
    , weapon   : Weapon
    , canMove  : Bool
    , label    : String
    }

type Character -- An entity with an alignment to a team
    = PlayerControlled Entity
    | Good Entity -- Won't attack player or other good entities
    | Chaotic Entity -- Will attack anything, even other chaotics
    | Evil Entity -- Won't attack other evil entities

type alias ExecutingGame =
    { programMemory : ProgramMemory
    , characters    : List Character -- In order of priority to perform an action
    , score         : Maybe Int
    }

type alias Model = -- The full state of the game at any point in time
    { source         : ProgramSource
    , sourceError    : Maybe String
    , gameWorld      : World
    , executingGame  : Maybe ExecutingGame
    }

initialWorld : World
initialWorld = 
    { width  = 25
    , height = 25
    }

initialBasicEnemy : Position -> Character
initialBasicEnemy pos =
    Evil
        { position = pos
        , health   = 6
        , weapon   = Damage 2 
        , canMove  = True
        , label    = "Enemy Tank"
        }
initialBasicChaotic : Position -> Character
initialBasicChaotic pos =
    Chaotic
        { position = pos
        , health   = 2
        , weapon   = Damage 1 
        , canMove  = True
        , label    = "Rogue Soldier"
        }

{-  
A tough but unarmed, immobile friendly base that the player wants to protect
-}
initialBase : Position -> Character
initialBase pos =
    Good
        { position = pos
        , health   = 50
        , weapon   = Unarmed
        , canMove  = False
        , label    = "Radar Facility"
        }

initialCharacters : List Character
initialCharacters =
    [ initialBasicEnemy   { x = 5,  y = 5  }
    , initialBasicEnemy   { x = 20, y = 5  }
    , initialBasicEnemy   { x = 5,  y = 20 }
    , initialBase         { x = 12, y = 12 }
    , initialPlayer       { x = 10, y = 8  }
    , initialBasicChaotic { x = 20, y = 20 }
    ]

initialPlayer : Position -> Character
initialPlayer pos = 
    PlayerControlled
        { position = pos
        , health   = 20
        , weapon   = Damage 2
        , canMove  = True
        , label    = "Defence Drone"
        }

initialExecutingGame : ExecutingGame
initialExecutingGame =
    { programMemory = empty
    , characters    = initialCharacters
    , score         = Just 0
    }

initialModel : Model
initialModel =
    { source         = ""
    , sourceError    = Nothing
    , gameWorld      = initialWorld
    , executingGame  = Nothing
    }

---- UPDATE ----

type Action

    -- User modifies their source code
    = ModifySource String  

    -- User starts the battle and their code executes within the game world
    | StartBattle          

    -- The wall clock ticks, driving our battle forward if the game has started
    | StepBattle Float       

    -- Ronseal
    | NoOp

type Direction
    = North
    | South
    | East
    | West

type Intent -- Something that a character intends to do
    = Move Direction
    | Fire Direction
    | Wait

isInWorld : World -> Position -> Bool
isInWorld world pos =
       pos.x < world.width
    && pos.y < world.height
    && pos.x >= 0
    && pos.y >= 0

modifySource : ProgramSource -> Model -> Model
modifySource newSource model =
    -- TODO Syntax checking?
    { model | source <- newSource }

startBattle : Model -> Model
startBattle model  =
    { model | executingGame <- Just initialExecutingGame }

getIntentWithAI : Character -> Model -> Intent
getIntentWithAI char model =
    -- TODO Add some basic AI here
    Wait

getIntentWithProgram : Character -> Model -> Intent
getIntentWithProgram char model =
    -- TODO Interpret player's code to see what to do next
    Wait

rotate : Array Character -> Array Character
rotate characters = -- Moves the first element to the back of the array
    case length of
        0 -> characters
        1 -> characters
        _ -> 
            let first = get 0 characters in
            let tail = slice 1 (length characters |> (-) 1) in
            push first tail

move : Direction -> Position -> Position
move direction position =
    case direction of
        North -> { position | y <- position.y - 1 }
        South -> { position | y <- position.y + 1 }
        West  -> { position | x <- position.x - 1 }
        East  -> { position | x <- position.x + 1 }

resolveIntent : List Character -> World -> List Character
resolveIntent characters world =
    let nextCharacter = get 0 characters in
    let intent = getIntentWithAI nextCharacter model in
    case intent of
        Move direction ->
            let intendedPosition = (move direction nextCharacter.position) in
            let updatedCharacter = 
                { nextCharacter | position <- -- TODO Check for collisions with other entities
                    if isInWorld intendedPosition then intendedPosition else nextCharacter.position }
            in
            rotate (set 0 updatedCharacter characters)
        Fire direction ->
            -- TODO Damage nearest enemy in direction of fire
            -- TODO Rotate self to back of queue
            rotate characters
        Wait -> rotate characters

timeStep : Model -> Model
timeStep model =
    case model.executingGame of
        Nothing -> model -- If the game isn't executing, we don't need to step
        Just executingGame ->
            let newCharacters = resolveIntent model.characters model.gameWorld in
            let newExecutingGame = { executingGame | characters <- newCharacters } in
            { model | executingGame <- newExecutingGame }

update : Action -> Model -> Model
update action model =
    case action of
        ModifySource newSource -> 
            modifySource newSource model 
        StartBattle -> 
            startBattle model
        StepBattle delta -> 
            model
        NoOp -> 
            model


---- VIEW ----

viewTextEditor : Model -> Html
viewTextEditor model =
    div
        [ class "panel panel-default fullHeight"
        , style [] 
        ]
        [ div
            [ class "panel-heading"
            , style []
          ]
          [ h3 
              [ class "panel-title"
              , style []
              ]
              [ text "Source code" ]
          ]
        , div
            [ class "panel-body"
            , style 
                [ ("height", "90%") ]
            ]
            [ textarea
                [ class "source"
                , style [] 
                , placeholder "Enter your source code here"
                , spellcheck False
                , on "change" targetValue (Signal.send updates << ModifySource)
                ]
                [ text model.source ]
            , button
                [ class "btn btn-danger pull-right"
                , style []
                , onClick (Signal.send updates StartBattle)
                ]
                [ text "Upload to Drone" ]
            ]
        ]

viewGameWorld : Model -> Svg.Svg
viewGameWorld model =
    let size = 600 in
    let sizeTxt = toString size in
    let positionToView pos =
        ( (toFloat pos.x / toFloat model.gameWorld.width)*size
        , (toFloat pos.y / toFloat model.gameWorld.height)*size
        )
    in
    let drawEntity pos color label =
        let charSize = toFloat size / 100.0 in
        let charSizeTxt = toString charSize in
        let viewPos = positionToView pos in
        let offsetLabel xy = ((fst xy) + 2 * charSize, (snd xy) - charSize) in
        [ Svg.rect
            [ Svg.Attributes.fill color
            , Svg.Attributes.width charSizeTxt
            , Svg.Attributes.height charSizeTxt 
            , Svg.Attributes.x (viewPos |> fst |> toString)
            , Svg.Attributes.y (viewPos |> snd |> toString)
            ]
            []
        , Svg.text
            [ Svg.Attributes.fill color
            , Svg.Attributes.x (viewPos |> offsetLabel |> fst |> toString)
            , Svg.Attributes.y (viewPos |> offsetLabel |> snd |> toString)
            ]
            [ text label ]
        ]
    in
    let viewCharacter char =
        case char of
            PlayerControlled e -> drawEntity e.position "#0bd193" e.label
            Good e -> drawEntity e.position "#149169" e.label
            Chaotic e -> drawEntity e.position "#ffda34" e.label
            Evil e -> drawEntity e.position "#ff2200" e.label
    in
    let characters =
        case model.executingGame of
            Nothing -> []
            Just exeGame -> exeGame.characters
    in
    let background =
        Svg.rect
            [ Svg.Attributes.fill "#000000"
            , Svg.Attributes.width sizeTxt
            , Svg.Attributes.height sizeTxt
            , Svg.Attributes.x "0"
            , Svg.Attributes.y "0"
            ]
            []
    in
    Svg.svg
        [ class "world"
        , Svg.Attributes.width sizeTxt
        , Svg.Attributes.height sizeTxt
        ]
        (background :: (characters |> List.concatMap viewCharacter))

view : Model -> Html
view model =
    div
        [ class "container-fluid"
        , style 
            [ ("height", "100vh")
            , ("margin", "10px")
            ]
        ]
        [ div 
            [ class "row fullHeight"
            , style [] 
            ] 
            [ div 
                [ class "col-md-6 fullHeight" ]
                [ viewTextEditor model ]
            , div
                [ class "col-md-6 fullHeight" ]
                [ viewGameWorld model ]
            ] 
        ]

---- INPUTS ----

-- wire the entire application together
main : Signal Html
main = Signal.map view model

-- manage the model of our application over time
model : Signal Model
model = Signal.foldp update initialModel (Signal.merge 
    (Signal.map 
        (\t -> StepBattle t) 
        (Time.fps 5)) -- TODO Use fpsWhen?
    (Signal.subscribe updates))

-- updates from user input
updates : Signal.Channel Action
updates = Signal.channel NoOp
