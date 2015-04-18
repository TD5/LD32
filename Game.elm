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
        , health   = 5
        , weapon   = Damage 1 
        , canMove  = True
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
        }

initialCharacters : List Character
initialCharacters =
    [ initialBasicEnemy { x = 5,  y = 5  }
    , initialBasicEnemy { x = 20, y = 5  }
    , initialBasicEnemy { x = 5,  y = 20 }
    , initialBase       { x = 12, y = 12 }
    , initialPlayer     { x = 10, y = 8  }
    , initialBasicEnemy { x = 20, y = 20 }
    ]

initialPlayer : Position -> Character
initialPlayer pos = 
    PlayerControlled
        { position = pos
        , health   = 20
        , weapon   = Damage 2
        , canMove  = True
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
    | TimeStep Float       

    -- Ronseal
    | NoOp

type Direction
    = North
    | South
    | East
    | West

type Intention -- Something that a character intends to do
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

updateWithAI : Character -> Model -> Intention
updateWithAI char model =
    -- TODO Add some basic AI here
    Wait

updateWithProgram : Character -> Model -> Intention
updateWithProgram char model =
    -- TODO Interpret player's code to see what to do next
    Wait

timeStep : Model -> Model
timeStep model =
    case model.executingGame of
        Nothing -> model -- If the game isn't executing, we don't need to step
        Just executingGame ->
            -- TODO Get the next unit's intent
            -- TODO Resolve the intent (they might not achieve what they way, e.g. if they try to move off the world)
            -- TODO Reorder characters
            model

update : Action -> Model -> Model
update action model =
    case action of
        ModifySource newSource -> 
            Debug.log "Modify source" (modifySource newSource model) 
        StartBattle -> 
            Debug.log "Start battle" (startBattle model)
        TimeStep delta -> 
            Debug.log "Time step" model
        NoOp -> 
            Debug.log "NoOp" model


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
                , on "change" targetValue (Signal.send updates << ModifySource)
                ]
                [ text model.source ]
            , button
                [ class "btn btn-danger pull-right"
                , style []
                , onClick (Signal.send updates StartBattle)
                ]
                [ text "Upload to drone" ]
            ]
        ]

viewGameWorld : Model -> Html
viewGameWorld model =
    let size = 600 in
    let sizeTxt = toString size in
    let positionToView pos =
        ( (toFloat pos.x / toFloat model.gameWorld.width)*size
        , (toFloat pos.y / toFloat model.gameWorld.height)*size
        )
    in
    let drawEntity pos color =
        let charSize = toFloat size / 100.0 in
        let charSizeTxt = toString charSize in
        let viewPos = positionToView pos in
        Svg.rect
            [ Svg.Attributes.fill color
            , Svg.Attributes.width charSizeTxt
            , Svg.Attributes.height charSizeTxt 
            , Svg.Attributes.x (viewPos |> fst |> toString)
            , Svg.Attributes.y (viewPos |> snd |> toString)
            ]
            []
    in
    let viewCharacter char =
        case char of
            PlayerControlled e -> drawEntity e.position "#106b57"
            Good e -> drawEntity e.position "#30543f"
            Chaotic e -> drawEntity e.position "#290101"
            Evil e -> drawEntity e.position "#381604"
    in
    let characters =
        case model.executingGame of
            Nothing -> []
            Just exeGame -> exeGame.characters
    in
    Svg.svg
        [ class "world"
        , Svg.Attributes.width sizeTxt
        , Svg.Attributes.height sizeTxt
        ]
        [ Svg.rect
            [ Svg.Attributes.fill "#010101"
            , Svg.Attributes.width sizeTxt
            , Svg.Attributes.height sizeTxt
            , Svg.Attributes.x "0"
            , Svg.Attributes.y "0"
            ]
            []
        , (characters |> List.map viewCharacter)
        ]

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
model = Signal.foldp update initialModel (Signal.subscribe updates)

-- updates from user input
updates : Signal.Channel Action
updates = Signal.channel NoOp
