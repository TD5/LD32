module Game where
{-| This is my game! 

The player writes a program which then controls their unit on the battlefield.

Their unit fights on their behalf until there are no friendly unit left - the
goal is to keep as many friendly units alive for as long as possible.

- The unconventional weapon is the player's logical reasoning!
-}

import Array (..)
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

type Team 
    = Friendly 
    | Neutral 
    | Enemy

type Weapon 
    = Unarmed 
    | Damage Int

type alias World =  -- The origin is (0, 0) and it extends out to (width - 1, height - 1) in a rectangle
    { width  : Int
    , height : Int 
    }

type alias Position = -- Coods in the world
    { x : Int
    , y : Int 
    }

type alias NPC = -- A character in the world that the player can't control 
    { team     : Team
    , position : Position
    , health   : Health
    , weapon   : Weapon
    , canMove  : Bool 
    }

type alias Player =  -- The unit the player's program controls
    { position : Position
    , health   : Health 
    }

type alias ExecutingGame =
    { programMemory : ProgramMemory
    , npcs          : List NPC
    , player        : Player
    , score         : Maybe Int
    }

type alias Model = -- The full state of the game at any point in time
    { source         : ProgramSource
    , sourceError    : Maybe String -- A description of what's wrong with the source code
    , gameWorld      : World
    , executingGame  : Maybe ExecutingGame
    }

initialWorld : World
initialWorld = 
    { width  = 25
    , height = 25
    }

initialBasicEnemy : Position -> NPC
initialBasicEnemy pos =
    { team     = Enemy
    , position = pos
    , health   = 3
    , weapon   = Damage 1 
    , canMove  = True
    }

{-  
A tough but unarmed, immobile friendly base that the player wants to protect
-}
initialBase : Position -> NPC
initialBase pos =
    { team     = Friendly
    , position = pos
    , health   = 50
    , weapon   = Unarmed
    , canMove  = False
    }

initialNpcs : List NPC
initialNpcs =  
    [ initialBasicEnemy { x = 5,  y = 5  }
    , initialBasicEnemy { x = 20, y = 5  }
    , initialBasicEnemy { x = 5,  y = 20 }
    , initialBasicEnemy { x = 20, y = 20 }
    , initialBase       { x = 12, y = 12 }
    ]

initialPlayer : Position -> Player
initialPlayer pos = 
    { position = pos
    , health   = 20
    }

initialExecutingGame : ExecutingGame
initialExecutingGame =
    { programMemory = empty
    , npcs          = initialNpcs
    , player        = initialPlayer { x = 10, y = 8 }
    , score         = Just 0
    }

initialModel : Model
initialModel =
    { source        = "Some example program here"
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

type GameEntityAction
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

timeStep : Model -> Model
timeStep model =
    case model.executingGame of
        Nothing -> model -- If the game isn't executing, we don't need to step
        Just executingGame ->
            model
            -- TODO Interpret player's code to see what to do next
            -- TODO Make each other unit perform its action

update : Action -> Model -> Model
update action model =
    case action of
        ModifySource newSource -> 
            modifySource newSource model 
        StartBattle -> 
            startBattle model
        TimeStep delta -> 
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
                ]
                [ text "TEST" ]
            ]
        ]

viewGameWorld : Model -> Html
viewGameWorld model =
    text "Game world"

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
