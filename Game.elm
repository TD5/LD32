module Game where
{-| This is my game! 

The player writes a program which then controls their unit on the battlefield.

Their unit fights on their behalf until there are no friendly unit left - the
goal is to keep as many friendly units alive for as long as possible.

- The unconventional weapon is the player's logical reasoning!
-}

import Array
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
type alias ProgramMemory = Array [ Int ] 

type alias Health = Int

type alias Team 
    = Friendly 
    | Neutral 
    | Enemy

type alias Weapon 
    = Unarmed 
    | Damage Int

type alias World =  -- The origin is (0, 0) and it extends out to (width - 1, height - 1) in a rectangle
    { width : Int
    , height : Int 
    }

type alias Position = -- Coods in the world
    { x : Int
    , y : Int 
    }

type alias NPC = -- A character in the world that the player can't control 
    { team : Team
    , position : Position
    , health : Health
    , weapon : Weapon
    , canMove : Bool 
    }

type alias Player =  -- The unit the player's program controls
    { position : Position
    , health : Health 
    }

type alias ExecutingGame =
    { programMemory : ProgramMemory
    , npcs          : List NPC
    , player        : Player
    , score         : Maybe Int
    }

type alias Model = -- The full state of the game at any point in time
    { program        : ProgramSource
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
    , initialBase { x = 12, y = 12 }
    ]

initialPlayer : Position -> Player
initialPlayer pos = 
    { position = pos
    , health   = 20
    }

emptyModel : Model
emptyModel =
    { program      = ""
    , programState = []
    , gameWorld    = initialWorld
    , npcs         = initialNpcs
    , player       = initialPlayer { x = 10, y = 8 } 
    }

---- UPDATE ----

type Action

    -- User modifies their source code
    = ModifySource String  

    -- User starts the battle and their code executes
    | StartBattle          

    -- The wall clock ticks, driving our battle forward if the game has started
    | TimeStep Float       

update : Action -> Model -> Model
update action model = -- TODO Implement - currently everything is a NoOp.
    case action of
      ModifySource source -> model
      StartBattle         -> model
      TimeStep delta      -> model

---- VIEW ----

view : Model -> Html
view model =
    div
      [ class "todomvc-wrapper"
      , style [ ("visibility", "hidden") ]
      ]
      [ section
          [ id "todoapp" ]
          [ lazy taskEntry model.field
          , lazy2 taskList model.visibility model.tasks
          , lazy2 controls model.visibility model.tasks
          ]
      , infoFooter
      ]

onEnter : Signal.Message -> Attribute
onEnter message =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

taskEntry : String -> Html
taskEntry task =
    header 
      [ id "header" ]
      [ h1 [] [ text "todos" ]
      , input
          [ id "new-todo"
          , placeholder "What needs to be done?"
          , autofocus True
          , value task
          , name "newTodo"
          , on "input" targetValue (Signal.send updates << UpdateField)
          , onEnter (Signal.send updates Add)
          ]
          []
      ]

taskList : String -> List Task -> Html
taskList visibility tasks =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              "All" -> True

        allCompleted = List.all .completed tasks

        cssVisibility = if List.isEmpty tasks then "hidden" else "visible"
    in
    section
      [ id "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onClick (Signal.send updates (CheckAll (not allCompleted)))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map todoItem (List.filter isVisible tasks))
      ]

todoItem : Task -> Html
todoItem todo =
    let className = (if todo.completed then "completed " else "") ++
                    (if todo.editing   then "editing"    else "")
    in

    li
      [ class className ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked todo.completed
              , onClick (Signal.send updates (Check todo.id (not todo.completed)))
              ]
              []
          , label
              [ onDoubleClick (Signal.send updates (EditingTask todo.id True)) ]
              [ text todo.description ]
          , button
              [ class "destroy"
              , onClick (Signal.send updates (Delete todo.id))
              ]
              []
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          , id ("todo-" ++ toString todo.id)
          , on "input" targetValue (Signal.send updates << UpdateTask todo.id)
          , onBlur (Signal.send updates (EditingTask todo.id False))
          , onEnter (Signal.send updates (EditingTask todo.id False))
          ]
          []
      ]

controls : String -> List Task -> Html
controls visibility tasks =
    let tasksCompleted = List.length (List.filter .completed tasks)
        tasksLeft = List.length tasks - tasksCompleted
        item_ = if tasksLeft == 1 then " item" else " items"
    in
    footer
      [ id "footer"
      , hidden (List.isEmpty tasks)
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul
          [ id "filters" ]
          [ visibilitySwap "#/" "All" visibility
          , text " "
          , visibilitySwap "#/active" "Active" visibility
          , text " "
          , visibilitySwap "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (tasksCompleted == 0)
          , onClick (Signal.send updates DeleteComplete)
          ]
          [ text ("Clear completed (" ++ toString tasksCompleted ++ ")") ]
      ]

visibilitySwap : String -> String -> String -> Html
visibilitySwap uri visibility actualVisibility =
    let className = if visibility == actualVisibility then "selected" else "" in
    li
      [ onClick (Signal.send updates (ChangeVisibility visibility)) ]
      [ a [ class className, href uri ] [ text visibility ] ]

infoFooter : Html
infoFooter =
    footer [ id "info" ]
      [ p [] [ text "Double-click to edit a todo" ]
      , p [] [ text "Written by "
             , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
             ]
      , p [] [ text "Part of "
             , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
             ]
      ]


---- INPUTS ----

-- wire the entire application together
main : Signal Html
main = Signal.map view model

-- manage the model of our application over time
model : Signal Model
model = Signal.foldp update initialModel (Signal.subscribe updates)

initialModel : Model
initialModel =
  Maybe.withDefault emptyModel getStorage

-- updates from user input
updates : Signal.Channel Action
updates = Signal.channel NoOp

port focus : Signal String
port focus =
    let needsFocus act =
            case act of
              EditingTask id bool -> bool
              _ -> False

        toSelector (EditingTask id _) = ("#todo-" ++ toString id)
    in
        Signal.subscribe updates
          |> Signal.keepIf needsFocus (EditingTask 0 True)
          |> Signal.map toSelector


-- interactions with localStorage to save the model
port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
