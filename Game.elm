module Game where
{-| This is my game! 

The player writes a program which then controls their unit on the battlefield.

Their unit fights on their behalf until there are no friendly unit left - the
goal is to keep as many friendly units alive for as long as possible.

- The unconventional weapon is the player's logical reasoning!
-}

import Array (..)
import Basics
import Debug
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import Json.Decode as Json
import List
import Maybe
import Random
import Regex
import Result
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
type alias SourceError = String

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

-- TODO Put the allegience inside the Entity
type Character -- An entity with an alignment to a team
    = Player Entity -- Player controlled entities
    | Good Entity -- Won't attack player or other good entities
    | Chaotic Entity -- Will attack anything, even other chaotics
    | Evil Entity -- Won't attack other evil entities

getPosition : Character -> Position
getPosition character =
    case character of
        Player e  -> e.position
        Good e    -> e.position
        Chaotic e -> e.position
        Evil e    -> e.position

setPosition : Character -> Position -> Character
setPosition character newPos =
    case character of
        Player e  -> Player  { e | position <- newPos }
        Good e    -> Good    { e | position <- newPos }
        Chaotic e -> Chaotic { e | position <- newPos }
        Evil e    -> Evil    { e | position <- newPos }

updateCharacter : (Entity -> Entity) -> Character -> Character
updateCharacter change character =
    case character of
        Player e  -> Player (change e)
        Good e    -> Good (change e)
        Chaotic e -> Chaotic (change e)
        Evil e    -> Evil (change e)

getHealth : Character -> Int
getHealth c =
    case c of
        Player e  -> e.health
        Good e    -> e.health
        Chaotic e -> e.health
        Evil e    -> e.health

canMove : Character -> Bool
canMove character =
    case character of
        Player e  -> e.canMove
        Good e    -> e.canMove
        Chaotic e -> e.canMove
        Evil e    -> e.canMove

isEvil : Character -> Bool
isEvil character =
    case character of
        Evil c -> True
        _      -> False

isGood : Character -> Bool
isGood character =
    case character of
        Good c -> True
        _      -> False


type alias ExecutingGame =
    { programMemory : ProgramMemory
    , characters    : Array Character -- In order of priority to perform an action
    , score         : Maybe Int
    }

type alias Model = -- The full state of the game at any point in time
    { source         : ProgramSource
    , sourceError    : Maybe SourceError
    , gameWorld      : World
    , executingGame  : Maybe ExecutingGame
    }

isGameOver : Model -> Bool
isGameOver model =
    let count test array =
        foldl (\x acc -> if test x then acc + 1 else acc ) 0 array
    in
    let isOnlyOneSideLeft chars =
        let isChaotic x = (isGood x |> not) && (isEvil x |> not) in
        let isAnyEvil = count isEvil chars > 0 in
        let isAnyGood = count isGood chars > 0 in
        let isAnyChaotic = count isChaotic chars > 0 in
        case (isAnyGood, isAnyEvil, isAnyChaotic) of
            (True, False, False) -> True
            (False, True, False) -> True
            (False, False, True) -> True
            _                    -> False
    in
    case model.executingGame of
        Nothing   -> False
        Just game -> isOnlyOneSideLeft game.characters

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
initialBase : Position -> String -> Int -> Character
initialBase pos name health=
    Good
        { position = pos
        , health   = health
        , weapon   = Unarmed
        , canMove  = False
        , label    = name
        }

initialCharacters : Array Character
initialCharacters =
    fromList
        [ initialBasicEnemy   { x = 5,  y = 5  }
        , initialBasicChaotic { x = 1,  y = 24 }
        , initialBasicEnemy   { x = 20, y = 5  }
        , initialBasicEnemy   { x = 5,  y = 20 }
        , initialBase         { x = 12, y = 12 } "Radar Facility" 50
        , initialBase         { x = 14, y = 23 } "Power Station" 45
        , initialPlayer       { x = 10, y = 8  }
        , initialBasicChaotic { x = 20, y = 20 }
        , initialBase         { x = 18, y = 7  } "Hospital" 30
        ]

initialPlayer : Position -> Character
initialPlayer pos = 
    Player
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
    { model | source <- newSource }
    {-
    -- No real world needed, just enough to make the program attempt to do something
    let lines = String.split "\n" newSource in
    let examplePlayer = initialPlayer { x = 0, y = 0 } in
    let attemptToGetIntent someSource = 
        getIntentWithProgram examplePlayer empty initialWorld someSource
    in
    let checkAttemptAcc line acc =
        case acc of
            Nothing ->
                let intentOrError = attemptToGetIntent line in
                case intentOrError of
                    AnIntentTo _        -> Nothing
                    AnErrorOf errorMsg  -> Just errorMsg
            Just errorMsg -> errorMsg
    in
    let maybeError = List.foldl checkAttemptAcc Nothing lines in
    { model | sourceError <- maybeError
            , source <- newSource 
    }-}

startBattle : Model -> Model
startBattle model  =
    { model | executingGame <- Just initialExecutingGame
            , sourceError <- Nothing
    }

dist : Position -> Position -> Float
dist posA posB =
    (posA.x - posB.x) ^ 2
    |> (+) ((posA.y - posB.y) ^ 2)
    |> toFloat
    |> sqrt

nearestWhere : (Character -> Bool) -> Array Character -> Position -> Maybe Character
nearestWhere check characters pos =
    let getClosest char maybeOtherChar =
        case maybeOtherChar of
            Nothing -> Just char
            Just otherChar ->
                if (getPosition char |> dist pos) < (getPosition otherChar |> dist pos)
                   then Just char
                   else Just otherChar
    in
    characters 
    |> filter check 
    |> foldl getClosest Nothing 

directionFrom : Position -> Position -> Direction
directionFrom from to =
    let diffX = to.x - from.x in
    let diffY = to.y - from.y in
    if | abs diffX > abs diffY ->
        if diffX > 0 then East else West
       | otherwise ->
        if diffY > 0 then South else North

isDirectly : Direction -> Position -> Position -> Bool
isDirectly direction from to =
    case direction of
        North -> from.x == to.x && from.y > to.y
        South -> from.x == to.x && from.y < to.y
        East  -> from.y == to.y && from.x < to.x
        West  -> from.y == to.y && from.x > to.x

getIntentWithAI : Character -> Array Character -> World -> Intent
getIntentWithAI char otherChars world =
    let here = getPosition char in
    let attackWhere check =
        let possibleTarget = 
            here |> nearestWhere check otherChars 
        in
        case possibleTarget of
            Nothing -> Wait
            Just target -> 
                let targetPos = getPosition target in
                let targetDirection = directionFrom here targetPos in
                if dist here targetPos == 1
                    then Fire targetDirection
                    else Move targetDirection
    in
    case char of
        Player e  -> Wait
        Good e    -> attackWhere (not << isGood)  
        Chaotic e -> attackWhere (\c -> True)
        Evil e    -> attackWhere (not << isEvil)

type IntentOrSourceError
    = AnIntentTo Intent
    | AnErrorOf SourceError

isWithinRegex : Regex.Regex
isWithinRegex = Regex.regex "^(enemy|friendly|edge) isWithin (\\d+) (north|south|east|west)?$"

type Object
    = Enemy
    | Friendly
    | WorldEdge

parseObj : String -> Maybe Object
parseObj objStr =
    case objStr of
        "enemy"    -> Just Enemy
        "friendly" -> Just Friendly
        "edge"     -> Just WorldEdge
        _          -> Nothing

distanceToEdgeOfWorld : Position -> World -> Int
distanceToEdgeOfWorld pos world =
    Basics.min 
        (Basics.min pos.x pos.y) 
        (Basics.min (world.width - pos.x) (world.height - pos.y))

isSomething : Maybe a -> Bool
isSomething maybe =
    case maybe of
        Just _  -> True
        Nothing -> False

parseIsWithinCheck : String -> Maybe (Character -> Array Character -> World -> Bool)
parseIsWithinCheck checkStr =
    let isWithinFunc object distance optDirection char otherChars world =
        let here = getPosition char in
        let distCheck from maybeChar distance =
            case maybeChar of
                Just to -> 
                    let isCloseEnough = 
                        getPosition to |> dist from |> (>=) (toFloat distance)
                    in
                       case optDirection of
                           Nothing  -> isCloseEnough
                           Just dir -> getPosition to |> isDirectly dir here
                Nothing -> False
        in
        case object of
            WorldEdge -> distanceToEdgeOfWorld here world <= distance
            Enemy     -> distCheck here (nearestWhere (not << isGood) otherChars here) distance
            Friendly  -> distCheck here (nearestWhere isGood otherChars here) distance
    in
    let buildIsWithinFunc optObjStr optDistStr optDirStr =
        case (optObjStr, optDistStr, optDirStr) of
            (Just objStr, Just distStr, Just dirStr) ->
                let maybeObj = parseObj objStr in
                let maybeDist = String.toInt distStr |> Result.toMaybe in
                let maybeDir = parseDir dirStr in
                case (maybeObj, maybeDist) of
                    (Just obj, Just dist) ->
                        Just (isWithinFunc obj dist maybeDir)
                    _ -> Nothing
            _ -> Nothing
    in
    let handleSubmatches submatches =
        case submatches of
            obj :: dist :: dir :: [] -> buildIsWithinFunc obj dist dir
            _ -> Nothing
    in
    case Regex.find (Regex.AtMost 1) isWithinRegex checkStr of
        match :: [] -> handleSubmatches match.submatches
        _           -> Nothing

parseCheck : String -> Maybe (Character -> Array Character -> World -> Bool)
parseCheck checkStr = -- Tries to parse a checking function from a string
    let alwaysFunc char otherChars world = True in
    let neverFunc char otherChars world = False in
    if | checkStr == "always" -> Just alwaysFunc -- Always matches
       | checkStr == "never"  -> Just neverFunc -- Never matches
       | Regex.contains isWithinRegex checkStr -> parseIsWithinCheck checkStr
       | otherwise -> Nothing -- Failure, syntax error in check string

parseDir : String -> Maybe Direction
parseDir dir =
    case dir of
        "north" -> Just North
        "south" -> Just South
        "east"  -> Just East
        "west"  -> Just West
        _       -> Nothing

parseIntent : String -> Maybe Intent
parseIntent intentStr = -- Tries to parse an intent
    case String.split " " intentStr of
        "wait" :: []           -> Just Wait
        "move" :: dirStr :: [] -> Maybe.andThen (parseDir dirStr) (\dir -> Just (Move dir))
        "fire" :: dirStr :: [] -> Maybe.andThen (parseDir dirStr) (\dir -> Just (Fire dir))
        _                      -> Nothing

getIntentWithProgram : Character -> Array Character -> World -> ProgramSource -> IntentOrSourceError
getIntentWithProgram char otherChars world source =
    let lines = String.split "\n" source in
    let handleCheckAndIntent checkStr intentStr = 
        let maybeCheck = parseCheck checkStr in -- Possibly some func for testing game state
        let maybeIntent = parseIntent intentStr in -- Possibly an Intent: Move North, Fire East, Wait etc
        let checkForIntent check intent = -- Applies a check to maybe yield an intent depending of whether the test passes
            if check char otherChars world
                then Just (AnIntentTo intent)
                else Nothing
        in
        case maybeCheck of
            Nothing -> Just (AnErrorOf ("Broken check: '" ++ checkStr ++ "'"))
            Just check ->
                case maybeIntent of
                    Nothing -> Just (AnErrorOf ("Broken intent: '" ++ intentStr ++ "'"))
                    Just intent -> checkForIntent check intent
    in
    let parseLine line = -- Takes a line and gives a Maybe IntentOrSourceError
        -- Assumes line of format "<intent> when <check>"
        case String.split " when " line of
            intentStr :: checkStr :: [] -> 
                handleCheckAndIntent checkStr intentStr
            _ -> Just (AnErrorOf ("A line is missing 'when': '" ++ line ++ "'"))
    in
    let handleLine line currentIntent =
        case currentIntent of
            Nothing -> parseLine line
            x       -> x -- We already have some output
    in
    let possiblyIntentOrError = 
        if String.isEmpty source -- No source at all just makes player wait
           then Nothing
           else List.foldl handleLine Nothing lines in
    case possiblyIntentOrError of
        Nothing -> AnIntentTo Wait
        Just x  -> x

tail : Array Character -> Array Character
tail chars = 
    case length chars of
        0 -> empty
        n -> slice 1 n chars

rotate : Array Character -> Array Character
rotate characters = -- Moves the first element to the back of the queue
    case get 0 characters of
        Nothing -> characters
        Just first ->
            tail characters |> push first 

move : Direction -> Position -> Position
move direction position =
    case direction of
        North -> { position | y <- position.y - 1 }
        South -> { position | y <- position.y + 1 }
        West  -> { position | x <- position.x - 1 }
        East  -> { position | x <- position.x + 1 }

type UpdatedCharactersOrSourceError
    = Some (Array Character)
    | ErrorOf SourceError

isAnyAt : Array Character -> Position -> Bool
isAnyAt characters position = -- Returns true if any character is already at the given position
    let isAtPosition character acc =
        getPosition character |> (==) position |> (||) acc
    in
    foldl isAtPosition False characters

hurt : Character -> Int -> Maybe Character
hurt char damage =
    let hurtEntity e =
        { e | health <- e.health - damage }
    in
    let damagedCharacter = updateCharacter hurtEntity char in
        if getHealth damagedCharacter > 0
        then Just damagedCharacter
        else Nothing

getDamage : Character -> Int
getDamage c =
    let weapon =
        case c of
            Player e  -> e.weapon
            Good e    -> e.weapon
            Chaotic e -> e.weapon
            Evil e    -> e.weapon
    in
       case weapon of
           Unarmed  -> 0
           Damage x -> x

resolveIntent : Array Character -> World -> ProgramSource -> UpdatedCharactersOrSourceError
resolveIntent characters world source =
    case get 0 characters of
        Nothing -> Some characters -- Nothing to do
        Just thisCharacter ->
            let here = getPosition thisCharacter in
            let otherCharacters = tail characters in
            let intentOrError = 
                case thisCharacter of
                    Player _ -> getIntentWithProgram thisCharacter otherCharacters world source |> Debug.log "Intent from program" 
                    _        -> AnIntentTo (getIntentWithAI thisCharacter otherCharacters world)
            in
            case intentOrError of
                AnErrorOf e -> ErrorOf e
                AnIntentTo intent ->
                    case intent of
                        Wait -> Some (rotate characters) -- The character has forfeited their turn
                        Move direction ->
                            let intendedPosition = here |> move direction in
                            let isValid pos =
                                isInWorld world pos &&
                                (isAnyAt characters pos |> not) &&
                                canMove thisCharacter
                            in
                            let resultantPosition = 
                                if isValid intendedPosition
                                   then intendedPosition 
                                   else here 
                            in
                            let updatedCharacter = setPosition thisCharacter resultantPosition
                            in
                            Some (set 0 updatedCharacter characters |> rotate)
                        Fire firingDirection ->
                            let isInFiringLine otherChar =
                                getPosition otherChar |> isDirectly firingDirection here
                            in
                            let possibleCharacterHit =
                                nearestWhere isInFiringLine otherCharacters here
                            in
                            let charactersLeft =
                                case possibleCharacterHit of
                                    Nothing  -> otherCharacters
                                    Just hit ->
                                        let applyDamage c acc =
                                            let afterDamage =
                                                if (c == hit)
                                                then getDamage thisCharacter |> hurt c 
                                                else Just c
                                            in
                                            case afterDamage of
                                                Nothing -> acc
                                                Just d -> push d acc
                                        in
                                        foldl applyDamage empty otherCharacters
                            in
                            Some (push thisCharacter charactersLeft)

timeStep : Model -> Model
timeStep model =
    case model.executingGame of
        Nothing -> 
            model -- If the game isn't executing, we don't need to step
        Just executingGame ->
            case resolveIntent executingGame.characters model.gameWorld model.source of
                Some newCharacters -> 
                    let newExecutingGame =
                        { executingGame | characters <- newCharacters } 
                    in
                    { model | executingGame <- Just newExecutingGame }
                ErrorOf error -> 
                    { model | sourceError <- Just (error |> Debug.log "Error") }

update : Action -> Model -> Model
update action model =
    case action of
        ModifySource newSource -> 
            modifySource newSource model
        StartBattle -> 
            startBattle model
        StepBattle delta -> 
            timeStep model
        NoOp -> model


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
        let makeLabelWithHealth entity =
            entity.label ++ " - " ++ (toString entity.health)
        in
        case char of
            Player e  -> drawEntity e.position "#0bd193" (makeLabelWithHealth e)
            Good e    -> drawEntity e.position "#149169" (makeLabelWithHealth e)
            Chaotic e -> drawEntity e.position "#ffda34" (makeLabelWithHealth e)
            Evil e    -> drawEntity e.position "#ff2200" (makeLabelWithHealth e)
    in
    let characters m =
        case m.executingGame of
            Nothing -> []
            Just eg -> toList eg.characters
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
        (if isGameOver model || isSomething model.sourceError
             then 
                [ Svg.text
                    [ Svg.Attributes.fill "#000000"
                    , Svg.Attributes.x "10"
                    , Svg.Attributes.y "10"
                    ]
                    [ text 
                        (if isGameOver model
                            then "Game Over - Try uploading a new program"
                            else 
                                case model.sourceError of
                                    Just error -> "Source code error - " ++ error
                                    _          -> "")
                    ]
                ]
             else (background :: (model |> characters |> List.concatMap viewCharacter)))

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
