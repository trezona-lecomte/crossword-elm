module Crossword exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String exposing (fromChar)


-- MODEL


type alias Model =
  { rows : List Row
  }

type alias Row = List Square

type alias Square =
  { number : Maybe Int
  , letter : Maybe Char
  , fillable : Bool
  , coord : CoOrd
  }


type alias CoOrd =
  ( Int, Int )


initialModel : (Model, Cmd Msg)
initialModel =
  ({ rows = sampleSquares }, Cmd.none)


sampleSquares : List Row
sampleSquares =
  [ [ Square (Just 1) (Just '1') True  ( 0, 0 )
    , Square Nothing  (Nothing)  True  ( 0, 1 )
    , Square (Just 2) (Just '2') True  ( 0, 2 )
    , Square Nothing  (Nothing)  True  ( 0, 3 )
    , Square (Just 3) (Just '3') True  ( 0, 4 )
    ]
  , [ Square Nothing  (Nothing)  True  ( 1, 0 )
    , Square Nothing  (Nothing)  False ( 1, 1 )
    , Square Nothing  (Nothing)  True  ( 1, 2 )
    , Square Nothing  (Nothing)  False ( 1, 3 )
    , Square Nothing  (Nothing)  True  ( 1, 4 )
    ]
  , [ Square (Just 4) (Just '4') True  ( 2, 0 )
    , Square Nothing  (Nothing)  True  ( 2, 1 )
    , Square Nothing  (Nothing)  True  ( 2, 2 )
    , Square Nothing  (Nothing)  True  ( 2, 3 )
    , Square Nothing  (Nothing)  True  ( 2, 4 )
    ]
  , [ Square Nothing  (Nothing)  True  ( 3, 0 )
    , Square Nothing  (Nothing)  False ( 3, 1 )
    , Square Nothing  (Nothing)  True  ( 3, 2 )
    , Square Nothing  (Nothing)  False ( 3, 3 )
    , Square Nothing  (Nothing)  True  ( 3, 4 )
    ]
  , [ Square (Just 5) (Just '5') True  ( 4, 0 )
    , Square Nothing  (Nothing)  True  ( 4, 1 )
    , Square Nothing  (Nothing)  True  ( 4, 2 )
    , Square Nothing  (Nothing)  True  ( 4, 3 )
    , Square Nothing  (Nothing)  True  ( 4, 4 )
    ]
  ]



-- UPDATE


type Msg
  = NoOp
  | EnterLetter CoOrd String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EnterLetter coord letter ->
      let
        updateSquare square =
          if square.coord == coord then
            (Debug.log "Model changed: " { square | letter = Just (Maybe.withDefault ' ' (List.head (String.toList letter))) })
          else
            square
      in
        ({ model | rows = List.map (List.map updateSquare) model.rows }, Cmd.none)

    NoOp ->
      (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  table
    [ class "crossword"
    ]
    (List.map viewRow model.rows)


viewRow : Row -> Html Msg
viewRow row =
    tr []
       (List.map viewSquare row)

viewSquare : Square -> Html Msg
viewSquare square =
  td
    [ class (classForSquare square)
    -- , contenteditable square.fillable
    ]
    [ numberForSquare square
    , letterInput square
    ]

numberForSquare : Square -> Html msg
numberForSquare square =
  let char =
        case square.number of
          (Just n) -> toString n
          Nothing  -> " "
  in
    div [ class "number"
        ]
        [ text char
        ]

letterInput : Square -> Html Msg
letterInput square =
  if square.fillable
  then input [ class "letter"
             , type' "text"
             , value (fromChar (Maybe.withDefault ' ' square.letter))
             , on "input" (Json.map (EnterLetter square.coord) targetValue)
             ]
             []
  else div [ class "black" ]
           [ ]

classForSquare : Square -> String
classForSquare square =
  if square.fillable
  then "white"
  else "black"
