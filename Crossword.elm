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
  , letter : Maybe String
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
  [ [ Square (Just 1) Nothing True  ( 0, 0 )
    , Square Nothing  Nothing True  ( 0, 1 )
    , Square (Just 2) Nothing True  ( 0, 2 )
    , Square Nothing  Nothing True  ( 0, 3 )
    , Square (Just 3) Nothing True  ( 0, 4 )
    ]
  , [ Square Nothing  Nothing True  ( 1, 0 )
    , Square Nothing  Nothing False ( 1, 1 )
    , Square Nothing  Nothing True  ( 1, 2 )
    , Square Nothing  Nothing False ( 1, 3 )
    , Square Nothing  Nothing True  ( 1, 4 )
    ]
  , [ Square (Just 4) Nothing True  ( 2, 0 )
    , Square Nothing  Nothing True  ( 2, 1 )
    , Square Nothing  Nothing True  ( 2, 2 )
    , Square Nothing  Nothing True  ( 2, 3 )
    , Square Nothing  Nothing True  ( 2, 4 )
    ]
  , [ Square Nothing  Nothing True  ( 3, 0 )
    , Square Nothing  Nothing False ( 3, 1 )
    , Square Nothing  Nothing True  ( 3, 2 )
    , Square Nothing  Nothing False ( 3, 3 )
    , Square Nothing  Nothing True  ( 3, 4 )
    ]
  , [ Square (Just 5) Nothing True  ( 4, 0 )
    , Square Nothing  Nothing True  ( 4, 1 )
    , Square Nothing  Nothing True  ( 4, 2 )
    , Square Nothing  Nothing True  ( 4, 3 )
    , Square Nothing  Nothing True  ( 4, 4 )
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
            { square | letter = Just (String.left 1 letter) }
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
    ((node "link" [ rel "stylesheet", href "styles.css" ] [])
    :: (List.map viewRow model.rows))


viewRow : Row -> Html Msg
viewRow row =
    tr []
       (List.map viewSquare row)

viewSquare : Square -> Html Msg
viewSquare square =
  td
    [ class (classForSquare square)
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
             , placeholder (Maybe.withDefault " " square.letter)
             , maxlength 1
             , onInput (EnterLetter square.coord)
             ]
             []
  else div [ class "black" ]
           [ ]

classForSquare : Square -> String
classForSquare square =
  if square.fillable
  then "white"
  else "black"
