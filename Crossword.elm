module Crossword exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra exposing (groupBy)
import Task
import Json.Decode
import Json.Encode
import String exposing (fromChar)
import API exposing (..)


-- MODEL

initialModel : (Crossword, Cmd Msg)
initialModel =
  ({ squares = [] }, Cmd.none)



-- UPDATE


type Msg
  = NoOp
  | EnterLetter X Y String
  | FetchCrossword
  | ReceiveCrossword Crossword
  | SubmitCrossword Crossword
  | FetchFail Http.Error


update : Msg -> Crossword -> (Crossword, Cmd Msg)
update msg model =
  case msg of
    EnterLetter x y letter ->
      let
        updateSquare square =
          if square.x == x && square.y == y then
            { square | guessedLetter = Just (String.left 1 letter) }
          else
            square
      in
        ({ model | squares = List.map updateSquare model.squares }, Cmd.none)

    FetchCrossword ->
      (model, getCrossword "b520ac22-acab-43ad-8431-acdae3aec390")

    ReceiveCrossword crossword ->
      (crossword, Cmd.none)

    SubmitCrossword crossword ->
      (model, putCrossword "b520ac22-acab-43ad-8431-acdae3aec390" crossword)

    FetchFail _ ->
      (model, Cmd.none)

    NoOp ->
      (model, Cmd.none)


getCrossword : String -> Cmd Msg
getCrossword uuid =
  let
    request =
      { verb =
          "GET"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/"
            ++ "crosswords"
            ++ "/"
            ++ (uuid |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Task.perform FetchFail ReceiveCrossword
          (Http.fromJson decodeCrossword
             (Http.send Http.defaultSettings request))

putCrossword : String -> Crossword -> Cmd Msg
putCrossword uuid crossword =
  let
    request =
      { verb =
          "PUT"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/"
            ++ "crosswords"
            ++ "/"
            ++ (uuid |> Http.uriEncode)
      , body =
          Http.string (Json.Encode.encode 0 (encodeCrossword crossword))
      }
  in
    Task.perform FetchFail ReceiveCrossword
          (Http.fromJson decodeCrossword
             (Http.send Http.defaultSettings request))



-- VIEW


view : Crossword -> Html Msg
view crossword =
  div []
        ((node "link" [ rel "stylesheet", href "styles.css" ] [])
        :: [ button [ onClick FetchCrossword ] [ text "Play!" ]
           , table
               [ class (classForTable crossword)
               ]
               (rows crossword.squares)
           , button [ onClick (SubmitCrossword crossword) ] [ text "Submit!" ]
           , viewClues
           ]
        )

classForTable : Crossword -> String
classForTable crossword =
  if Debug.log "All correct: " (List.all correct (List.filter .fillable crossword.squares))
  then "crossword solved"
  else "crossword"

correct : Square -> Bool
correct square =
  case square.guessedLetter of
    Nothing -> False
    (Just l) -> String.toLower l == String.toLower square.letter


rows : List Square -> List (Html Msg)
rows squares =
  let
    sorted = List.sortBy .y squares
    grouped = groupBy (\a b -> a.y == b.y) sorted
  in
    List.map viewRow (Debug.log "grouped: " (List.map (\r -> List.sortBy .x r) grouped))


viewRow : List Square -> Html Msg
viewRow squares =
    tr [ ]
       (List.map viewSquare squares)

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
             , placeholder (Maybe.withDefault " " square.guessedLetter)
             , maxlength 1
             , onInput (EnterLetter square.x square.y)
             ]
             []
  else div [ class "black" ]
           [ ]

classForSquare : Square -> String
classForSquare square =
  if square.fillable
  then "white"
  else "black"

viewClues : Html Msg
viewClues =
  div []
      [ div []
            [ text "1 Down: Aesopian ending" ]
      , div []
            [ text "2 Down: Complex contrapuntal music" ]
      , div []
            [ text "3 Down: Cavern, in poetry" ]
      , div []
            [ text "--" ]
      , div []
            [ text "1 Across: Group with rackets" ]
      , div []
            [ text "4 Across: One way to turn" ]
      , div []
            [ text "5 Across: Loyal vassal" ]
      ]
