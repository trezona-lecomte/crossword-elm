module API exposing (..)

import Json.Decode exposing ((:=), maybe)
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import Task

type alias Crossword =
  { squares : List Square
  }

type alias Square =
  { number : Maybe Int
  , guessedLetter : Maybe String
  , letter : String
  , fillable : Bool
  , x : X
  , y : Y
  }

type alias X = Int
type alias Y = Int

encodeCrossword : Crossword -> Json.Encode.Value
encodeCrossword crossword =
  Json.Encode.object
    [ ("squares", Json.Encode.list (List.map encodeSquare crossword.squares))
    ]

decodeCrossword : Json.Decode.Decoder Crossword
decodeCrossword =
  Json.Decode.succeed Crossword
    |: ("squares" := Json.Decode.list decodeSquare)


decodeSquare : Json.Decode.Decoder Square
decodeSquare =
  Json.Decode.succeed Square
    |: (maybe ("number" := Json.Decode.int))
    |: (maybe ("guessedLetter" := Json.Decode.string))
    |: ("letter" := Json.Decode.string)
    |: ("fillable" := Json.Decode.bool)
    |: ("x" := Json.Decode.int)
    |: ("y" := Json.Decode.int)

encodeSquare : Square -> Json.Encode.Value
encodeSquare square =
  Json.Encode.object
     [ ( "number", (numberFor square) )
     , ( "guessedLetter", (guessedLetterFor square) )
     , ( "fillable", Json.Encode.bool square.fillable )
     , ( "letter", Json.Encode.string square.letter)
     , ( "x", Json.Encode.int square.x )
     , ( "y", Json.Encode.int square.y )
     ]

numberFor : Square -> Json.Encode.Value
numberFor square =
  case square.number of
    Nothing -> Json.Encode.null
    (Just n) -> Json.Encode.int n

guessedLetterFor : Square -> Json.Encode.Value
guessedLetterFor square =
  case square.guessedLetter of
    Nothing -> Json.Encode.null
    (Just l) -> Json.Encode.string l

-- getCrosswords : Task.Task Http.Error (List (Crossword))
-- getCrosswords =
--   let
--     request =
--       { verb =
--           "GET"
--       , headers =
--           [ ( "Content-Type", "application/json" ) ]
--       , url =
--           "http://localhost:8081/" ++ "crosswords"
--       , body =
--           Http.empty
--       }
--   in
--     Http.fromJson
--       (Json.Decode.list encodeCrossword)
--       (Http.send Http.defaultSettings request)

-- postCrosswordzes : Crossword -> Task.Task Http.Error (Crossword)
-- postCrosswordzes body =
--   let
--     request =
--       { verb =
--           "POST"
--       , headers =
--           [ ( "Content-Type", "application/json" ) ]
--       , url =
--           "http://localhost:8081/" ++ "crosswords"
--       , body =
--           Http.string (Json.Encode.encode 0 (encodeCrossword body))
--       }
--   in
--     Http.fromJson
--       decodeCrossword
--       (Http.send Http.defaultSettings request)


emptyResponseHandler : a -> String -> Task.Task Http.Error a
emptyResponseHandler x str =
  if str == "[]" then
    Task.succeed x
  else
    Task.fail (Http.UnexpectedPayload str)


handleResponse : (String -> Task.Task Http.Error a) -> Http.Response -> Task.Task Http.Error a
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
      Http.Text str ->
        handle str

      _ ->
        Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    Task.fail (Http.BadResponse response.status response.statusText)


promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
    Http.RawTimeout ->
      Http.Timeout

    Http.RawNetworkError ->
      Http.NetworkError


-- deleteCrosswordzesById : Int -> Task.Task Http.Error ()
-- deleteCrosswordzesById id =
--   let
--     request =
--       { verb =
--           "DELETE"
--       , headers =
--           [ ( "Content-Type", "application/json" ) ]
--       , url =
--           "http://localhost:8081/"
--             ++ "crosswords"
--             ++ "/"
--             ++ (id |> toString |> Http.uriEncode)
--       , body =
--           Http.empty
--       }
--   in
--     Task.mapError
--       promoteError
--       (Http.send Http.defaultSettings request)
--       `Task.andThen` handleResponse (emptyResponseHandler ())




-- type alias Crosswordlet =
--   { quizletId : Int
--   , quizletCrosswordId : Int
--   , quizletQuestion : String
--   , quizletAnswer : String
--   }


-- decodeSquare : Json.Decode.Decoder Square
-- decodeSquare =
--   Json.Decode.succeed Square
--     |: ("number" := Json.Decode.int)
--     |: ("guessedLetter" := Json.Decode.string)
--     |: ("fillable" := Json.Decode.bool)
--     |: ("x" := Json.Decode.int)
--     |: ("y" := Json.Decode.int)



-- getCrosswordzesByCrosswordIdSquares : Int -> Task.Task Http.Error (List (Square))
-- getCrosswordzesByCrosswordIdSquares quizId =
--   let
--     request =
--       { verb =
--           "GET"
--       , headers =
--           [ ( "Content-Type", "application/json" ) ]
--       , url =
--           "http://localhost:8081/"
--             ++ "crosswords"
--             ++ "/"
--             ++ (quizId |> toString |> Http.uriEncode)
--             ++ "/"
--             ++ "quizlets"
--       , body =
--           Http.empty
--       }
--   in
--     Http.fromJson
--       (Json.Decode.list decodeSquare)
--       (Http.send Http.defaultSettings request)


-- getSquaresById : Int -> Task.Task Http.Error (Square)
-- getSquaresById id =
--   let
--     request =
--       { verb =
--           "GET"
--       , headers =
--           [ ( "Content-Type", "application/json" ) ]
--       , url =
--           "/"
--             ++ "quizlets"
--             ++ "/"
--             ++ (id |> toString |> Http.uriEncode)
--       , body =
--           Http.empty
--       }
--   in
--     Http.fromJson
--       decodeSquare
--       (Http.send Http.defaultSettings request)


-- postSquares : Square -> Task.Task Http.Error (Int)
-- postSquares body =
--   let
--     request =
--       { verb =
--           "POST"
--       , headers =
--           [ ( "Content-Type", "application/json" ) ]
--       , url =
--           "/" ++ "quizlets"
--       , body =
--           Http.string (Json.Encode.encode 0 (encodeSquare body))
--       }
--   in
--     Http.fromJson
--       Json.Decode.int
--       (Http.send Http.defaultSettings request)
