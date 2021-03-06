module Main exposing (..)

import Html.App as App
import Crossword exposing (initialModel, update, view)

main : Program Never
main =
    App.program
       { init = initialModel
       , view = view
       , update = update
       , subscriptions = \_ -> Sub.none
       }

-- port setStorage : Model -> Cmd msg

-- port focus : String -> Cmd msg

-- withSetStorage : (Model, Cmd Msg) -> (Model, Cmd Msg)
-- withSetStorage (model, cmds) =
--     (model, Cmd.batch [ setStorage model, cmds ])
