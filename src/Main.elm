module Main exposing (main)

import Browser
import Logic exposing (..)
import Types exposing (..)
import View exposing (view)


initialModel : Model
initialModel =
    { match =
        MatchInProgress
            { completedSets = []
            , currentSet = SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Ongoing Love Love)
            }
    , config = { setsToWin = BestOfThree }
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlayerScores player ->
            let
                newMatch =
                    model.match
                        |> updatePoint player
                        |> updateSet
                        |> updateMatch model.config.setsToWin
            in
            { model | match = newMatch }

        NewMatch ->
            { model | match = initialModel.match }
