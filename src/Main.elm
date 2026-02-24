module Main exposing (main)

import Browser
import Logic exposing (updateMatch, updatePoint, updateSet)
import Types exposing (..)
import View exposing (view)


initialModel : Model
initialModel =
    { match = MatchNotStarted
    , config = { setsToWin = BestOfThree }
    }


newMatch : Match
newMatch =
    MatchInProgress
        { completedSets = []
        , currentSet =
            SetInProgress
                { playerOneGames = 0, playerTwoGames = 0 }
                (Ongoing Love Love)
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
        PlayerWinsPoint player ->
            let
                nextMatchState =
                    model.match
                        |> updatePoint player
                        |> updateSet
                        |> updateMatch model.config.setsToWin
            in
            { model | match = nextMatchState }

        NewMatch ->
            { model | match = newMatch }

        SetMatchLength newSetsToWin ->
            let
                config =
                    model.config

                updatedConfig =
                    { config | setsToWin = newSetsToWin }
            in
            { model
                | config = updatedConfig
                , match = newMatch
            }
