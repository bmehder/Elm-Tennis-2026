module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, text)
import Html.Events


type alias MatchConfig =
    { setsToWin : Int
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }


type Player
    = PlayerOne
    | PlayerTwo


type Point
    = Love
    | Fifteen
    | Thirty
    | Forty


type GameState
    = Ongoing Point Point
    | Deuce
    | Advantage Player
    | GameWon Player
    | Tiebreak Int Int
    | TiebreakWon Player TiebreakScore


type alias SetScore =
    { playerOne : Int
    , playerTwo : Int
    }


type alias TiebreakScore =
    { playerOne : Int
    , playerTwo : Int
    }


type SetResult
    = SetInProgress SetScore (Maybe GameState)
    | SetFinished Player SetScore (Maybe TiebreakScore)


type Msg
    = PlayerScores Player


type Match
    = MatchInProgress
        { sets : List SetResult
        , currentSet : Maybe SetResult
        }
    | MatchFinished Player (List SetResult)


type alias Model =
    { match : Match
    , config : MatchConfig
    }


initialModel : Model
initialModel =
    { match =
        MatchInProgress
            { sets = []
            , currentSet = Just (SetInProgress { playerOne = 0, playerTwo = 0 } (Just (Ongoing Love Love)))
            }
    , config = { setsToWin = 2 }
    }


update : Msg -> Model -> Model
update (PlayerScores player) model =
    let
        newMatch =
            model.match
                |> updatePoint player
                |> updateSet
                |> updateMatch model.config.setsToWin
    in
    { model | match = newMatch }


applyIfMatchInProgress : ({ sets : List SetResult, currentSet : Maybe SetResult } -> Match) -> Match -> Match
applyIfMatchInProgress fn match =
    case match of
        MatchInProgress details ->
            fn details

        MatchFinished _ _ ->
            match


updatePoint : Player -> Match -> Match
updatePoint pointWinner =
    applyIfMatchInProgress <|
        \{ sets, currentSet } ->
            let
                ( newSets, newCurrentSet ) =
                    case currentSet of
                        Just (SetInProgress setScore maybeGame) ->
                            case maybeGame of
                                Just gameState ->
                                    -- Continue current game
                                    ( sets
                                    , Just (SetInProgress setScore (Just (scorePoint pointWinner gameState)))
                                    )

                                Nothing ->
                                    -- No active game → start a fresh one (tiebreak at 6–6)
                                    let
                                        start =
                                            if setScore.playerOne == 6 && setScore.playerTwo == 6 then
                                                Tiebreak 0 0
                                            else
                                                Ongoing Love Love

                                        newGame =
                                            scorePoint pointWinner start
                                    in
                                    ( sets
                                    , Just (SetInProgress setScore (Just newGame))
                                    )

                        -- If somehow the set is finished (shouldn't happen mid-match), do nothing
                        _ ->
                            ( sets, currentSet )
            in
            MatchInProgress { sets = newSets, currentSet = newCurrentSet }


updateSet : Match -> Match
updateSet =
    applyIfMatchInProgress <|
        \{ sets, currentSet } ->
            let
                result =
                    case currentSet of
                        Just (SetInProgress score (Just (GameWon winner))) ->
                            let
                                newScore =
                                    incrementSetScore winner score
                            in
                            if hasPlayerWonSet newScore then
                                ( sets ++ [ SetFinished winner newScore Nothing ]
                                , Just (SetInProgress { playerOne = 0, playerTwo = 0 } Nothing)
                                )
                            else
                                ( sets
                                , Just (SetInProgress newScore Nothing)
                                )

                        Just (SetInProgress score (Just (TiebreakWon winner tb))) ->
                            let
                                newScore =
                                    incrementSetScore winner score
                            in
                            ( sets ++ [ SetFinished winner newScore (Just tb) ]
                            , Just (SetInProgress { playerOne = 0, playerTwo = 0 } Nothing)
                            )

                        _ ->
                            ( sets, currentSet )
            in
            let
                ( newSets, newCurrentSet ) =
                    result
            in
            MatchInProgress { sets = newSets, currentSet = newCurrentSet }



updateMatch : Int -> Match -> Match
updateMatch setsToWin =
    applyIfMatchInProgress <|
        \details ->
            case matchWinner setsToWin details.sets of
                Just winner ->
                    MatchFinished winner details.sets

                Nothing ->
                    MatchInProgress details


scorePoint : Player -> GameState -> GameState
scorePoint pointWinner gameState =
    case gameState of
        Ongoing playerOnePoint playerTwoPoint ->
            case ( pointWinner, playerOnePoint, playerTwoPoint ) of
                -- Player 1 scoring
                ( PlayerOne, Thirty, Forty ) ->
                    Deuce

                ( PlayerOne, Forty, _ ) ->
                    GameWon PlayerOne

                -- Player 2 scoring (mirror logic, but same structure)
                ( PlayerTwo, Forty, Thirty ) ->
                    Deuce

                ( PlayerTwo, _, Forty ) ->
                    GameWon PlayerTwo

                -- Normal scoring
                ( PlayerOne, _, _ ) ->
                    Ongoing (nextPoint playerOnePoint) playerTwoPoint

                ( PlayerTwo, _, _ ) ->
                    Ongoing playerOnePoint (nextPoint playerTwoPoint)

        Deuce ->
            Advantage pointWinner

        Advantage player ->
            if player == pointWinner then
                GameWon pointWinner

            else
                Deuce

        Tiebreak playerOnePoints playerTwoPoints ->
            let
                ( newPlayerOnePoint, newPlayer2Point ) =
                    case pointWinner of
                        PlayerOne -> ( playerOnePoints + 1, playerTwoPoints )
                        PlayerTwo -> ( playerOnePoints, playerTwoPoints + 1 )

                pointDifference =
                    abs (newPlayerOnePoint - newPlayer2Point)

                highestScore =
                    max newPlayerOnePoint newPlayer2Point
            in
            if highestScore >= 7 && pointDifference >= 2 then
                case compare newPlayerOnePoint newPlayer2Point of
                    GT ->
                        TiebreakWon PlayerOne { playerOne = newPlayerOnePoint, playerTwo = newPlayer2Point }

                    LT ->
                        TiebreakWon PlayerTwo { playerOne = newPlayerOnePoint, playerTwo = newPlayer2Point }

                    EQ ->
                        Tiebreak newPlayerOnePoint newPlayer2Point
            else
                Tiebreak newPlayerOnePoint newPlayer2Point

        TiebreakWon _ _ ->
            gameState

        GameWon _ ->
            gameState


nextPoint : Point -> Point
nextPoint point =
    case point of
        Love ->
            Fifteen

        Fifteen ->
            Thirty

        Thirty ->
            Forty

        Forty ->
            Forty


incrementSetScore : Player -> SetScore -> SetScore
incrementSetScore player setScore =
    case player of
        PlayerOne ->
            { setScore | playerOne = setScore.playerOne + 1 }

        PlayerTwo ->
            { setScore | playerTwo = setScore.playerTwo + 1 }


hasPlayerWonSet : SetScore -> Bool
hasPlayerWonSet { playerOne, playerTwo } =
    let
        diff = abs (playerOne - playerTwo)
        maxScore = max playerOne playerTwo
    in
    maxScore >= 6 && diff >= 2

countSetWins : Player -> List SetResult -> Int
countSetWins player sets =
    List.length <|
        List.filter
            (\s ->
                case s of
                    SetFinished winner _ _ ->
                        winner == player

                    _ ->
                        False
            )
            sets


matchWinner : Int -> List SetResult -> Maybe Player
matchWinner setsToWin sets =
    let
        playerOneWins = countSetWins PlayerOne sets
        playerTwoWins = countSetWins PlayerTwo sets
    in
    if playerOneWins == setsToWin then
        Just PlayerOne
    else if playerTwoWins == setsToWin then
        Just PlayerTwo
    else
        Nothing


playerToString : Player -> String
playerToString player =
    case player of
        PlayerOne ->
            "Player 1"

        PlayerTwo ->
            "Player 2"


pointToString : Point -> String
pointToString point =
    case point of
        Love ->
            "0"

        Fifteen ->
            "15"

        Thirty ->
            "30"

        Forty ->
            "40"


view : Model -> Html Msg
view model =
    case model.match of
        MatchInProgress matchData ->
            let
                currentSetValue =
                    Maybe.withDefault (SetInProgress { playerOne = 0, playerTwo = 0 } (Just (Ongoing Love Love))) matchData.currentSet

                ( setScore, gameState ) =
                    case currentSetValue of
                        SetInProgress score maybeGame ->
                            ( score, Maybe.withDefault (Ongoing Love Love) maybeGame )

                        SetFinished winner score _ ->
                            ( score, GameWon winner )

                completedSetsText =
                    case matchData.sets of
                        [] ->
                            "Completed Sets: None"

                        sets ->
                            let
                                toScore setResult =
                                    case setResult of
                                        SetFinished _ score _ ->
                                            String.fromInt score.playerOne ++ " - " ++ String.fromInt score.playerTwo

                                        _ ->
                                            ""
                            in
                            "Completed Sets: " ++ String.join ", " (List.map toScore sets)

                gameText =
                    case gameState of
                        Ongoing p1 p2 ->
                            "Game: " ++ pointToString p1 ++ " - " ++ pointToString p2

                        Deuce ->
                            "Game: Deuce"

                        Advantage player ->
                            "Game: Advantage " ++ playerToString player

                        Tiebreak p1 p2 ->
                            "Game: Tiebreak "
                                ++ String.fromInt p1
                                ++ " - "
                                ++ String.fromInt p2

                        TiebreakWon player tb ->
                            "Tiebreak won by "
                                ++ playerToString player
                                ++ " ("
                                ++ String.fromInt tb.playerOne
                                ++ "-"
                                ++ String.fromInt tb.playerTwo
                                ++ ")"

                        GameWon player ->
                            "Game won by " ++ playerToString player

                setText =
                    "Set: " ++ String.fromInt setScore.playerOne ++ " - " ++ String.fromInt setScore.playerTwo
            in
            Html.div []
                [ Html.h1 [] [ text "Tennis Match" ]
                , Html.p [] [ text completedSetsText ]
                , Html.p [] [ text setText ]
                , Html.p [] [ text gameText ]
                , Html.button [ Html.Events.onClick (PlayerScores PlayerOne) ] [ text "Player 1 Scores" ]
                , Html.button [ Html.Events.onClick (PlayerScores PlayerTwo) ] [ text "Player 2 Scores" ]
                ]

        MatchFinished winner sets ->
            Html.div []
                [ Html.h1 [] [ text "Tennis Match" ]
                , Html.p [] [ text ("Winner: " ++ playerToString winner) ]
                , Html.p []
                    [ text
                        ("Final Sets: "
                            ++ String.join ", "
                                (List.map
                                    (\set ->
                                        case set of
                                            SetFinished _ score maybeTb ->
                                                case maybeTb of
                                                    Just tb ->
                                                        String.fromInt score.playerOne
                                                            ++ " - "
                                                            ++ String.fromInt score.playerTwo
                                                            ++ " (" 
                                                            ++ String.fromInt tb.playerOne
                                                            ++ "-"
                                                            ++ String.fromInt tb.playerTwo
                                                            ++ ")"
                                                    Nothing ->
                                                        String.fromInt score.playerOne
                                                            ++ " - "
                                                            ++ String.fromInt score.playerTwo
                                            _ ->
                                                ""
                                    )
                                    sets
                                )
                        )
                    ]
                ]
