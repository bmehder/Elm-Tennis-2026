module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, button, div, h1, p, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


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
    { playerOnePoint : Int
    , playerTwoPoint : Int
    }


type alias TiebreakScore =
    { playerOnePoint : Int
    , playerTwoPoint : Int
    }


type SetResult
    = SetInProgress SetScore (Maybe GameState)
    | SetFinished Player SetScore (Maybe TiebreakScore)


type Match
    = MatchInProgress
        { sets : List SetResult
        , currentSet : Maybe SetResult
        }
    | MatchFinished Player (List SetResult)


type SetsToWin
    = BestOfThree
    | BestOfFive


type alias MatchConfig =
    { setsToWin : SetsToWin
    }


type alias Model =
    { match : Match
    , config : MatchConfig
    }


type Msg
    = PlayerScores Player
    | NewMatch


initialModel : Model
initialModel =
    { match =
        MatchInProgress
            { sets = []
            , currentSet = Just (SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Just (Ongoing Love Love)))
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
                                            if setScore.playerOnePoint == 6 && setScore.playerTwoPoint == 6 then
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
                                , Just (SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } Nothing)
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
                            , Just (SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } Nothing)
                            )

                        _ ->
                            ( sets, currentSet )
            in
            let
                ( newSets, newCurrentSet ) =
                    result
            in
            MatchInProgress { sets = newSets, currentSet = newCurrentSet }


updateMatch : SetsToWin -> Match -> Match
updateMatch setsToWin =
    applyIfMatchInProgress <|
        \details ->
            case matchWinner (setsToWinToInt setsToWin) details.sets of
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
                        PlayerOne ->
                            ( playerOnePoints + 1, playerTwoPoints )

                        PlayerTwo ->
                            ( playerOnePoints, playerTwoPoints + 1 )

                pointDifference =
                    abs (newPlayerOnePoint - newPlayer2Point)

                highestScore =
                    max newPlayerOnePoint newPlayer2Point
            in
            if highestScore >= 7 && pointDifference >= 2 then
                case compare newPlayerOnePoint newPlayer2Point of
                    GT ->
                        TiebreakWon PlayerOne { playerOnePoint = newPlayerOnePoint, playerTwoPoint = newPlayer2Point }

                    LT ->
                        TiebreakWon PlayerTwo { playerOnePoint = newPlayerOnePoint, playerTwoPoint = newPlayer2Point }

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
            { setScore | playerOnePoint = setScore.playerOnePoint + 1 }

        PlayerTwo ->
            { setScore | playerTwoPoint = setScore.playerTwoPoint + 1 }


setsToWinToInt : SetsToWin -> Int
setsToWinToInt stw =
    case stw of
        BestOfThree ->
            2

        BestOfFive ->
            3


hasPlayerWonSet : SetScore -> Bool
hasPlayerWonSet { playerOnePoint, playerTwoPoint } =
    let
        diff =
            abs (playerOnePoint - playerTwoPoint)

        maxScore =
            max playerOnePoint playerTwoPoint
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
        playerOneWins =
            countSetWins PlayerOne sets

        playerTwoWins =
            countSetWins PlayerTwo sets
    in
    if playerOneWins == setsToWin then
        Just PlayerOne

    else if playerTwoWins == setsToWin then
        Just PlayerTwo

    else
        Nothing



-- VIEW (drop-in replacement)


view : Model -> Html Msg
view model =
    case model.match of
        MatchInProgress matchData ->
            viewMatchInProgress matchData

        MatchFinished winner sets ->
            viewMatchFinished winner sets


viewMatchLayout : { sets : List SetResult, currentSet : Maybe SetResult } -> Html Msg -> Html Msg
viewMatchLayout scoreboardData innerContent =
    -- keep this super simple to avoid list-append pitfalls
    div []
        [ h1 [ class "grid justify-content-center" ] [ text "🎾 Elm Tennis 2026" ]
        , viewScoreboard scoreboardData
        , innerContent
        ]


viewMatchInProgress : { sets : List SetResult, currentSet : Maybe SetResult } -> Html Msg
viewMatchInProgress matchData =
    viewMatchLayout matchData <|
        div [ class "flex justify-content-center gap-0-5" ]
            [ button [ onClick (PlayerScores PlayerOne) ] [ text "Player 1 Scores" ]
            , button [ onClick (PlayerScores PlayerTwo) ] [ text "Player 2 Scores" ]
            ]


viewMatchFinished : Player -> List SetResult -> Html Msg
viewMatchFinished winner sets =
    viewMatchLayout { sets = sets, currentSet = Nothing } <|
        div [ class "grid justify-content-center align-items-center" ]
            [ p [] [ text ("Winner: " ++ playerToString winner) ]
            , button [ onClick NewMatch ] [ text "New Match" ]
            ]


viewScoreboard : { sets : List SetResult, currentSet : Maybe SetResult } -> Html Msg
viewScoreboard matchData =
    let
        players : List Player
        players =
            [ PlayerOne, PlayerTwo ]

        -- We stick with 3 set columns to match Best-of-3;
        -- easy to tweak later if you want dynamic columns.
        setHeaders : List (Html Msg)
        setHeaders =
            List.map
                (\i -> div [ class "heading" ] [ text ("Set " ++ String.fromInt (i + 1)) ])
                [ 0, 1, 2 ]

        headersRow : List (Html Msg)
        headersRow =
            -- Use List.concat to avoid singleton-list ++ warnings
            List.concat
                [ [ div [ class "heading" ] [ text "Player" ] ]
                , setHeaders
                , [ div [ class "heading" ] [ text "Points" ] ]
                ]

        playerRow : Player -> List (Html Msg)
        playerRow player =
            [ div [ class "player" ] [ text (playerToString player) ]
            , div [] [ getSetDisplay player 0 matchData ]
            , div [] [ getSetDisplay player 1 matchData ]
            , div [] [ getSetDisplay player 2 matchData ]
            , div [] [ text (getCurrentGamePoints player matchData) ]
            ]
    in
    div [ class "scoreboard" ]
        (headersRow
            ++ List.concatMap playerRow players
        )


getCurrentGamePoints : Player -> { sets : List SetResult, currentSet : Maybe SetResult } -> String
getCurrentGamePoints player matchData =
    case matchData.currentSet of
        Just (SetInProgress _ (Just (Ongoing p1 p2))) ->
            case player of
                PlayerOne ->
                    pointToString p1

                PlayerTwo ->
                    pointToString p2

        Just (SetInProgress _ (Just (Tiebreak p1 p2))) ->
            case player of
                PlayerOne ->
                    String.fromInt p1

                PlayerTwo ->
                    String.fromInt p2

        Just (SetInProgress _ (Just Deuce)) ->
            "40"

        Just (SetInProgress _ (Just (Advantage p))) ->
            case player of
                PlayerOne ->
                    if p == PlayerOne then
                        "Ad"

                    else
                        "40"

                PlayerTwo ->
                    if p == PlayerTwo then
                        "Ad"

                    else
                        "40"

        Just (SetInProgress _ Nothing) ->
            "0"

        _ ->
            "-"


getSetDisplay : Player -> Int -> { sets : List SetResult, currentSet : Maybe SetResult } -> Html Msg
getSetDisplay player index matchData =
    case List.drop index matchData.sets |> List.head of
        -- ✅ Completed set
        Just (SetFinished winner score maybeTb) ->
            case maybeTb of
                -- ✅ Set had a tiebreak
                Just tb ->
                    case player of
                        PlayerOne ->
                            if score.playerOnePoint > score.playerTwoPoint then
                                strong [] [ text "7" ]

                            else
                                text (String.fromInt score.playerOnePoint ++ " (" ++ String.fromInt tb.playerOnePoint ++ ")")

                        PlayerTwo ->
                            if score.playerTwoPoint > score.playerOnePoint then
                                strong [] [ text "7" ]

                            else
                                text (String.fromInt score.playerTwoPoint ++ " (" ++ String.fromInt tb.playerTwoPoint ++ ")")

                -- ✅ Normal completed set (no tiebreak)
                Nothing ->
                    case player of
                        PlayerOne ->
                            if score.playerOnePoint > score.playerTwoPoint then
                                strong [] [ text (String.fromInt score.playerOnePoint) ]

                            else
                                text (String.fromInt score.playerOnePoint)

                        PlayerTwo ->
                            if score.playerTwoPoint > score.playerOnePoint then
                                strong [] [ text (String.fromInt score.playerTwoPoint) ]

                            else
                                text (String.fromInt score.playerTwoPoint)

        -- ✅ If it's the current set being played
        Nothing ->
            case matchData.currentSet of
                Just (SetInProgress score _) ->
                    if index == List.length matchData.sets then
                        case player of
                            PlayerOne ->
                                text (String.fromInt score.playerOnePoint)

                            PlayerTwo ->
                                text (String.fromInt score.playerTwoPoint)

                    else
                        text "0"

                -- future set placeholder
                _ ->
                    text "0"

        -- ✅ Safety fallback (should never hit this)
        Just (SetInProgress score _) ->
            case player of
                PlayerOne ->
                    text (String.fromInt score.playerOnePoint)

                PlayerTwo ->
                    text (String.fromInt score.playerTwoPoint)


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
