module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, text)
import Html.Attributes
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
    { playerOnePoint : Int
    , playerTwoPoint : Int
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
    | NewMatch


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
            , currentSet = Just (SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Just (Ongoing Love Love)))
            }
    , config = { setsToWin = 2 }
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
            { setScore | playerOnePoint = setScore.playerOnePoint + 1 }

        PlayerTwo ->
            { setScore | playerTwoPoint = setScore.playerTwoPoint + 1 }


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
            viewMatchInProgress matchData

        MatchFinished winner sets ->
            Html.div []
                [ Html.h1 [ Html.Attributes.class "grid justify-content-center" ] [ text "🎾 Elm Tennis 2026" ]
                , viewScoreboard { sets = sets, currentSet = Nothing }
                , Html.div [ Html.Attributes.class "grid justify-content-center align-items-center" ]
                    [ Html.p [] [ text ("Winner: " ++ playerToString winner) ]
                    , Html.button [ Html.Events.onClick NewMatch ] [ text "New Match" ]
                    ]
                ]


viewMatchInProgress : { sets : List SetResult, currentSet : Maybe SetResult } -> Html Msg
viewMatchInProgress matchData =
    Html.div []
        [ Html.h1 [ Html.Attributes.class "grid justify-content-center" ] [ text "🎾 Elm Tennis 2026" ]
        , viewScoreboard matchData
        , Html.div [ Html.Attributes.class "flex justify-content-center gap-0-5" ]
            [ Html.button [ Html.Events.onClick (PlayerScores PlayerOne) ] [ text "Player 1 Scores" ]
            , Html.button [ Html.Events.onClick (PlayerScores PlayerTwo) ] [ text "Player 2 Scores" ]
            ]
        ]


getSetScore : Player -> Int -> { sets : List SetResult, currentSet : Maybe SetResult } -> String
getSetScore player index matchData =
    case List.drop index matchData.sets |> List.head of
        Just (SetFinished _ score _) ->
            case player of
                PlayerOne ->
                    String.fromInt score.playerOnePoint

                PlayerTwo ->
                    String.fromInt score.playerTwoPoint

        _ ->
            "-"


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
                    "TB " ++ String.fromInt p1

                PlayerTwo ->
                    "TB " ++ String.fromInt p2

        Just (SetInProgress _ (Just Deuce)) ->
            "Deuce"

        Just (SetInProgress _ (Just (Advantage p))) ->
            "Ad " ++ playerToString p

        Just (SetInProgress _ (Just (GameWon p))) ->
            "Won by " ++ playerToString p

        Just (SetInProgress _ Nothing) ->
            "0"

        _ ->
            "-"



-- ✅ Updates view to use getSetDisplay everywhere


viewScoreboard : { sets : List SetResult, currentSet : Maybe SetResult } -> Html Msg
viewScoreboard matchData =
    Html.div [ Html.Attributes.class "scoreboard" ]
        [ Html.div [ Html.Attributes.class "heading" ] [ text "Player" ]
        , Html.div [ Html.Attributes.class "heading" ] [ text "Set 1" ]
        , Html.div [ Html.Attributes.class "heading" ] [ text "Set 2" ]
        , Html.div [ Html.Attributes.class "heading" ] [ text "Set 3" ]
        , Html.div [ Html.Attributes.class "heading" ] [ text "Points" ]

        -- ✅ Player 1 row
        , Html.div [ Html.Attributes.class "player" ] [ text "Player 1" ]
        , Html.div [] [ getSetDisplay PlayerOne 0 matchData ]
        , Html.div [] [ getSetDisplay PlayerOne 1 matchData ]
        , Html.div [] [ getSetDisplay PlayerOne 2 matchData ]
        , Html.div [] [ text (getCurrentGamePoints PlayerOne matchData) ]

        -- ✅ Player 2 row
        , Html.div [ Html.Attributes.class "player" ] [ text "Player 2" ]
        , Html.div [] [ getSetDisplay PlayerTwo 0 matchData ]
        , Html.div [] [ getSetDisplay PlayerTwo 1 matchData ]
        , Html.div [] [ getSetDisplay PlayerTwo 2 matchData ]
        , Html.div [] [ text (getCurrentGamePoints PlayerTwo matchData) ]
        ]



-- Get how a set should appear for a given player and set index
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
                                Html.strong [] [ text "7" ]
                            else
                                text (String.fromInt score.playerOnePoint ++ " (" ++ String.fromInt tb.playerOne ++ ")")
                        PlayerTwo ->
                            if score.playerTwoPoint > score.playerOnePoint then
                                Html.strong [] [ text "7" ]
                            else
                                text (String.fromInt score.playerTwoPoint ++ " (" ++ String.fromInt tb.playerTwo ++ ")")
                -- ✅ Normal completed set (no tiebreak)
                Nothing ->
                    case player of
                        PlayerOne ->
                            if score.playerOnePoint > score.playerTwoPoint then
                                Html.strong [] [ text (String.fromInt score.playerOnePoint) ]
                            else
                                text (String.fromInt score.playerOnePoint)
                        PlayerTwo ->
                            if score.playerTwoPoint > score.playerOnePoint then
                                Html.strong [] [ text (String.fromInt score.playerTwoPoint) ]
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


viewMatchFinished : Player -> List SetResult -> Html Msg
viewMatchFinished winner sets =
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
                                                let
                                                    p1 =
                                                        String.fromInt score.playerOnePoint

                                                    p2 =
                                                        String.fromInt score.playerTwoPoint

                                                    tb1 =
                                                        String.fromInt tb.playerOne

                                                    tb2 =
                                                        String.fromInt tb.playerTwo
                                                in
                                                if score.playerOnePoint > score.playerTwoPoint then
                                                    p1 ++ " - " ++ p2 ++ " (" ++ tb2 ++ ")"

                                                else
                                                    p1 ++ " - " ++ p2 ++ " (" ++ tb1 ++ ")"

                                            Nothing ->
                                                String.fromInt score.playerOnePoint ++ " - " ++ String.fromInt score.playerTwoPoint

                                    _ ->
                                        ""
                            )
                            sets
                        )
                )
            ]
        ]



-- Return full set score as a string like "6-4" or "7-6 (7-5)"
-- Get the number of games won by a specific player in a specific set


getSetGameCount : Player -> Int -> { sets : List SetResult, currentSet : Maybe SetResult } -> String
getSetGameCount player index matchData =
    case List.drop index matchData.sets |> List.head of
        Just (SetFinished _ score _) ->
            case player of
                PlayerOne ->
                    String.fromInt score.playerOnePoint

                PlayerTwo ->
                    String.fromInt score.playerTwoPoint

        -- If this is the current active set (not finished yet)
        Nothing ->
            case matchData.currentSet of
                Just (SetInProgress score _) ->
                    if index == List.length matchData.sets then
                        case player of
                            PlayerOne ->
                                String.fromInt score.playerOnePoint

                            PlayerTwo ->
                                String.fromInt score.playerTwoPoint

                    else
                        "0"

                _ ->
                    "0"

        -- If a set is stored but not finished (unlikely, but safe)
        Just (SetInProgress score _) ->
            case player of
                PlayerOne ->
                    String.fromInt score.playerOnePoint

                PlayerTwo ->
                    String.fromInt score.playerTwoPoint
