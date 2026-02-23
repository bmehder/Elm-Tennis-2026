module View exposing (view)

import Html exposing (Html, button, div, h1, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)



-- ROOT


view : Model -> Html Msg
view model =
    case model.match of
        MatchInProgress details ->
            viewMatchInProgress details

        MatchFinished winner sets ->
            viewMatchFinished winner sets



-- MATCH RENDERING


viewMatchInProgress :
    { completedSets : List SetResult
    , currentSet : SetResult
    }
    -> Html Msg
viewMatchInProgress details =
    viewMatchLayout
        details.completedSets
        details.currentSet
        scoringButtons


viewMatchFinished : Player -> List SetResult -> Html Msg
viewMatchFinished winner sets =
    let
        ( completedSets, currentSet ) =
            splitLastSet sets
    in
    viewMatchLayout
        completedSets
        currentSet
        (finishedFooter winner)



-- MATCH LAYOUT


viewMatchLayout :
    List SetResult
    -> SetResult
    -> Html Msg
    -> Html Msg
viewMatchLayout completedSets currentSet footer =
    div [ class "grid justify-items-center gap-1-5" ]
        [ h1 [ ]
            [ text "🎾 Elm Tennis 2026" ]
        , viewScoreboard completedSets currentSet
        , footer
        ]


scoringButtons : Html Msg
scoringButtons =
    div [ class "flex gap-0-5" ]
        [ button [ onClick (PlayerWinsPoint PlayerOne) ]
            [ text "Player 1 Scores" ]
        , button [ onClick (PlayerWinsPoint PlayerTwo) ]
            [ text "Player 2 Scores" ]
        ]


finishedFooter : Player -> Html Msg
finishedFooter winner =
    div [ class "grid justify-items-center gap-1" ]
        [ strong []
            [ text ("Winner: " ++ playerToString winner) ]
        , button [ onClick NewMatch ]
            [ text "Start New Match" ]
        ]


splitLastSet : List SetResult -> ( List SetResult, SetResult )
splitLastSet sets =
    case List.reverse sets of
        last :: rest ->
            ( List.reverse rest, last )

        [] ->
            ( []
            , SetInProgress { playerOneGames = 0, playerTwoGames = 0 } (Ongoing Love Love)
            )



-- SCOREBOARD


viewScoreboard :
    List SetResult
    -> SetResult
    -> Html Msg
viewScoreboard completedSets currentSet =
    let
        allSets =
            completedSets ++ [ currentSet ]

        set1 =
            formatSetScore 0 allSets

        set2 =
            formatSetScore 1 allSets

        set3 =
            formatSetScore 2 allSets

        ( p1Points, p2Points ) =
            currentGamePoints currentSet
    in
    div [ class "scoreboard" ]
        [ div [ class "heading" ] [ text "Player" ]
        , div [ class "heading" ] [ text "Set 1" ]
        , div [ class "heading" ] [ text "Set 2" ]
        , div [ class "heading" ] [ text "Set 3" ]
        , div [ class "heading" ] [ text "Points" ]

        -- Player 1
        , playerLabel "Player 1"
        , div [] [ set1.playerOne ]
        , div [] [ set2.playerOne ]
        , div [] [ set3.playerOne ]
        , div [] [ text p1Points ]

        -- Player 2
        , playerLabel "Player 2"
        , div [] [ set1.playerTwo ]
        , div [] [ set2.playerTwo ]
        , div [] [ set3.playerTwo ]
        , div [] [ text p2Points ]
        ]


playerLabel : String -> Html Msg
playerLabel name =
    div [ class "player" ] [ text name ]



-- SET SCORE FORMAT


type alias SetScoreDisplay =
    { playerOne : Html Msg
    , playerTwo : Html Msg
    }


formatSetScore : Int -> List SetResult -> SetScoreDisplay
formatSetScore index sets =
    case List.drop index sets |> List.head of
        Nothing ->
            emptySetDisplay

        Just setResult ->
            case setResult of
                SetInProgress setScore _ ->
                    normalSetDisplay setScore

                SetFinished winner setScore maybeTb ->
                    finishedSetDisplay winner setScore maybeTb


emptySetDisplay : SetScoreDisplay
emptySetDisplay =
    { playerOne = text "0"
    , playerTwo = text "0"
    }


normalSetDisplay : SetScore -> SetScoreDisplay
normalSetDisplay setScore =
    { playerOne = text (String.fromInt setScore.playerOneGames)
    , playerTwo = text (String.fromInt setScore.playerTwoGames)
    }


finishedSetDisplay :
    Player
    -> SetScore
    -> Maybe TiebreakScore
    -> SetScoreDisplay
finishedSetDisplay winner setScore maybeTb =
    let
        baseP1 =
            String.fromInt setScore.playerOneGames

        baseP2 =
            String.fromInt setScore.playerTwoGames

        ( p1Score, p2Score ) =
            applyTiebreak winner baseP1 baseP2 maybeTb
    in
    case winner of
        PlayerOne ->
            { playerOne = strong [] [ text p1Score ]
            , playerTwo = text p2Score
            }

        PlayerTwo ->
            { playerOne = text p1Score
            , playerTwo = strong [] [ text p2Score ]
            }


applyTiebreak :
    Player
    -> String
    -> String
    -> Maybe TiebreakScore
    -> ( String, String )
applyTiebreak winner p1 p2 maybeTb =
    case maybeTb of
        Nothing ->
            ( p1, p2 )

        Just tb ->
            case winner of
                PlayerOne ->
                    ( p1
                    , p2 ++ " (" ++ String.fromInt tb.playerTwoPoint ++ ")"
                    )

                PlayerTwo ->
                    ( p1 ++ " (" ++ String.fromInt tb.playerOnePoint ++ ")"
                    , p2
                    )



-- CURRENT GAME DISPLAY


currentGamePoints : SetResult -> ( String, String )
currentGamePoints setResult =
    case setResult of
        SetInProgress _ game ->
            case game of
                Ongoing p1 p2 ->
                    ( pointToString p1, pointToString p2 )

                Deuce ->
                    ( "40", "40" )

                Advantage PlayerOne ->
                    ( "Ad", "" )

                Advantage PlayerTwo ->
                    ( "", "Ad" )

                GameWon PlayerOne ->
                    ( "Game", "" )

                GameWon PlayerTwo ->
                    ( "", "Game" )

                Tiebreak p1 p2 ->
                    ( String.fromInt p1, String.fromInt p2 )

                TiebreakWon _ tb ->
                    ( String.fromInt tb.playerOnePoint
                    , String.fromInt tb.playerTwoPoint
                    )

        SetFinished _ _ _ ->
            ( "0", "0" )



-- STRING HELPERS


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