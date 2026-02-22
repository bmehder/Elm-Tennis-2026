module View exposing (view)

import Html exposing (Html, button, div, h1, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.match of
        MatchInProgress details ->
            viewMatchInProgress details

        MatchFinished winner sets ->
            viewMatchFinished winner sets



-- MATCH IN PROGRESS


viewMatchInProgress :
    { completedSets : List SetResult
    , currentSet : SetResult
    }
    -> Html Msg
viewMatchInProgress details =
    div []
        [ h1 [ class "grid justify-content-center" ]
            [ text "🎾 Elm Tennis 2026" ]
        , viewScoreboard details.completedSets details.currentSet
        , div [ class "flex justify-content-center gap-0-5" ]
            [ button [ onClick (PlayerScores PlayerOne) ]
                [ text "Player 1 Scores" ]
            , button [ onClick (PlayerScores PlayerTwo) ]
                [ text "Player 2 Scores" ]
            ]
        ]


viewMatchFinished : Player -> List SetResult -> Html Msg
viewMatchFinished winner sets =
    let
        reversed =
            List.reverse sets

        currentSet =
            case List.head reversed of
                Just set ->
                    set

                Nothing ->
                    -- Fallback: render an empty in-progress set shape
                    SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Ongoing Love Love)

        completedSets =
            List.reverse (List.drop 1 reversed)
    in
    div []
        [ h1 [ class "grid justify-content-center" ]
            [ text "🎾 Elm Tennis 2026" ]
        , viewScoreboard completedSets currentSet
        , div [ class "flex justify-content-center" ]
            [ strong []
                [ text ("Winner: " ++ playerToString winner) ]
            ]
        ]



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
            getSetScore 0 allSets

        set2 =
            getSetScore 1 allSets

        set3 =
            getSetScore 2 allSets

        ( p1Points, p2Points ) =
            getCurrentGamePoints currentSet
    in
    div [ class "scoreboard" ]
        [ div [ class "heading" ] [ text "Player" ]
        , div [ class "heading" ] [ text "Set 1" ]
        , div [ class "heading" ] [ text "Set 2" ]
        , div [ class "heading" ] [ text "Set 3" ]
        , div [ class "heading" ] [ text "Points" ]

        -- Player 1 row
        , div [ class "player" ] [ text "Player 1" ]
        , div [] [ strong [] [ text set1.p1 ] ]
        , div [] [ text set2.p1 ]
        , div [] [ text set3.p1 ]
        , div [] [ text p1Points ]

        -- Player 2 row
        , div [ class "player" ] [ text "Player 2" ]
        , div [] [ text set1.p2 ]
        , div [] [ text set2.p2 ]
        , div [] [ text set3.p2 ]
        , div [] [ text p2Points ]
        ]


-- HELPERS


type alias SetScoreView =
    { p1 : String
    , p2 : String
    }


getSetScore : Int -> List SetResult -> SetScoreView
getSetScore index sets =
    case List.drop index sets |> List.head of
        Nothing ->
            { p1 = "0", p2 = "0" }

        Just setResult ->
            case setResult of
                SetInProgress score _ ->
                    { p1 = String.fromInt score.playerOnePoint
                    , p2 = String.fromInt score.playerTwoPoint
                    }

                SetFinished _ score _ ->
                    { p1 = String.fromInt score.playerOnePoint
                    , p2 = String.fromInt score.playerTwoPoint
                    }


getCurrentGamePoints : SetResult -> ( String, String )
getCurrentGamePoints setResult =
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