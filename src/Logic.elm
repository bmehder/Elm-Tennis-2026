module Logic exposing
    ( countSetWins
    , hasPlayerWonSet
    , incrementSetScore
    , matchWinner
    , nextPoint
    , scorePoint
    , setsToWinToInt
    , updateMatch
    , updatePoint
    , updateSet
    )

import Types exposing (..)


applyIfMatchInProgress :
    ({ completedSets : List SetResult, currentSet : SetResult } -> Match)
    -> Match
    -> Match
applyIfMatchInProgress fn match =
    case match of
        MatchInProgress details ->
            fn details

        MatchFinished _ _ ->
            match


updatePoint : Player -> Match -> Match
updatePoint pointWinner =
    applyIfMatchInProgress <|
        \{ completedSets, currentSet } ->
            let
                ( newSets, newCurrentSet ) =
                    case currentSet of
                        SetInProgress setScore gameState ->
                            ( completedSets
                            , SetInProgress setScore (scorePoint pointWinner gameState)
                            )

                        _ ->
                            ( completedSets, currentSet )
            in
            MatchInProgress { completedSets = newSets, currentSet = newCurrentSet }


updateSet : Match -> Match
updateSet =
    applyIfMatchInProgress <|
        \{ completedSets, currentSet } ->
            let
                result =
                    case currentSet of
                        SetInProgress score gameState ->
                            case gameState of
                                GameWon winner ->
                                    let
                                        newScore =
                                            incrementSetScore winner score

                                        nextGame =
                                            if newScore.playerOnePoint == 6 && newScore.playerTwoPoint == 6 then
                                                Tiebreak 0 0

                                            else
                                                Ongoing Love Love
                                    in
                                    if hasPlayerWonSet newScore then
                                        ( completedSets ++ [ SetFinished winner newScore Nothing ]
                                        , SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Ongoing Love Love)
                                        )

                                    else
                                        ( completedSets
                                        , SetInProgress newScore nextGame
                                        )

                                TiebreakWon winner tb ->
                                    let
                                        newScore =
                                            incrementSetScore winner score
                                    in
                                    ( completedSets ++ [ SetFinished winner newScore (Just tb) ]
                                    , SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Ongoing Love Love)
                                    )

                                _ ->
                                    ( completedSets, currentSet )

                        _ ->
                            ( completedSets, currentSet )
            in
            let
                ( newSets, newCurrentSet ) =
                    result
            in
            MatchInProgress { completedSets = newSets, currentSet = newCurrentSet }


updateMatch : SetsToWin -> Match -> Match
updateMatch setsToWin =
    applyIfMatchInProgress <|
        \details ->
            let
                allSets =
                    case details.currentSet of
                        SetFinished _ _ _ ->
                            details.completedSets ++ [ details.currentSet ]

                        _ ->
                            details.completedSets
            in
            case matchWinner (setsToWinToInt setsToWin) allSets of
                Just winner ->
                    MatchFinished winner allSets

                Nothing ->
                    MatchInProgress details


scorePoint : Player -> Game -> Game
scorePoint pointWinner gameState =
    case gameState of
        Ongoing playerOnePoint playerTwoPoint ->
            case ( pointWinner, playerOnePoint, playerTwoPoint ) of
                ( PlayerOne, Thirty, Forty ) ->
                    Deuce

                ( PlayerOne, Forty, _ ) ->
                    GameWon PlayerOne

                ( PlayerTwo, Forty, Thirty ) ->
                    Deuce

                ( PlayerTwo, _, Forty ) ->
                    GameWon PlayerTwo

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
