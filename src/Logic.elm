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


updateIfMatchInProgress :
    ({ completedSets : List SetResult, currentSet : SetResult } -> Match)
    -> Match
    -> Match
updateIfMatchInProgress fn match =
    case match of
        MatchInProgress details ->
            fn details

        MatchFinished _ _ ->
            match


updatePoint : Player -> Match -> Match
updatePoint pointWinner =
    updateIfMatchInProgress <|
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
    updateIfMatchInProgress <|
        \{ completedSets, currentSet } ->
            let
                emptySet : SetResult
                emptySet =
                    SetInProgress { playerOnePoint = 0, playerTwoPoint = 0 } (Ongoing Love Love)

                nextGameFor : SetScore -> Game
                nextGameFor score =
                    if score.playerOnePoint == 6 && score.playerTwoPoint == 6 then
                        Tiebreak 0 0

                    else
                        Ongoing Love Love

                finishSet : Player -> SetScore -> Maybe TiebreakScore -> Match
                finishSet winner finalScore tb =
                    MatchInProgress
                        { completedSets = completedSets ++ [ SetFinished winner finalScore tb ]
                        , currentSet = emptySet
                        }

                continueSet : SetScore -> Match
                continueSet score =
                    MatchInProgress
                        { completedSets = completedSets
                        , currentSet = SetInProgress score (nextGameFor score)
                        }

                applyGameWon : Player -> SetScore -> Match
                applyGameWon winner score =
                    let
                        newScore =
                            incrementSetScore winner score
                    in
                    if hasPlayerWonSet newScore then
                        finishSet winner newScore Nothing

                    else
                        continueSet newScore

                applyTiebreakWon : Player -> SetScore -> TiebreakScore -> Match
                applyTiebreakWon winner score tb =
                    let
                        newScore =
                            incrementSetScore winner score
                    in
                    finishSet winner newScore (Just tb)

                keepCurrent : Match
                keepCurrent =
                    MatchInProgress { completedSets = completedSets, currentSet = currentSet }
            in
            case currentSet of
                SetInProgress score gameState ->
                    case gameState of
                        GameWon winner ->
                            applyGameWon winner score

                        TiebreakWon winner tb ->
                            applyTiebreakWon winner score tb

                        _ ->
                            keepCurrent

                _ ->
                    keepCurrent


updateMatch : SetsToWin -> Match -> Match
updateMatch setsToWin =
    updateIfMatchInProgress <|
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
            (\set ->
                case set of
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
