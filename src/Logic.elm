module Logic exposing
    ( updateMatch
    , updatePoint
    , updateSet
    )

import Types exposing (..)


updateIfMatchInProgress :
    ({ completedSets : List Set, currentSet : Set } -> Match)
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
                startNewSet : Set
                startNewSet =
                    SetInProgress { playerOneGames = 0, playerTwoGames = 0 } (Ongoing Love Love)

                nextGameAfterSetScore : SetScore -> Game
                nextGameAfterSetScore score =
                    if score.playerOneGames == 6 && score.playerTwoGames == 6 then
                        Tiebreak 0 0

                    else
                        Ongoing Love Love

                finalizeSet : Player -> SetScore -> Maybe TiebreakScore -> Match
                finalizeSet winner finalScore maybeTb =
                    MatchInProgress
                        { completedSets = completedSets ++ [ SetFinished winner finalScore maybeTb ]
                        , currentSet = startNewSet
                        }

                continueSetWithUpdatedScore : SetScore -> Match
                continueSetWithUpdatedScore updatedScore =
                    MatchInProgress
                        { completedSets = completedSets
                        , currentSet = SetInProgress updatedScore (nextGameAfterSetScore updatedScore)
                        }

                resolveGameWin : Player -> SetScore -> Match
                resolveGameWin winner score =
                    let
                        updatedScore =
                            incrementSetScore winner score
                    in
                    if hasPlayerWonSet updatedScore then
                        finalizeSet winner updatedScore Nothing

                    else
                        continueSetWithUpdatedScore updatedScore

                resolveTiebreakWin : Player -> SetScore -> TiebreakScore -> Match
                resolveTiebreakWin winner score tb =
                    let
                        updatedScore =
                            incrementSetScore winner score
                    in
                    finalizeSet winner updatedScore (Just tb)

                noSetChange : Match
                noSetChange =
                    MatchInProgress
                        { completedSets = completedSets
                        , currentSet = currentSet
                        }
            in
            case currentSet of
                SetInProgress score gameState ->
                    case gameState of
                        GameWon winner ->
                            resolveGameWin winner score

                        TiebreakWon winner tb ->
                            resolveTiebreakWin winner score tb

                        _ ->
                            noSetChange

                _ ->
                    noSetChange


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
        Ongoing playerOneGames playerTwoPoint ->
            case ( pointWinner, playerOneGames, playerTwoPoint ) of
                ( PlayerOne, Thirty, Forty ) ->
                    Deuce

                ( PlayerOne, Forty, _ ) ->
                    GameWon PlayerOne

                ( PlayerTwo, Forty, Thirty ) ->
                    Deuce

                ( PlayerTwo, _, Forty ) ->
                    GameWon PlayerTwo

                ( PlayerOne, _, _ ) ->
                    Ongoing (nextPoint playerOneGames) playerTwoPoint

                ( PlayerTwo, _, _ ) ->
                    Ongoing playerOneGames (nextPoint playerTwoPoint)

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
            { setScore | playerOneGames = setScore.playerOneGames + 1 }

        PlayerTwo ->
            { setScore | playerTwoGames = setScore.playerTwoGames + 1 }


setsToWinToInt : SetsToWin -> Int
setsToWinToInt setsToWin =
    case setsToWin of
        BestOfThree ->
            2

        BestOfFive ->
            3


hasPlayerWonSet : SetScore -> Bool
hasPlayerWonSet { playerOneGames, playerTwoGames } =
    let
        diff =
            abs (playerOneGames - playerTwoGames)

        maxScore =
            max playerOneGames playerTwoGames
    in
    maxScore >= 6 && diff >= 2


countSetWins : Player -> List Set -> Int
countSetWins player sets =
    sets
        |> List.filter
            (\set ->
                case set of
                    SetFinished winner _ _ ->
                        winner == player

                    _ ->
                        False
            )
        |> List.length


matchWinner : Int -> List Set -> Maybe Player
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
