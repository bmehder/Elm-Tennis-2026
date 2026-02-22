module Types exposing
    ( Game(..)
    , Match(..)
    , MatchConfig
    , Model
    , Msg(..)
    , Player(..)
    , Point(..)
    , SetResult(..)
    , SetScore
    , SetsToWin(..)
    , TiebreakScore
    )

-- PLAYERS


type Player
    = PlayerOne
    | PlayerTwo



-- GAME


type Point
    = Love
    | Fifteen
    | Thirty
    | Forty


type Game
    = Ongoing Point Point
    | Deuce
    | Advantage Player
    | GameWon Player
    | Tiebreak Int Int
    | TiebreakWon Player TiebreakScore



-- SET


type alias SetScore =
    { playerOnePoint : Int
    , playerTwoPoint : Int
    }


type alias TiebreakScore =
    { playerOnePoint : Int
    , playerTwoPoint : Int
    }


type SetResult
    = SetInProgress SetScore Game
    | SetFinished Player SetScore (Maybe TiebreakScore)



-- MATCH


type Match
    = MatchInProgress
        { completedSets : List SetResult
        , currentSet : SetResult
        }
    | MatchFinished Player (List SetResult)


type SetsToWin
    = BestOfThree
    | BestOfFive


type alias MatchConfig =
    { setsToWin : SetsToWin
    }



-- APP MODEL


type alias Model =
    { match : Match
    , config : MatchConfig
    }


type Msg
    = PlayerScores Player
    | NewMatch
