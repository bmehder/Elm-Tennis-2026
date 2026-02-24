module Types exposing
    ( Game(..)
    , Match(..)
    , MatchConfig
    , Model
    , Msg(..)
    , Player(..)
    , Point(..)
    , Set(..)
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
    { playerOneGames : Int
    , playerTwoGames : Int
    }


type alias TiebreakScore =
    { playerOnePoint : Int
    , playerTwoPoint : Int
    }


type Set
    = SetInProgress SetScore Game
    | SetFinished Player SetScore (Maybe TiebreakScore)



-- MATCH


type Match
    = MatchNotStarted
    | MatchInProgress
        { completedSets : List Set
        , currentSet : Set
        }
    | MatchFinished Player (List Set)


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
    = PlayerWinsPoint Player
    | NewMatch
    | SetMatchLength SetsToWin
