module Player exposing (Player, createPlayer, icon, name)


type Player
    = Player Internals


type alias Internals =
    { id : Int
    , name : String
    , icon : String
    }


createPlayer : String -> String -> List Player -> List Player
createPlayer playerName playerIcon players =
    Player
        { id = List.length players + 1
        , name = playerName
        , icon = playerIcon
        }
        :: players



-- INFO


name : Player -> String
name (Player internals) =
    internals.name


icon : Player -> String
icon (Player internals) =
    internals.icon
