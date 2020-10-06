module Game exposing (Game, createGame, icon, id, playGame, refreshGames, sortById, sortByLastPlayed, title)

import Time exposing (posixToMillis)


type Game
    = Game Internals


type alias Internals =
    { id : Int
    , title : String
    , icon : String
    , lastPlayed : Time.Posix
    }


createGame : Int -> String -> String -> List Game -> List Game
createGame gameId gameTitle gameIcon games =
    Game
        { id = gameId
        , title = gameTitle
        , icon = gameIcon
        , lastPlayed = Time.millisToPosix 0
        }
        :: games


playGame : Time.Posix -> Game -> Game
playGame playtime (Game internals) =
    Game { internals | lastPlayed = playtime }


refreshGames : Game -> Game -> List Game -> List Game
refreshGames current new games =
    games
        |> List.map (refreshGame current new)
        |> List.sortWith compareGameById
        |> List.sortWith compareGameByLastPlayed


refreshGame : Game -> Game -> Game -> Game
refreshGame searched substitute current =
    if current == searched then
        substitute

    else
        current


sortById : List Game -> List Game
sortById games =
    games |> List.sortWith compareGameById


sortByLastPlayed : List Game -> List Game
sortByLastPlayed games =
    games
        |> List.sortWith compareGameByLastPlayed


compareGameById : Game -> Game -> Order
compareGameById (Game a) (Game b) =
    compare b.id a.id


compareGameByLastPlayed : Game -> Game -> Order
compareGameByLastPlayed (Game a) (Game b) =
    compare (posixToMillis b.lastPlayed) (posixToMillis a.lastPlayed)



-- INFO


id : Game -> Int
id (Game internals) =
    internals.id


title : Game -> String
title (Game internals) =
    internals.title


icon : Game -> String
icon (Game internals) =
    internals.icon
