module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Game as G exposing (Game, createGame, icon, playGame, refreshGames, sortById, sortByLastPlayed, title)
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Ionicon
import Ionicon.Ios as IconIos
import Player as P exposing (Player, createPlayer, icon, name)
import Task
import Time


type alias Model =
    { displayMode : DisplayMode
    , gamesList : List Game
    , selectedGame : Maybe Game
    , selectedPlayer : Maybe Player
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    }


initialModel : Model
initialModel =
    { displayMode = Day
    , gamesList = initialGamesList
    , selectedGame = Nothing
    , selectedPlayer = List.head playerList
    , timeZone = Time.utc
    , currentTime = Time.millisToPosix 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform AdjustTimeZone Time.here
    )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = mainView
        , update = update
        , subscriptions = subscriptions
        }


type DisplayMode
    = Night
    | Day


type alias ThemeColors =
    { mainColors :
        { backgroundColor : Color
        , fontColor : Color
        }
    , buttonColors :
        { backgroundColor : Color
        , borderColor : Color
        , mouseOverColor : Color
        }
    , playerItemColors :
        { backgroundColor : Color
        , borderColor : Color
        , mouseOverColor : Color
        }
    }


type alias DisplayTheme msg =
    { mainColors : List (Attribute msg)
    , buttonColors : List (Attribute msg)
    , playerItemStyle : List (Attribute msg)
    , gameStyle : List (Attribute msg)
    }


setThemeColors : Model -> ThemeColors
setThemeColors model =
    let
        {- Use current time to dynamically modify border alpha value. -}
        actualMillis =
            getMillis model.timeZone model.currentTime

        hoverBorderAlpha =
            if actualMillis <= 300 then
                1.0 - (toFloat actualMillis / 1000)

            else if actualMillis >= 800 then
                1.0

            else
                toFloat actualMillis / 1000

        mainColors =
            case model.displayMode of
                Day ->
                    { backgroundColor = rgb255 235 235 235
                    , fontColor = rgb255 41 41 41
                    }

                Night ->
                    { backgroundColor = rgb255 44 44 44
                    , fontColor = rgb255 255 255 255
                    }

        buttonColors =
            case model.displayMode of
                Day ->
                    { backgroundColor = rgb255 255 255 255
                    , borderColor = rgb255 218 218 218
                    , mouseOverColor = rgba255 46 215 200 hoverBorderAlpha
                    }

                Night ->
                    { backgroundColor = rgb255 80 80 80
                    , borderColor = rgb255 40 40 40
                    , mouseOverColor = rgba255 255 255 255 hoverBorderAlpha
                    }

        playerItemColors =
            case model.displayMode of
                Day ->
                    { borderColor = rgb255 255 255 255
                    , backgroundColor = rgb255 255 255 255
                    , mouseOverColor = rgba255 46 215 200 hoverBorderAlpha
                    }

                Night ->
                    { borderColor = rgb255 88 77 75
                    , backgroundColor = rgb255 255 255 255
                    , mouseOverColor = rgba255 46 215 200 hoverBorderAlpha
                    }
    in
    ThemeColors mainColors buttonColors playerItemColors


displayTheme : ThemeColors -> DisplayTheme msg
displayTheme themeColors =
    let
        mainColors =
            themeColors.mainColors

        buttonColors =
            themeColors.buttonColors

        playerItemColors =
            themeColors.playerItemColors
    in
    { mainColors =
        [ Background.color <| mainColors.backgroundColor
        , Font.color <| mainColors.fontColor
        ]
    , playerItemStyle =
        [ width <| fill
        , height <| fill
        , Border.color <| playerItemColors.borderColor
        , Border.width 4
        , Border.rounded 90
        , clip
        , Background.color <| playerItemColors.backgroundColor
        , mouseOver
            [ Border.shadow
                { blur = 0
                , color = playerItemColors.mouseOverColor
                , offset = ( 0, 0 )
                , size = 5
                }
            ]
        , focused
            [ Border.shadow
                { blur = 0
                , color = playerItemColors.mouseOverColor
                , offset = ( 0, 0 )
                , size = 5
                }
            ]
        ]
    , gameStyle =
        [ mouseOver
            [ Border.shadow
                { blur = 0
                , color = buttonColors.mouseOverColor
                , offset = ( 0, 0 )
                , size = 5
                }
            ]
        , focused
            [ Border.shadow
                { blur = 0
                , color = buttonColors.mouseOverColor
                , offset = ( 0, 0 )
                , size = 5
                }
            ]
        ]
    , buttonColors =
        [ Background.color <| buttonColors.backgroundColor
        , Border.shadow
            { blur = 0
            , color = buttonColors.borderColor
            , offset = ( 0, 0 )
            , size = 0.8
            }
        , mouseOver
            [ Border.shadow
                { blur = 0
                , color = buttonColors.mouseOverColor
                , offset = ( 0, 0 )
                , size = 5
                }
            ]
        , focused
            [ Border.shadow
                { blur = 0
                , color = buttonColors.mouseOverColor
                , offset = ( 0, 0 )
                , size = 5
                }
            ]
        ]
    }


type alias QuickAction msg =
    { title : String
    , icon : Icon msg
    , color : Color
    , action : Msg
    }



--- MSG


type Msg
    = NoOp
    | ClickedThemeButton
    | SelectGame Game
    | SelectPlayer Player
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone



--- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedThemeButton ->
            if model.displayMode == Day then
                ( { model | displayMode = Night }, Cmd.none )

            else
                ( { model | displayMode = Day }, Cmd.none )

        SelectGame game ->
            if model.selectedGame == Just game then
                ( { model | selectedGame = Nothing }, Cmd.none )

            else
                let
                    selectedGame =
                        playGame model.currentTime game

                    gamesList =
                        refreshGames game selectedGame model.gamesList
                in
                ( { model | selectedGame = Just selectedGame, gamesList = gamesList }, Cmd.none )

        SelectPlayer player ->
            if model.selectedPlayer == Just player then
                ( model, Cmd.none )

            else
                ( { model | selectedPlayer = Just player, selectedGame = Nothing }, Cmd.none )

        Tick newTime ->
            ( { model | currentTime = newTime }, Cmd.none )

        AdjustTimeZone newTimeZone ->
            ( { model | timeZone = newTimeZone }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 60 Tick


playerList : List Player
playerList =
    []
        |> createPlayer "mats" "public/assets/icons/players/player_0.png"
        |> createPlayer "Guest" "public/assets/icons/players/player_1.png"
        |> createPlayer "フクロウ" "public/assets/icons/players/player_2.png"
        |> createPlayer "姉さん" "public/assets/icons/players/player_3.png"
        |> List.reverse


initialGamesList : List Game
initialGamesList =
    []
        |> createGame 2 "三ノ国・赤き聖灰の王" "public/assets/icons/games/n_n_kn.png"
        |> createGame 387 "本末転倒ラボ・Soy-Con 01" "public/assets/icons/games/t_con.png"
        |> createGame 44 "パレオブレイド伝説" "public/assets/icons/games/xb_chr.png"
        |> createGame 1021 "Untitled Moose Game" "public/assets/icons/games/unt_gse.png"
        |> createGame 33 "Moria Odyssey" "public/assets/icons/games/t_con.png"
        |> createGame 77 "ばらまけ! ごはんの大盛" "public/assets/icons/games/t_con.png"
        |> G.sortById
        |> G.sortByLastPlayed


quickActionsList : List (QuickAction msg)
quickActionsList =
    [ QuickAction "News" IconIos.chatboxesOutline (rgba255 204 51 0 1.0) NoOp
    , QuickAction "eShop" Ionicon.bag (rgba255 204 204 0 1.0) NoOp
    , QuickAction "Album" Ionicon.image (rgb255 102 102 204) NoOp
    , QuickAction "Controllers" IconIos.gameControllerA (rgb255 175 175 175) NoOp
    , QuickAction "System Settings" IconIos.lightbulb (rgb255 175 175 175) ClickedThemeButton
    , QuickAction "Sleep Mode" Ionicon.power (rgb255 175 175 175) NoOp
    ]


playerItem : DisplayTheme Msg -> Player -> Element Msg
playerItem theme player =
    let
        playerIcon =
            P.icon player

        playerName =
            P.name player
    in
    Element.column
        [ centerX
        , centerY
        , spacingXY 0 10
        , Font.color <| rgba255 255 255 255 0
        , mouseOver [ Font.color <| rgb255 120 140 200 ]
        ]
        [ Input.button
            [ centerX
            , centerY
            , height <| px 64
            , width <| px 64
            , Border.rounded 90
            , inFront <|
                image
                    theme.playerItemStyle
                    { src = playerIcon, description = playerName }
            , Region.description playerName
            ]
            { onPress = Just <| SelectPlayer player
            , label = text ""
            }
        , el [ centerX ] <| text playerName
        ]


switchTopRow : Model -> ThemeColors -> DisplayTheme Msg -> Element Msg
switchTopRow model colors theme =
    Element.row
        [ width fill
        , height <| fillPortion 1
        ]
        [ Element.column [ Font.center ]
            [ Element.row
                [ spacingXY 10 0
                , height <| px 100
                ]
                (List.map (playerItem theme) playerList)
            ]
        , Element.column
            [ height <| fill
            , padding 10
            , alignRight
            ]
            [ row [ spacing 10 ]
                [ text <| renderTime model.timeZone model.currentTime
                , viewWifiIcon colors.mainColors.fontColor
                , viewBatteryIcon colors.mainColors.fontColor
                , text "48%"
                ]
            ]
        ]


gameItem : Model -> DisplayTheme Msg -> Game -> Element Msg
gameItem model theme game =
    let
        gameIcon =
            G.icon game

        gameTitle =
            G.title game

        gameIsSelected =
            case model.selectedGame of
                Just selectedGame ->
                    game == selectedGame

                Nothing ->
                    False

        playerIcon =
            case model.selectedPlayer of
                Just player ->
                    P.icon player

                Nothing ->
                    ""

        selectionIcon =
            if gameIsSelected then
                row
                    [ alignBottom
                    , centerX
                    , spacing 10
                    , height <| px 56
                    ]
                    [ column [ width <| px 42, height <| px 42 ]
                        [ image
                            [ width <| fill
                            , height <| fill
                            , Border.color <| rgb255 255 255 255
                            , Border.width 3
                            , Border.rounded 45
                            , clip
                            ]
                            { src = playerIcon, description = "" }
                        ]
                    , column
                        [ width <| fillPortion 80
                        , height <| px 42
                        , paddingXY 20 2
                        , Border.color <| rgb255 255 255 255
                        , Border.width 3
                        , Border.rounded 45
                        , Background.color <|
                            rgba255 0 0 0 0.6
                        ]
                        [ el
                            [ centerY
                            , paddingXY 20 0
                            , width <| fill
                            , Font.color <| rgb255 255 255 255
                            , Font.size 18
                            , Font.light
                            ]
                          <|
                            text "Playing"
                        ]
                    ]

            else
                none
    in
    column
        [ spacing 10
        , Font.color <| rgba255 255 255 255 0
        , mouseOver [ Font.color <| rgb255 120 140 200 ]
        ]
        [ el
            [ centerX
            ]
          <|
            text gameTitle
        , Input.button
            [ Region.description gameTitle
            , width <| px 254
            , height <| px 254
            , inFront <|
                image
                    ([ width fill
                     , height fill
                     ]
                        ++ theme.gameStyle
                    )
                    { src = gameIcon, description = gameTitle }
            , inFront selectionIcon
            ]
            { onPress = Just <| SelectGame game
            , label = text gameTitle
            }
        ]


switchGameRow : Model -> DisplayTheme Msg -> Element Msg
switchGameRow model theme =
    Element.row
        [ width fill
        , height <| fillPortion 4
        , spacing 15
        , padding 5
        , scrollbarY
        ]
    <|
        List.map (gameItem model theme) model.gamesList


quickActionItem : DisplayTheme Msg -> QuickAction Msg -> Element Msg
quickActionItem theme quickAction =
    column
        [ centerX
        , centerY
        , spacingXY 0 10
        , width <| px 80
        , Font.color <| rgba255 255 255 255 0
        , mouseOver
            [ Font.color <| rgb255 120 140 200
            ]
        ]
        [ Input.button
            ([ centerX
             , centerY
             , height <| px 72
             , width <| px 72
             , Border.rounded 90
             , inFront <|
                viewIcon quickAction.icon 48 quickAction.color
             , Region.description quickAction.title
             ]
                ++ theme.buttonColors
            )
            { onPress = Just quickAction.action, label = text "" }
        , el [ centerX ] <| text quickAction.title
        ]


switchQuickActionsRow : DisplayTheme Msg -> Element Msg
switchQuickActionsRow theme =
    Element.row
        [ width fill
        , spacingXY 20 0
        , height <| fillPortion 3
        ]
    <|
        List.map (quickActionItem theme) quickActionsList


switchBottomRow : Element msg
switchBottomRow =
    Element.row
        [ width fill
        , height <| fillPortion 2
        , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
        ]
        [ el [] <| text "Controller status"
        , el [ alignRight ] <| text "Allowed actions"
        ]


switchHomeScreen : Model -> ThemeColors -> DisplayTheme Msg -> Element Msg
switchHomeScreen model colors theme =
    Element.column
        [ height fill
        , width fill
        , padding 15
        , htmlAttribute (HtmlAttributes.style "user-select" "none")
        ]
        [ switchTopRow model colors theme
        , switchGameRow model theme
        , switchQuickActionsRow theme
        , switchBottomRow
        ]


mainView model =
    let
        colors =
            model |> setThemeColors

        theme =
            displayTheme colors
    in
    Element.layout ([ width fill, height fill ] ++ theme.mainColors)
        (switchHomeScreen model colors theme)



-- HELPERS


renderTime : Time.Zone -> Time.Posix -> String
renderTime zone time =
    let
        padZero string =
            if String.length string == 1 then
                "0" ++ string

            else
                string

        hours =
            Time.toHour zone time |> String.fromInt |> padZero

        minutes =
            Time.toMinute zone time |> String.fromInt |> padZero
    in
    hours ++ ":" ++ minutes


getMillis : Time.Zone -> Time.Posix -> Int
getMillis zone time =
    Time.toMillis zone time


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias Icon msg =
    Int -> RGBA -> Html msg


viewIcon : Icon Msg -> Int -> Color -> Element Msg
viewIcon icon size color =
    el [ centerX, centerY ] <| html <| icon size (toRgb color)


viewWifiIcon : Color -> Element Msg
viewWifiIcon color =
    viewIcon Ionicon.wifi 28 color

viewBatteryIcon : Color -> Element Msg
viewBatteryIcon color =
    viewIcon Ionicon.batteryCharging 28 color
