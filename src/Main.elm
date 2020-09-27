module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html.Attributes as HtmlAttributes
import Task
import Time


type alias Model =
    { displayMode : DisplayMode
    , selectedGame : Maybe Game
    , selectedPlayer : Maybe Player
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    }


initialModel : Model
initialModel =
    { displayMode = Day
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


displayMode : DisplayMode
displayMode =
    Night


type alias DisplayTheme msg =
    { mainColors : List (Attribute msg)
    , buttonColors : List (Attribute msg)
    , playerItemStyle : List (Attribute msg)
    , gameStyle : List (Attribute msg)
    }


displayTheme : Model -> DisplayTheme msg
displayTheme model =
    let
        actualMillis =
            getMillis model.timeZone model.currentTime

        hoverBorderAlpha =
            if actualMillis <= 300 then
                1.0 - (toFloat actualMillis / 1000)

            else if actualMillis >= 800 then
                1.0

            else
                toFloat actualMillis / 1000
    in
    case model.displayMode of
        Day ->
            { mainColors =
                [ Background.color <| rgb255 235 235 235
                , Font.color <| rgb255 41 41 41
                ]
            , buttonColors =
                [ Background.color <| rgb255 255 255 255
                , Border.shadow
                    { blur = 0
                    , color = rgba255 218 218 218 1.0
                    , offset = ( 0, 0 )
                    , size = 0.8
                    }
                , mouseOver
                    [ Border.shadow
                        { blur = 0
                        , color = rgba255 46 215 200 hoverBorderAlpha
                        , offset = ( 0, 0 )
                        , size = 5
                        }
                    ]
                ]
            , playerItemStyle =
                [ width <| fill
                , height <| fill
                , Border.color <| rgb255 255 255 255
                , Border.width 4
                , Border.rounded 90
                , clip
                , Background.color <| rgb255 255 255 255
                , Border.shadow
                    { blur = 0
                    , color = rgba255 218 218 218 1.0
                    , offset = ( 0, 0 )
                    , size = 0.8
                    }
                , mouseOver
                    [ Border.shadow
                        { blur = 0
                        , color = rgba255 46 215 200 hoverBorderAlpha
                        , offset = ( 0, 0 )
                        , size = 5
                        }
                    ]
                ]
            , gameStyle =
                [ mouseOver
                    [ Border.shadow
                        { blur = 0
                        , color = rgba255 46 215 200 hoverBorderAlpha
                        , offset = ( 0, 0 )
                        , size = 4
                        }
                    ]
                ]
            }

        Night ->
            { mainColors =
                [ Font.color <| rgb255 255 255 255
                , Background.color <| rgb255 44 44 44
                ]
            , buttonColors =
                [ Background.color <| rgb255 80 80 80
                , Border.shadow
                    { blur = 3
                    , color = rgb255 40 40 40
                    , offset = ( 0, 0 )
                    , size = 0.8
                    }
                , mouseOver
                    [ Border.shadow
                        { blur = 0
                        , color = rgba255 255 255 255 hoverBorderAlpha
                        , offset = ( 0, 0 )
                        , size = 5
                        }
                    ]
                ]
            , playerItemStyle =
                [ width <| fill
                , height <| fill
                , Border.color <| rgb255 88 77 75
                , Border.width 3
                , Border.rounded 45
                , clip
                , Background.color <| rgb255 255 255 255
                , Border.shadow
                    { blur = 0
                    , color = rgba255 218 218 218 1.0
                    , offset = ( 0, 0 )
                    , size = 0.8
                    }
                , mouseOver
                    [ Border.shadow
                        { blur = 0
                        , color = rgba255 46 215 200 hoverBorderAlpha
                        , offset = ( 0, 0 )
                        , size = 5
                        }
                    ]
                ]
            , gameStyle =
                [ mouseOver
                    [ Border.shadow
                        { blur = 0
                        , color = rgba255 255 255 255 hoverBorderAlpha
                        , offset = ( 0, 0 )
                        , size = 5
                        }
                    ]
                ]
            }


type alias Player =
    { id : Int
    , name : String
    , icon : String
    }


type alias Game =
    { id : Int
    , title : String
    , icon : String
    }


type alias QuickAction =
    { title : String
    , icon : String
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
                ( { model | selectedGame = Just game }, Cmd.none )

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
    [ Player 0 "mats" "public/assets/icons/players/player_0.png"
    , Player 1 "Guest" "public/assets/icons/players/player_1.png"
    , Player 2 "フクロウ" "public/assets/icons/players/player_2.png"
    , Player 3 "姉さん" "public/assets/icons/players/player_3.png"
    ]


gamesList : List Game
gamesList =
    [ Game 2 "三ノ国・赤き聖灰の王" "public/assets/icons/games/n_n_kn.png"
    , Game 387 "本末転倒ラボ・Soy-Con 01" "public/assets/icons/games/t_con.png"
    , Game 44 "パレオブレイド伝説" "public/assets/icons/games/xb_chr.png"
    , Game 1021 "Untitled Moose Game" "public/assets/icons/games/unt_gse.png"
    , Game 33 "Moria Odyssey" "public/assets/icons/games/t_con.png"
    , Game 77 "ばらまけ! ごはんの大盛" "public/assets/icons/games/t_con.png"
    ]


quickActionsList : List QuickAction
quickActionsList =
    [ QuickAction "News" "" NoOp
    , QuickAction "eShop" "" NoOp
    , QuickAction "Album" "" NoOp
    , QuickAction "Controllers" "" NoOp
    , QuickAction "System Settings" "" ClickedThemeButton
    , QuickAction "Sleep Mode" "" NoOp
    ]


playerItem : DisplayTheme Msg -> Player -> Element Msg
playerItem theme player =
    Element.column
        [ spacingXY 0 10
        , width <| px 64
        , Font.color <| rgba255 255 255 255 0
        , mouseOver [ Font.color <| rgb255 120 140 200 ]
        , Events.onClick <| SelectPlayer player
        ]
        [ row [ centerX, centerY ]
            [ image
                theme.playerItemStyle
                { src = player.icon, description = "" }
            ]
        , el [ centerX ] <| text player.name
        ]


switchTopRow : Model -> DisplayTheme Msg -> Element Msg
switchTopRow model theme =
    Element.row
        [ width fill
        , height <| fillPortion 1
        ]
        [ Element.column [ Font.center ]
            [ Element.row [ spacingXY 10 0 ]
                (List.map (playerItem theme) playerList)
            ]
        , Element.column
            [ height <| fill
            , padding 10
            , alignRight
            ]
            [ row [ spacing 10 ]
                [ text <| renderTime model.timeZone model.currentTime
                , text "Wifi"
                , text "48%"
                ]
            ]
        ]


gameItem : Model -> DisplayTheme Msg -> Game -> Element Msg
gameItem model theme game =
    let
        gameIsSelected =
            case model.selectedGame of
                Just selectedGame ->
                    game == selectedGame

                Nothing ->
                    False

        playerIcon =
            case model.selectedPlayer of
                Just player ->
                    player.icon

                Nothing ->
                    ""

        selectionIcon =
            if gameIsSelected then
                inFront
                    (row
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
                    )

            else
                inFront none
    in
    column
        [ spacing 10
        , Font.color <| rgba255 255 255 255 0
        , mouseOver [ Font.color <| rgb255 120 140 200 ]
        , Events.onClick <| SelectGame game
        ]
        [ el
            [ centerX
            ]
          <|
            text game.title
        , el
            [ width <| px 254
            , height <| px 254
            ]
          <|
            image
                ([ width fill
                 , height fill
                 , selectionIcon
                 ]
                    ++ theme.gameStyle
                )
                { src = game.icon, description = game.title }
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
        List.map (gameItem model theme) gamesList


quickActionItem : DisplayTheme Msg -> QuickAction -> Element Msg
quickActionItem theme quickAction =
    column
        [ centerX
        , centerY
        , spacingXY 0 10
        , width <| px 80
        , Font.color <| rgba255 255 255 255 0
        , mouseOver [ Font.color <| rgb255 120 140 200 ]
        ]
        [ el
            ([ centerX
             , centerY
             , height <| px 72
             , width <| px 72
             , Border.rounded 90
             , Events.onClick quickAction.action
             ]
                ++ theme.buttonColors
            )
          <|
            el
                [ centerX
                , centerY
                ]
            <|
                text "[X]"
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


switchHomeScreen : Model -> DisplayTheme Msg -> Element Msg
switchHomeScreen model theme =
    Element.column
        [ height fill
        , width fill
        , padding 15
        , htmlAttribute (HtmlAttributes.style "user-select" "none")
        ]
        [ switchTopRow model theme
        , switchGameRow model theme
        , switchQuickActionsRow theme
        , switchBottomRow
        ]


mainView model =
    let
        displaySettings =
            model |> displayTheme
    in
    Element.layout ([ width fill, height fill ] ++ displaySettings.mainColors)
        (switchHomeScreen model displaySettings)



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
