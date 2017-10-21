module Home exposing (..)

import Color exposing (rgba)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html
import Json.Decode
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font


type Styles
    = None
    | Search
    | Title
    | ProfileBadge
    | Menu
    | Elm
    | Navbar
    | Main
    | Backdrop
    | MenuContents
    | MenuControls
    | MenuItem
    | Caret
    | TitleFlix


colors =
    { reactiveGreen = Color.rgb 85 175 106
    , navbarBackground = Color.darkCharcoal
    }


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style ProfileBadge [ Color.text Color.white ]
        , style Title [ Font.weight 400, Font.size 32 ]
        , style TitleFlix [ Color.text colors.reactiveGreen ]
        , style MenuContents [ Color.background colors.navbarBackground ]
        , style MenuControls
            [ cursor "pointer"
            , Color.text Color.white
            , hover [ Color.text colors.reactiveGreen ]
            ]
        , style MenuItem
            [ cursor "pointer"
            , Color.text Color.white
            , hover
                [ Color.background colors.reactiveGreen
                , Color.text Color.white
                ]
            ]
        , style Elm
            [ Font.typeface [ Font.font "Corsiva", Font.font "Georgia", Font.font "serif" ]
            , Color.text Color.gray
            , Font.italic
            ]
        , style Navbar
            [ Color.text Color.white
            , Color.background colors.navbarBackground
            , Font.typeface [ Font.font "helvetica" ]
            , Font.size 24
            ]
        , style Caret [ Font.size 12 ]
        , style Menu
            [ cursor "pointer"
            , hover [ Color.text colors.reactiveGreen ]
            ]
        ]


main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


initialModel =
    { menu = Closed }


type alias Model =
    { menu : MenuState }


type MenuState
    = Open
    | Closed


type Msg
    = ToggleMenu
    | CloseMenu


update msg model =
    case msg of
        CloseMenu ->
            { model | menu = Closed }

        ToggleMenu ->
            { model
                | menu =
                    case model.menu of
                        Open ->
                            Closed

                        Closed ->
                            Open
            }


onClickPreventDefault msg =
    onWithOptions "click"
        { preventDefault = True, stopPropagation = True }
        (Json.Decode.succeed msg)


view model =
    Element.viewport stylesheet <|
        column Main
            [ onClick CloseMenu ]
            [ row Navbar
                [ width fill
                , paddingLeft 24
                , paddingRight 24
                , paddingTop 16
                , paddingBottom 16
                , verticalCenter
                ]
                [ row Title
                    [ alignLeft
                    , width (fillPortion 1)
                    ]
                    [ text "REACTIVE"
                    , el TitleFlix [] (text "FLIX")
                    ]
                , row Elm
                    [ width (fillPortion 1)
                    , center
                    ]
                    [ text "Built with Elm!" ]
                , row None
                    [ alignRight
                    , width (fillPortion 1)
                    ]
                    [ viewMenu model.menu ]
                ]
            , image Backdrop
                [ width fill ]
                { src = "https://wallpaperscraft.com/image/space_background_blue_dots_73340_2300x1300.jpg", caption = "backdrop" }
            ]


viewMenu menu =
    let
        menuControls =
            row Menu
                [ spacing 16, onClickPreventDefault ToggleMenu ]
                [ image ProfileBadge
                    [ width (px 32), height (px 32) ]
                    { src = "https://pbs.twimg.com/profile_images/635812303342956545/Fo4RyEgH_400x400.jpg", caption = "Richard Feldman" }
                , row MenuControls
                    [ spacing 12 ]
                    [ text "rtfeldman"
                    , viewCaret menu
                    ]
                ]
    in
    case menu of
        Closed ->
            menuControls

        Open ->
            menuControls
                |> below [ menuContents ]


menuContents =
    column MenuContents
        [ alignRight ]
        (List.map viewMenuItem [ "Account", "Messages", "Sign Out" ])


viewMenuItem caption =
    el MenuItem [ padding 10 ] (text caption)


viewCaret menu =
    let
        caretText =
            case menu of
                Closed ->
                    "▼"

                Open ->
                    "▲"
    in
    el Caret [ center, verticalCenter ] (text caretText)



--
--
-- navbar =
--     column Navbar
--         [ padding 20
--         , alignLeft
--         , width <| px 300
--         ]
--         [ el H3 [] (text "Channels") ]
--
--
-- inspector =
--     column Inspector
--         [ padding 20
--         , alignLeft
--         , width <| px 200
--         , height fill
--         ]
--         [ text "Inspector" ]
--
--
-- body =
--     column None
--         [ alignLeft
--         , width fill
--         ]
--         [ messages, messageBox ]
--
--
-- messages =
--     column
--         Chat
--         [ width fill
--         , alignLeft
--         , yScrollbar
--         ]
--         (List.map message <| List.range 1 100)
--
--
-- message n =
--     el None
--         [ padding 10 ]
--         (text <| "message" ++ toString n)
--
--
-- messageBox =
--     el MessageBox
--         [ height <| px 300
--         , width fill
--         , verticalCenter
--         ]
--         (text "Message box")
