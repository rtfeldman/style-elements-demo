module Home exposing (..)

import Color exposing (rgba)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Html exposing (Html)
import Json.Decode
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Window


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
    | GreenEmphasis


colors =
    { reactiveGreen = Color.rgb 85 175 106
    , navbarBackground = Color.darkCharcoal
    , mainBackground = Color.black
    }


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Main [ Color.background colors.mainBackground ]
        , style ProfileBadge [ Color.text Color.white ]
        , style Title [ Font.weight 400, Font.size 32 ]
        , style GreenEmphasis [ Color.text colors.reactiveGreen ]
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


main : Program { height : Int, width : Int } Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Window.resizes Resize


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init flags =
    ( { menu = Closed, viewport = flags }, Cmd.none )


type alias Model =
    { menu : MenuState
    , viewport : Window.Size
    }


type MenuState
    = Open
    | Closed


type Msg
    = ToggleMenu
    | CloseMenu
    | Resize Window.Size


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        CloseMenu ->
            ( { model | menu = Closed }, Cmd.none )

        ToggleMenu ->
            ( { model
                | menu =
                    case model.menu of
                        Open ->
                            Closed

                        Closed ->
                            Open
              }
            , Cmd.none
            )

        Resize viewport ->
            ( { model | viewport = viewport }, Cmd.none )


onClickPreventDefault : msg -> Element.Attribute variation msg
onClickPreventDefault msg =
    onWithOptions "click"
        { preventDefault = True, stopPropagation = True }
        (Json.Decode.succeed msg)


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        column Main
            [ onClick CloseMenu, height fill ]
            [ viewNavbar model.viewport.width model.menu
            , row None [ height fill, padding 20 ] [ backgroundImage ]
                |> within
                    [ reactiveConfLogo [] ]
            ]


reactiveConfLogo : List (Element.Attribute variation msg) -> Element Styles variation msg
reactiveConfLogo attrs =
    image None attrs { src = "reactive-logo.png", caption = "ReactiveConf" }


backgroundImage : Element Styles variation msg
backgroundImage =
    image None [ width (percent 100) ] { src = "bratislava.jpg", caption = "Bratislava" }


pageLogo : Element Styles variation msg
pageLogo =
    row Title
        [ alignLeft
        , width (fillPortion 1)
        ]
        [ text "CSS"
        , el GreenEmphasis [] (text " as bytecode")
        ]


builtWithElm : Element Styles variation msg
builtWithElm =
    row Elm
        [ width (fillPortion 1)
        , center
        ]
        [ text "Built with Elm!" ]


viewNavbar : Int -> MenuState -> Element Styles variation Msg
viewNavbar viewportWidth menu =
    let
        menuControls alignment =
            [ row None
                [ alignment
                , width (fillPortion 1)
                ]
                [ viewMenu menu ]
            ]

        contents =
            if viewportWidth < 640 then
                menuControls center
            else if viewportWidth < 1024 then
                List.append [ pageLogo ] (menuControls alignRight)
            else
                List.append [ pageLogo, builtWithElm ] (menuControls alignRight)
    in
    row Navbar
        [ width fill
        , paddingLeft 24
        , paddingRight 24
        , paddingTop 16
        , paddingBottom 16
        , verticalCenter
        ]
        contents


viewMenu : MenuState -> Element Styles variation Msg
viewMenu menu =
    let
        menuControls =
            row Menu
                [ spacing 16, onClickPreventDefault ToggleMenu ]
                [ image ProfileBadge
                    [ width (px 32), height (px 32) ]
                    { src = "profile.jpg", caption = "Richard Feldman" }
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


menuContents : Element Styles variation msg
menuContents =
    column MenuContents
        [ alignRight ]
        (List.map viewMenuItem [ "Account", "Messages", "Sign Out" ])


viewMenuItem : String -> Element Styles variation msg
viewMenuItem caption =
    el MenuItem [ padding 16 ] (text caption)


viewCaret : MenuState -> Element Styles variation msg
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
