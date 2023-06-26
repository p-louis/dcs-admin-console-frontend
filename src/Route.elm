module Route exposing (..)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes
import String exposing (fromInt)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, oneOf, s)


type Route
    = Login
    | Logout
    | Reload
    | Home
    | Root
    | Upload
    | Tacview 


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Reload (s "reload")
        , Parser.map Logout (s "logout")
        , Parser.map Upload (s "upload")
        , Parser.map Tacview (s "tacview")
        ]


string =
    Parser.custom "STRING" Url.percentDecode


href : Route -> Attribute msg
href route =
    Html.Attributes.href (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


routeToString : Route -> String
routeToString route =
    "#/" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        Root ->
            []

        Reload ->
            ["reload"]

        Home ->
            [ "" ]

        Login ->
            [ "login" ]

        Logout ->
            [ "logout" ]

        Upload ->
            [ "upload" ]

        Tacview ->
            [ "tacview" ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)
