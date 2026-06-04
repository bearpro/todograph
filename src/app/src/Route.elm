module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import UUID exposing (UUID)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = ProjectSelector
    | Project UUID


uuidParser : Parser (UUID -> a) a
uuidParser =
    Parser.custom "UUID" <|
        \str ->
            UUID.fromString str
                |> Result.toMaybe


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map ProjectSelector Parser.top
        , Parser.map Project (s "p" </> uuidParser)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


routeToString page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        ProjectSelector ->
            []

        Project id ->
            [ "p", UUID.toString id ]
