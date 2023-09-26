module Api.Endpoint exposing (..)

import Http
import String exposing (fromInt)
import Url.Builder exposing (QueryParameter)



-- Types


type alias HttpRequest a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , tracker : Maybe String
    }


type Endpoint
    = Endpoint String


-- Functions


{-| Http.request, except it takes an Endpoint instead of a Url.
It can also be provided with a flag that changes the request to a riskyRequest.
-}
request :
    { body : Http.Body
    , expect : Http.Expect a
    , headers : List Http.Header
    , method : String
    , timeout : Maybe Float
    , url : Endpoint
    , tracker : Maybe String
    , withCredentials : Bool
    }
    -> Cmd a
request config =
    case config.withCredentials of
        True ->
            Http.riskyRequest
                { body = config.body
                , expect = config.expect
                , headers = config.headers
                , method = config.method
                , timeout = config.timeout
                , url = unwrap config.url
                , tracker = config.tracker
                }

        False ->
            Http.request
                { body = config.body
                , expect = config.expect
                , headers = config.headers
                , method = config.method
                , timeout = config.timeout
                , url = unwrap config.url
                , tracker = config.tracker
                }


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str

url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.relative
        ("api" :: paths)
        queryParams
        |> Endpoint

login : Endpoint
login =
    url [ "login" ] []


user : Endpoint
user =
    url [ "admin", "user" ] []

upload : Endpoint
upload =
    url [ "admin", "upload" ] []

mission : Endpoint
mission =
    url [ "admin", "mission" ] []

currentMission : Endpoint
currentMission =
    url [ "admin", "mission", "current" ] []

pause : Endpoint
pause =
    url [ "admin", "pause" ] []

unpause : Endpoint
unpause =
    url [ "admin", "unpause" ] []

tacview : Endpoint
tacview =
    url [ "admin", "tacview" ] []

dcs : Endpoint
dcs =
    url [ "admin", "dcs" ] []

srs : Endpoint
srs =
    url [ "admin", "srs" ] []
