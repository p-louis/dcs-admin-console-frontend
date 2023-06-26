module Environment exposing (Environment, getEnvironment)


import Api
import Api.Endpoint exposing (environment)
import Http
import Json.Decode exposing (Decoder, field, map, map2, string)
type alias Environment =
    { homePageUrl : String
    , bannerBackgroundColor : String
    }


environmentDecoder : Decoder Environment
environmentDecoder =
    map2 Environment
        (field "homePageUrl" string)
        (field "bannerBackgroundColor" string)


getEnvironment :  (Result Http.Error Environment -> msg) -> Cmd msg
getEnvironment msg =
    Api.get environment msg environmentDecoder