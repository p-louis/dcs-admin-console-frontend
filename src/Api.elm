port module Api exposing (..)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Browser
import Browser.Navigation as Nav
import Http exposing (Body, Error(..), Header(..))
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Url exposing (Url)
import User exposing (User)
import Util



-- Ports
--| onStoreChange will be triggered
--| after a change has occurred to
--| This applications' localstorage.


port onStoreChange : (Value -> msg) -> Sub msg



--| storeCache can be used to store a
--| json-value in the localstorage.

port storeCache : Maybe Value -> Cmd msg


userChanges : (Maybe user -> msg) -> Decoder user -> Sub msg
userChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder user -> Value -> Maybe user
decodeFromChange userDecoder val =
    Decode.decodeValue userDecoder val
        |> Result.toMaybe


storeUserWith : user -> (user -> Value) -> Cmd msg
storeUserWith user encoder =
    let
        json =
            encoder user
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing



--| Can convert all Http-Errors to a string,
--| but is indiscriminate of eg. Status-Codes.


toString : Http.Error -> String
toString err =
    case err of
        Timeout ->
            "Timed out"

        NetworkError ->
            "Network Error"

        BadStatus resp ->
            "Bad Status: " ++ String.fromInt resp

        BadBody text ->
            "Unexpected response: " ++ text

        BadUrl url ->
            "Bad url: " ++ url


encodeFormType : String -> Http.Body
encodeFormType formType =
    Http.jsonBody
        <| Encode.object
            [ ( "formType", Encode.string formType )
            ]

-- Http-call abstractions


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get url toMsg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }


getSecure : User -> Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
getSecure user url toMsg decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers =
          [ Http.header "Authorization" ("Bearer " ++ user.token)
          ]
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }

put : Endpoint -> (Result Http.Error a -> msg) -> Body -> Decoder a -> Cmd msg
put url toMsg body decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }


post : Endpoint -> (Result Http.Error a -> msg) -> Body -> Decoder a -> Cmd msg
post url toMsg body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }

postSecure : User -> Endpoint -> (Result Http.Error a -> msg) -> Body -> Decoder a -> Cmd msg
postSecure user url toMsg body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers =
          [ Http.header "Authorization" ("Bearer " ++ user.token)
          ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }

postWithErrorBody : Endpoint -> (Result Http.Error a -> msg) -> Body -> Decoder a -> Cmd msg
postWithErrorBody url toMsg body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Util.expectJson toMsg decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }

postSecureWithErrorBody : User -> Endpoint -> (Result Http.Error a -> msg) -> Body -> Decoder a -> Cmd msg
postSecureWithErrorBody user url toMsg body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Util.expectJson toMsg decoder
        , headers =
          [ Http.header "Authorization" ("Bearer " ++ user.token)
          ]
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }

delete : Endpoint -> (Result Http.Error a -> msg) -> Body -> Decoder a -> Cmd msg
delete url toMsg body decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson toMsg decoder
        , headers = []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        , withCredentials = False
        }



--| The application function allows us to
--| 'inject' a decoder for the provided
--| flags to enable passing of complex
--| data structures as flags.


application :
    Decoder user
    ->
        { init : Maybe user -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application userDecoder config =
    let
        init flags url navKey =
            let
                maybeUser =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString userDecoder)
                        |> Result.toMaybe
            in
            config.init maybeUser url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }
