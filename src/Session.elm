module Session exposing (..)

import Api
import Api.Endpoint as Endpoint
import Browser.Navigation as Nav
import Http exposing (jsonBody)
import Json.Encode as Encode exposing (Value, object)
import User exposing (User, tokenDecoder, userEncoder)


type Session
    = Guest Nav.Key
    | LoggedIn Nav.Key User


type alias Credentials a =
    { a
        | username : String
        , password : String
    }


user : Session -> Maybe User
user session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn _ userVal ->
            Just userVal


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest navKeyVal ->
            navKeyVal

        LoggedIn navKeyVal _ ->
            navKeyVal


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.userChanges (\maybeUser -> toMsg (fromUser key maybeUser)) tokenDecoder


fromUser : Nav.Key -> Maybe User -> Session
fromUser key maybeUser =
    case maybeUser of
        Just userVal ->
            LoggedIn key userVal

        Nothing ->
            Guest key


login : Credentials a -> (Result Http.Error User -> msg) -> Cmd msg
login loginBody msg =
    Api.post Endpoint.login msg (jsonBody (encodeLoginBody loginBody)) tokenDecoder


logout : (Result Http.Error User -> msg) -> Cmd msg
logout msg =
    Api.delete Endpoint.login msg Http.emptyBody tokenDecoder


encodeLoginBody : Credentials a -> Value
encodeLoginBody loginBody =
    object
        [ ( "username", Encode.string loginBody.username )
        , ( "password", Encode.string loginBody.password )
        ]


store : User -> Cmd msg
store userVal =
    Api.storeUserWith userVal userEncoder
