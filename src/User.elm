module User exposing (User, tokenDecoder, userEncoder)

import Json.Decode as Decode exposing (Decoder, field, map)
import Json.Encode as Encode exposing (Value)


type alias User =
    { token : String
    }

tokenDecoder : Decoder User
tokenDecoder =
    map User
        (field "token" Decode.string)

userEncoder : User -> Value
userEncoder userVal =
    Encode.object
        [ ( "token", Encode.string userVal.token )
        ]
