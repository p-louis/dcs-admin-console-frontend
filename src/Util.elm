module Util exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http exposing (Expect, expectStringResponse)
import Json.Decode as Decode exposing (Decoder, errorToString)
import String exposing (fromInt)

type Either a b
    = Left a
    | Right b

compose : (b -> c) -> (a -> b) -> (a -> c)
compose g f = \x -> g(f(x))

compose2 : (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 = compose compose compose

flip : (a -> b -> c) -> (b -> a -> c)
flip f = \a b -> f b a

orElse : Maybe a -> Maybe a -> Maybe a
orElse sec prim =
    case prim of
        Nothing -> sec
        Just _ -> prim

updateWith : (subModel -> model) -> (subMsg -> msg) -> ( subModel, Cmd subMsg ) -> ( model, Cmd msg )
updateWith toModel toMsg ( subModel, subMsg ) =
    ( toModel subModel
    , Cmd.map toMsg subMsg
    )

viewLoading : Html msg
viewLoading =
    div [ class "loader" ]
        [ div [ ] [ text "Loading"]
        , div [ class "lds-facebook" ]
            [ div [] []
            , div [] []
            , div [] []
            ]
        ]


viewLoadingWithMsg : String -> Html msg
viewLoadingWithMsg message =
    div [ class "loader" ]
        [ div [ ] [ text message ]
        , div [ class "lds-facebook" ]
            [ div [] []
            , div [] []
            , div [] []
            ]
        ]

type alias Content a =
    { title : String
    , content : Html a
    }


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xl yl =
    case ( xl, yl ) of
        ( _, [] ) ->
            []

        ( [], _ ) ->
            []

        ( x :: xs, y :: ys ) ->
            f x y :: zipWith f xs ys


zip : List a -> List b -> List ( a, b )
zip =
    zipWith (\a b -> ( a, b ))


fst : ( a, b ) -> a
fst ( x, _ ) =
    x

snd : ( a, b ) -> b
snd ( _, y ) =
    y

fstt : ( a, b, c ) -> a
fstt ( x, _, _ ) =
    x
sndt : ( a, b, c ) -> b
sndt ( _, y, _ ) =
    y

thrd : ( a, b, c ) -> c
thrd ( _, _, y ) =
    y

-- Field Selectors
id : { a | id : b } -> b
id field = field.id

expectJson : (Result Http.Error a -> msg) -> Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadBody ("BadStatus " ++ fromInt metadata.statusCode ++ ": " ++ body))

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value
                        Err err ->
                            Err (Http.BadBody (errorToString err))

errToString : Http.Error -> String
errToString err =
    case err of
        Http.BadUrl url ->
            url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus int ->
            "BadStatus " ++ fromInt int

        Http.BadBody error ->
            error
