module Pages.NotFound exposing (..)

import Html exposing (Html, div, h1, text)


view : { title : String, body : List (Html msg) }
view =
    { title = "Not Found"
    , body =
        [ div []
            [ h1 [] [ text "Not found" ]
            ]
        ]
    }
