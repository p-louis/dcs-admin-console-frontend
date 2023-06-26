module Pages.Blank exposing (view)

import Html exposing (Html, div)


view : { title : String, body : List (Html msg) }
view =
    { title = "Blank"
    , body = [ div [] [] ]
    }
