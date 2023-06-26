module Pages.LoggedOut exposing (..)

import Html exposing (Html, a, div, h2, li, text, ul)
import Html.Attributes exposing (class, href, id)
import Route
import Session exposing (Session)

view : Session -> { title : String, body : List (Html msg) }
view session =
    { title = "Logged out"
    , body =
        [ div [ id "gate" ] [
            div [ class "dialog" ]
                [ h2 [] [ text "Logged out" ]
                , ul []
                    [ li [] [ text "You've been logged out of the application." ]
                    , li [] [ text "You may still be logged in to your single sign-on identity provider." ]
                    ]
                , div [ class "dialog-buttons" ] [
                    a [ class "button"
                      , href (Route.routeToString Route.Reload)
                      ]
                      [ text "Back to Login" ]
                    ]
                ]
          ]
        ]
    }
