module Pages.Login exposing (Model, LoginMsg(..), init, subscriptions, update, view)

import Html exposing (Html, button, div, fieldset, form, input, legend, text)
import Html.Attributes exposing (autocomplete, autofocus, class, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Route
import Session exposing (Session(..))
import User exposing (User)



-- Types


type alias Model =
    { username : String
    , password : String
    , session : Session
    }


type LoginMsg
    = UpdateUsername String
    | UpdatePassword String
    | LoginSubmitted
    | CompletedLogin (Result Http.Error User)
    | GotSession Session



-- Init


init : Session -> ( Model, Cmd LoginMsg )
init session =
    let
        model =
            { username = ""
            , password = ""
            , session = session
            }
    in
    case session of
        Guest _ ->
            ( model, Cmd.none )

        LoggedIn _ _ ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )



-- Update


update : LoginMsg -> Model -> ( Model, Cmd LoginMsg )
update msg model =
    case msg of
        UpdateUsername username ->
            ( { model | username = username }
            , Cmd.none
            )

        UpdatePassword password ->
            ( { model | password = password }
            , Cmd.none
            )

        LoginSubmitted ->
            ( model, Session.login model CompletedLogin )

        CompletedLogin result ->
            case result of
                Err e ->
                  let
                    er = Debug.log "Error " e
                  in
                  ( model, Cmd.none )

                Ok token ->
                    ( model
                    , Session.store <| Debug.log "Token value " token
                    )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )



-- Subscriptions


subscriptions : Model -> Sub LoginMsg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- View


view : Model -> { title : String, body : List (Html LoginMsg) }
view model =
    { title = "Login"
    , body =
        [ div [ id "gate" ] [
            div [ id "login" ]
                [ viewForm model
                ]
          ]
        ]
    }


viewForm : Model -> Html LoginMsg
viewForm { username, password } =
    form
        [ class "dialog", onSubmit LoginSubmitted ]
        [ fieldset []
            [ legend [] [ text "Login" ]
            , div [ class "form-field-input-container" ]
                [ input
                    [ id "login"
                    , placeholder "Username"
                    , name "login"
                    , type_ "text"
                    , value username
                    , onInput UpdateUsername
                    , autofocus True
                    , autocomplete False
                    ]
                    []
                ]
            , div [ class "form-field-input-container" ]
                [ input
                    [ id "password"
                    , placeholder "Password"
                    , name "password"
                    , type_ "password"
                    , value password
                    , onInput UpdatePassword
                    , autocomplete False
                    ]
                    []
                ]
            , div [ class "dialog-buttons" ] [
                button
                    [ class "button"
                    , value "Login"
                    , onClick LoginSubmitted
                    ]
                    [ text "Login" ]
                ]
            ]
        ]
