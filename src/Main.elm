module Main exposing (..)

import Api
import Api.Endpoint as Endpoint
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html)
import Http
import Pages.Blank as Blank
import Pages.LoggedOut as LoggedOut
import Pages.Login as Login
import Pages.NotFound as NotFound
import Pages.Upload as Upload
import Route exposing (Route)
import Session exposing (Session(..))
import Json.Decode as Decode
import Url
import User exposing (User, tokenDecoder)
import Util exposing (updateWith)


type Model
    = NotFound Session
    | Logout Session
    | Redirect Session
    | Login Login.Model
    | Upload Upload.Model


type Msg
    = NoOp
    | ChangeUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | GotSession Session
    | GotLoginMsg Login.LoginMsg
    | GotUploadMsg Upload.UploadMsg
    | GotUserResult (Result Http.Error ())

-- INIT
init : Maybe User -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeUser url nav =
    let
      (mdl, msg) = changeRouteTo (Route.fromUrl url)
          (Redirect (Session.fromUser nav maybeUser))
      userMsg = Maybe.withDefault Cmd.none <|
        Maybe.map (\user -> Api.getSecure user Endpoint.user GotUserResult (Decode.succeed ())) (Session.user (toSession mdl))
    in
    ( mdl,
    Cmd.batch
      [ msg
      , userMsg
      ]
    )

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
      session =
          toSession model
  in
  case ( msg, model ) of
      ( ClickedLink urlRequest, _ ) ->
          case urlRequest of
              Browser.Internal url ->
                  case url.fragment of
                      Nothing ->
                          ( model, Cmd.none )

                      Just _ ->
                          ( model
                          , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                          )

              Browser.External href ->
                  ( model
                  , Nav.load href
                  )

      ( ChangeUrl url, _ ) ->
          changeRouteTo (Route.fromUrl url) model


      ( GotLoginMsg subMsg, Login loginMdl ) ->
            Login.update subMsg loginMdl
                |> updateWith Login GotLoginMsg

      ( GotUploadMsg subMsg, Upload uploadMdl ) ->
            Upload.update subMsg uploadMdl
                |> updateWith Upload GotUploadMsg

      ( GotUserResult result, _ ) ->
        case result of
          Err _ ->
            let
              (lmdl, _) = (Login.init <| Session.fromUser (Session.navKey session) Nothing)
            in
            changeRouteTo (Just Route.Login) (Login lmdl)

          Ok _ ->
            (model, Cmd.none)


      ( _, _ ) ->
          ( Debug.log "Model " model, Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
      NotFound session ->
        session

      Redirect session ->
        session

      Login loginMdl ->
        loginMdl.session

      Logout session ->
        session

      Upload uploadModel ->
        uploadModel.session



changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case ( route, session ) of
        ( Just Route.Reload, _ ) ->
            ( model, Nav.load "" )

        ( Just Route.Root, LoggedIn _ _ ) ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Upload )

        ( Just Route.Home, LoggedIn _ _ ) ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Upload )

        ( Just Route.Login, _ ) ->
            let
                ( loginMdl, loginCmd ) =
                    Login.init session
            in
            ( Login loginMdl, Cmd.map GotLoginMsg loginCmd )

        ( Just Route.Upload, LoggedIn _ _ ) ->
            let
                ( uploadMdl, uploadMsg ) =
                    Upload.init session
            in
            ( Upload uploadMdl, Cmd.map GotUploadMsg uploadMsg )

        ( Just Route.Logout, _ ) ->
            ( Logout (Session.Guest <| Session.navKey session)
            , Cmd.batch
                [ Api.logout
                , Session.logout (\_ -> NoOp)
                ]
            )

        ( Nothing, _ ) ->
            ( NotFound session, Cmd.none )

        ( _, _ ) ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Login )



-- SUBSCRIPTIONS
{-| Subscriptions are a representation of side-effects occurring outside
of the Elm-Application. This includes eg Time and Randomness, but
can also cover access to local-storage or the use of websockets.
|
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
      NotFound _ ->
        Sub.none

      Redirect _ ->
        Session.changes GotSession (Session.navKey (toSession model))

      Logout _ ->
        Sub.none

      Login loginMdl ->
        Sub.map GotLoginMsg (Login.subscriptions loginMdl)

      Upload uploadMdl ->
        Sub.map GotUploadMsg (Upload.subscriptions uploadMdl)


-- VIEW
view : Model -> Document Msg
view model =
    case model of
      NotFound _ ->
        NotFound.view

      Logout session ->
        LoggedOut.view session

      Login mdl ->
        viewPage Login.view mdl GotLoginMsg

      Redirect session ->
        Blank.view

      Upload mdl ->
        viewPage Upload.view mdl GotUploadMsg



viewPage : (mdl -> { title: String, body: List (Html msg) }) -> mdl -> (msg -> Msg) -> Document Msg
viewPage pageView mdl toMsg =
  let
    result = pageView mdl
  in
  { title = result.title
  , body = List.map (Html.map toMsg) result.body
  }

-- MAIN


main =
    Api.application tokenDecoder
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = ChangeUrl
        , onUrlRequest = ClickedLink
        }
