module Pages.Upload exposing (..)
import Api
import Api.Endpoint as Endpoint exposing (Endpoint(..))
import File exposing (File)
import File.Select as Select exposing (file)
import Html exposing (Html, a, button, div, h3, label, span, text)
import Html.Attributes exposing (class, for, href, id, style, target)
import Html.Events exposing (onClick)
import Http exposing (filePart, multipartBody)
import Json.Decode as Decode exposing (Decoder)
import Session exposing (Session(..))
import User exposing (User)
import Util exposing (errToString, viewLoadingWithMsg)



-- Types
type alias FileName =
  { filename : String
  }

filenameDecoder : Decoder FileName
filenameDecoder =
    Decode.map FileName
        (Decode.field "filename" Decode.string)

type Status t
  = Loading
  | Loaded t
  | LoadError String

type Selection
  = None
  | Selected File
  | Uploading File
  | Error String
  | Success

type alias Model =
  { session: Session
  , file: Selection
  , missions: Status (List FileName)
  , tacViews: Status (List FileName)
  , currenMission: Status FileName
  }



type UploadMsg
    = UpdateFile File
    | ClickedSelectFile
    | ClickedUpload File
    | GotUploadResult (Result Http.Error ())
    | GotMissionResult (Result Http.Error (List FileName))
    | GotCurrentMissionResult (Result Http.Error FileName)
    | GotTacViewResult (Result Http.Error (List FileName))



-- Init


init : Session -> ( Model, Cmd UploadMsg )
init session =
  ({ session = session
  , file = None
  , missions = Loading
  , tacViews = Loading
  , currenMission = Loading
  }
  , Cmd.batch
    [ refreshMissions session
    , refreshTacViews session
    , refreshMission session
    ]
  )

refreshMissions : Session -> Cmd UploadMsg
refreshMissions session =
  Api.getSecure (sessUser session) Endpoint.mission GotMissionResult (Decode.list filenameDecoder)

refreshMission : Session -> Cmd UploadMsg
refreshMission session =
  Api.getSecure (sessUser session) Endpoint.currentMission GotCurrentMissionResult filenameDecoder

refreshTacViews : Session -> Cmd UploadMsg
refreshTacViews session =
  Api.getSecure (sessUser session) Endpoint.tacview GotTacViewResult (Decode.list filenameDecoder)
-- Update
sessionUser : Model -> User
sessionUser model =
  case model.session of
    Guest _ -> { token = ""}
    LoggedIn _ user -> user

sessUser : Session -> User
sessUser session =
  case session of
    Guest _ -> { token = ""}
    LoggedIn _ user -> user

update : UploadMsg -> Model -> ( Model, Cmd UploadMsg )
update msg model =
    case msg of
      UpdateFile file ->
        ( { model | file = Selected file }, Cmd.none)

      ClickedSelectFile ->
        ( model, Select.file [] UpdateFile )

      ClickedUpload file ->
        ( model
        , Api.postSecureWithErrorBody (sessionUser model) Endpoint.upload GotUploadResult
          (multipartBody
              [ filePart "file" file
              ]
          ) (Decode.succeed ())
        )

      GotUploadResult result ->
        case result of
          Ok _ ->
            ( { model | file = Success } , refreshMissions model.session)

          Err err -> ( { model | file = Error (errToString err) }, Cmd.none)

      GotMissionResult result ->
        case result of
          Ok list ->
            ( { model | missions = Loaded list }
            , Cmd.none
            )

          Err err -> ( { model | missions = LoadError (errToString err) }, Cmd.none)

      GotTacViewResult result ->
        case result of
          Ok list ->
            ( { model | tacViews = Loaded list }
            , Cmd.none
            )

          Err err -> ( { model | tacViews = LoadError (errToString err) }, Cmd.none)

      GotCurrentMissionResult result ->
        case result of
          Ok name ->
            ( { model | currenMission = Loaded name }
            , Cmd.none
            )

          Err err -> ( { model | currenMission = LoadError (errToString err) }, Cmd.none)



-- Subscriptions
subscriptions : Model -> Sub UploadMsg
subscriptions model = Sub.none


-- View
view : Model -> { title : String, body : List (Html UploadMsg) }
view model =
    let
      errorCss = case model.file of
          Error _ -> " form-field-input-container-error"
          _ -> ""
    in
    { title = "Server File Management"
    , body =
        [ div [ id "gate" ]
          [ div [ id "upload" ]
            <| h3 [] [ text "Upload Mission File" ]
              :: case model.file of
                Uploading file ->
                  [ viewLoadingWithMsg <| "Uploading " ++ File.name file ]
                _ ->
                  [ label [ class "form-field-label", for "form-type" ]
                      [ span [ class "form-field-mandatory" ] [ text "Select File " ] ]
                  , div
                      [ class <| "form-field-input-container" ++ errorCss
                      , onClick ClickedSelectFile
                      ]
                      (
                          [ button
                              [ class "button"
                              ]
                              [ text "Select File.."
                              ]
                          ]
                          ++
                          case model.file of
                              Selected file ->
                                  [ div [ style "margin-left" "1em" ] [ text <| File.name file ]
                                  ]
                              Uploading file ->
                                  [ div [ style "margin-left" "1em" ] [ text <| File.name file ]
                                  ]
                              _ -> []
                      )
                  , div [ class "form-field-description" ] [ text "Select the file to upload" ]
                  , div
                    []
                    <| case model.file of
                      None -> []
                      Selected file ->
                        [ button
                          [ onClick <| ClickedUpload file
                          , class "button"
                          ]
                          [ text "Upload" ]
                        ]
                      Uploading file ->
                        [ viewLoadingWithMsg <| "Uploading file " ++  File.name file
                        ]
                      Error error ->
                        [ div []
                          [ text "Something went wrong: "
                          , text error
                          ]
                        ]
                      Success ->
                        [ div [] [ text "Upload Successful"]
                        ]
                  ]
            ]
          , div [ id "liberation" ]
            [ h3 [] [ text "Liberation Status" ]
            , div []
              [ a
                [ href "/api/liberation/state.json"
                , target "new"
                ]
                [ text "Download Liberation Status"
                ]
              ]
            ]
          , div [ id "current-mission" ]
            [ h3 [] [ text "Current Mission" ]
            , div []
              <| case model.currenMission of
                Loading ->
                  [ viewLoadingWithMsg "Loading Current Mission" ]

                Loaded mis ->
                  [ text mis.filename ]

                LoadError err ->
                  [ div [] [ text err ] ]
            ]
          , div [ id "mission-list" ]
            [ h3 [] [text "Existing Missions"]
            , div []
              <| case model.missions of
                Loading ->
                  [ viewLoadingWithMsg "Loading Mission files" ]

                Loaded missions ->
                  List.map viewMission missions

                LoadError err ->
                  [ div [] [ text err ] ]
            ]
          , div [ id "tac-view-list" ]
            [ h3 [] [text "Existing TacView Files"]
            , div []
              <| case model.tacViews of
                Loading ->
                  [ viewLoadingWithMsg "Loading TacView Files" ]

                Loaded tacViews ->
                  List.map viewTacView tacViews

                LoadError err ->
                  [ div [] [ text err ] ]
            ]
          ]
    }

viewMission : FileName -> Html msg
viewMission miz =
  div []
    [ text miz.filename
    ]

viewTacView : FileName -> Html msg
viewTacView tv =
  div []
    [ a
      [ href <| "/api/tacviews/" ++ tv.filename
      , target "new"
      ]
      [ text tv.filename
      ]
    ]
