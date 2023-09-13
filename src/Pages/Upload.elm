module Pages.Upload exposing (..)
import Api
import Api.Endpoint as Endpoint exposing (Endpoint(..))
import Delay
import File exposing (File)
import File.Select as Select exposing (file)
import Html exposing (Html, a, button, div, h3, i, label, span, text)
import Html.Attributes exposing (class, disabled, for, href, id, style, target)
import Html.Events exposing (onClick)
import Http exposing (emptyBody, filePart, jsonBody, multipartBody)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (string, object)
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

missionListEntryDecoder : Decoder MissionListEntry
missionListEntryDecoder =
    Decode.map2 MissionListEntry
        (Decode.field "filename" Decode.string)
        (Decode.field "index" Decode.int)

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
  , paused: Bool
  , missions: Status (List MissionListEntry)
  , tacViews: Status (List FileName)
  , currentMission: Status FileName
  }

type alias Pause =
  { pauseState: Bool
  }

type alias MissionListEntry =
  { filename: String
  , index: Int
  }

pauseDecoder : Decoder Pause
pauseDecoder =
  Decode.map Pause
    (Decode.field "pause_state" Decode.bool)

type UploadMsg
    = UpdateFile File
    | ClickedSelectFile
    | ClickedUpload File
    | ClickedRun Int
    | ClickedDelete Int
    | ClickedRefreshCurrent
    | ClickedRefreshTac
    | ClickedPause
    | RefreshPause
    | ClickedRefreshMissions
    | GotUploadResult (Result Http.Error ())
    | GotPauseResult (Result Http.Error Pause)
    | GotEmptyResult (Result Http.Error ())
    | GotPauseChangeResult (Result Http.Error ())
    | GotMissionResult (Result Http.Error (List MissionListEntry))
    | GotCurrentMissionResult (Result Http.Error FileName)
    | GotTacViewResult (Result Http.Error (List FileName))

-- Init
init : Session -> ( Model, Cmd UploadMsg )
init session =
  ({ session = session
  , file = None
  , paused = False
  , missions = Loading
  , tacViews = Loading
  , currentMission = Loading
  }
  , Cmd.batch
    [ refreshMissions session
    , refreshTacViews session
    , refreshMission session
    , getPause session
    ]
  )

refreshMissions : Session -> Cmd UploadMsg
refreshMissions session =
  Api.getSecure (sessUser session) Endpoint.mission GotMissionResult (Decode.list missionListEntryDecoder)

getPause : Session -> Cmd UploadMsg
getPause session =
  Api.getSecure (sessUser session) Endpoint.pause GotPauseResult (pauseDecoder)

refreshMission : Session -> Cmd UploadMsg
refreshMission session =
  Api.getSecure (sessUser session) Endpoint.currentMission GotCurrentMissionResult filenameDecoder

refreshTacViews : Session -> Cmd UploadMsg
refreshTacViews session =
  Api.getSecure (sessUser session) Endpoint.tacview GotTacViewResult (Decode.list filenameDecoder)
-- Update
sessionUser : Model -> User
sessionUser model = sessUser model.session

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
            ( { model | currentMission = Loaded name }
            , Cmd.none
            )

          Err err -> ( { model | currentMission = LoadError (errToString err) }, Cmd.none)

      ClickedRun missionIndex ->
        ( model
        , Cmd.batch
          [ Api.postSecureWithErrorBody (sessionUser model) Endpoint.mission GotEmptyResult
            (jsonBody
                <| Encode.object
                  [ ("mission_index", Encode.int missionIndex)
                  ]
            ) (Decode.succeed ())
          , Delay.after 1000 <| ClickedRefreshCurrent
          ]
        )

      ClickedDelete missionIndex ->
        ( model
        , Api.deleteSecureWithErrorBody (sessionUser model) Endpoint.mission GotEmptyResult
          (jsonBody
              <| Encode.object
                [ ("mission_index", Encode.int missionIndex)
                ]
          ) (Decode.succeed ())
        )

      GotEmptyResult _ ->
        (model, Cmd.none)

      ClickedRefreshCurrent ->
        ( { model | currentMission = Loading }
        , Cmd.batch
          [ refreshMission model.session
          , getPause model.session
          ]
        )

      ClickedRefreshTac ->
        ( { model | tacViews = Loading }
        , refreshTacViews model.session
        )

      ClickedRefreshMissions ->
        ( { model | missions = Loading }
        , refreshMissions model.session
        )

      ClickedPause ->
        if model.paused then
          ( model
          , Api.postSecureWithErrorBody (sessionUser model) Endpoint.unpause GotPauseChangeResult emptyBody (Decode.succeed ())
          )
        else
          ( model
          , Api.postSecureWithErrorBody (sessionUser model) Endpoint.pause GotPauseChangeResult emptyBody (Decode.succeed ())
          )


      GotPauseResult result ->
        case result of
          Ok state ->
            ( { model | paused = state.pauseState }
            , Cmd.none
            )

          Err err -> ( model, Cmd.none)


      GotPauseChangeResult _ ->
        ( model, Delay.after 500 RefreshPause )

      RefreshPause ->
        ( model, getPause model.session)


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

      currentMission = case model.currentMission of
        Loaded s -> s.filename
        _ -> ""

      pauseText = if model.paused then "Unpause" else "Pause"
      circle = if model.paused then "circle-paused" else "circle-running"
    in
    { title = "Server File Management"
    , body =
        [ div [ id "gate" ]
          [ div [ id "mission-stuff" ]
            [ div [ id "current-mission" ]
              [ div [class "split"]
                [ h3 [] [ text "Current Mission" ]
                , button
                  [ class "button button-secondary"
                  , onClick ClickedRefreshCurrent
                  ]
                  [ text "Refresh" ]
                , button
                  [ class "button"
                  , onClick ClickedPause
                  ]
                  [ text pauseText ]
                ]
              , div []
                <| case model.currentMission of
                  Loading ->
                    [ viewLoadingWithMsg "Loading Current Mission" ]

                  Loaded mis ->
                    [ div [ class "split" ]
                      [ text mis.filename
                      , span [ class circle ] []
                      ]
                    ]

                  LoadError err ->
                    [ div [] [ text err ] ]
              ]
            , div [ id "upload" ]
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
            , div [ id "mission-list" ]
              [ div [ class "split"]
                [ h3 [] [ text "Existing Missions" ]
                , button
                  [ class "button button-secondary"
                  , onClick ClickedRefreshMissions
                  ]
                  [ text "Refresh" ]
                ]
              , div []
                <| case model.missions of
                  Loading ->
                    [ viewLoadingWithMsg "Loading Mission files" ]

                  Loaded missions ->
                    List.map (viewMission currentMission) missions

                  LoadError err ->
                    [ div [] [ text err ] ]
              ]
            ]
          , div [ id "file-stuff" ]
            [ div [ id "liberation" ]
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
            , div [ id "tac-view-list" ]
              [ div [class "split"]
                [ h3 [] [ text "TacView Files" ]
                , button
                  [ class "button button-secondary"
                  , onClick ClickedRefreshTac
                  ]
                  [ text "Refresh" ]
                ]

              , div []
                <| case model.tacViews of
                  Loading ->
                    [ viewLoadingWithMsg "Loading TacView Files" ]

                  Loaded tacViews ->
                    List.map viewTacView (List.reverse tacViews)

                  LoadError err ->
                    [ div [] [ text err ] ]
            ]
          ]
        ]
      ]
    }

viewMission : String -> MissionListEntry -> Html UploadMsg
viewMission current miz =
  let
    buttonEnable = miz.filename == current ++ ".miz"
  in
  div [ class "split" ]
    [ text miz.filename
    , div []
    [ button
        [ onClick (ClickedRun miz.index)
        , class "button button-secondary"
        , disabled buttonEnable
        ]
        [ text "Run" ]
      , button
        [ onClick (ClickedDelete miz.index)
        , class "button button-secondary"
        , disabled buttonEnable
        ]
        [ i [ class "fas fa-trash" ] [] ]
      ]
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
