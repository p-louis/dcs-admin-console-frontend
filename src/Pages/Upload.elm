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

serviceStateDecoder : Decoder ServiceState
serviceStateDecoder =
    Decode.map (\x -> if x == "running" then Running else Stopped)
        (Decode.field "status" Decode.string)

missionListEntryDecoder : Decoder MissionListEntry
missionListEntryDecoder =
    Decode.map2 MissionListEntry
        (Decode.field "filename" Decode.string)
        (Decode.field "index" Decode.int)

type Status t
  = Loading
  | Loaded t
  | LoadError String

type ServiceState
  = Stopped
  | Running
  | StateLoading
  | StateError

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
  , dcsStatus: ServiceState
  , srsStatus: ServiceState
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
    | ClickedRefreshStates
    | ClickedRestartDcs
    | ClickedRestartSrs
    | GotUploadResult (Result Http.Error ())
    | GotRestartDcsResult (Result Http.Error ())
    | GotRestartSrsResult (Result Http.Error ())
    | GotDcsStateResult (Result Http.Error ServiceState)
    | GotSrsStateResult (Result Http.Error ServiceState)
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
  , dcsStatus = StateLoading
  , srsStatus = StateLoading
  }
  , Cmd.batch
    [ refreshMissions session
    , refreshTacViews session
    , refreshMission session
    , getPause session
    , refreshDcsState session
    , refreshSrsState session
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

refreshDcsState : Session -> Cmd UploadMsg
refreshDcsState session =
  Api.getSecure (sessUser session) Endpoint.dcs GotDcsStateResult serviceStateDecoder

refreshSrsState : Session -> Cmd UploadMsg
refreshSrsState session =
  Api.getSecure (sessUser session) Endpoint.srs GotSrsStateResult serviceStateDecoder

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

      ClickedRefreshStates ->
        ( { model | srsStatus = StateLoading, dcsStatus = StateLoading }
        , Cmd.batch
          [ refreshSrsState model.session
          , refreshDcsState model.session
          ]
        )

      GotDcsStateResult result ->
        case result of
          Ok status ->
            ( { model | dcsStatus = status }
            , Cmd.none
            )

          Err _ ->
            ( { model | dcsStatus = StateError }
            , Cmd.none
            )

      GotSrsStateResult result ->
        case result of
          Ok status ->
            ( { model | srsStatus = status }
            , Cmd.none
            )

          Err _ ->
            ( { model | srsStatus = StateError }
            , Cmd.none
            )

      ClickedRestartDcs ->
        ( model
        , Api.postSecureWithErrorBody (sessionUser model) Endpoint.dcs GotRestartDcsResult emptyBody (Decode.succeed ())
        )

      ClickedRestartSrs ->
        ( model
        , Api.postSecureWithErrorBody (sessionUser model) Endpoint.srs GotRestartSrsResult emptyBody (Decode.succeed ())
        )

      GotRestartDcsResult result ->
        ( { model | dcsStatus = StateLoading }
        , refreshDcsState model.session
        )


      GotRestartSrsResult result ->
        ( { model | srsStatus = StateLoading }
        , refreshSrsState model.session
        )



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

      pauseClass = if model.paused then "fas fa-play" else "fas fa-pause"
      pauseText = if model.paused then "Play" else "Pause"
      circle = if model.paused then "circle-paused" else "circle-running"
    in
    { title = "DCS Server Management"
    , body =
        [ div [ id "gate" ]
          [ div [ id "mission-stuff" ]
            [ div [ id "services" ]
              [ div [class "split"]
                [ h3 [] [ text "Service States" ]
                , viewRefreshButton ClickedRefreshStates
                ]
              , div []
                <| case model.dcsStatus of
                  StateLoading ->
                    [ viewLoadingWithMsg "Loading DCS Status" ]

                  Running ->
                    [ div [ class "split" ]
                      [ text "DCS Running"
                      , viewRestartButton ClickedRestartDcs
                      ]
                    ]

                  Stopped ->
                    [ div [ class "split" ]
                      [ text "DCS Stopped"
                      , viewRestartButton ClickedRestartDcs
                      ]
                    ]

                  StateError ->
                    [ div [ class "split" ]
                      [ text "Error Loading DCS Status"
                      ]
                    ]
              , div []
                <| case model.srsStatus of
                  StateLoading ->
                    [ viewLoadingWithMsg "Loading SRS Status" ]

                  Running ->
                    [ div [ class "split" ]
                      [ text "SRS Running"
                      , viewRestartButton ClickedRestartSrs
                      ]
                    ]

                  Stopped ->
                    [ div [ class "split" ]
                      [ text "SRS Stopped"
                      , viewRestartButton ClickedRestartSrs
                      ]
                    ]

                  StateError ->
                    [ div [ class "split" ]
                      [ text "Error Loading SRS Status"
                      ]
                    ]
              ]
            , div [ id "current-mission" ]
              [ div [class "split"]
                [ h3 [] [ text "Current Mission" ]
                , div [ class "button-group" ]
                  [ viewRefreshButton ClickedRefreshCurrent
                  ,  button
                    [ class "button icon-button"
                    , onClick ClickedPause
                    ]
                    [ i [ class pauseClass ] []
                    , span [ class "label" ] [ text pauseText ]
                    ]
                  ]
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
                , viewRefreshButton ClickedRefreshMissions
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
                , viewRefreshButton ClickedRefreshTac
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

viewRefreshButton : msg -> Html msg
viewRefreshButton message =
  button
    [ class "button button-secondary icon-button"
    , onClick message
    ]
    [ i [ class "fas fa-sync-alt" ] []
    , span [ class "label" ] [ text "Refresh" ]
    ]

viewRestartButton : msg -> Html msg
viewRestartButton message =
  button
    [ class "button button-secondary icon-button"
    , onClick message
    ]
    [ i [ class "fas fa-power-off" ] []
    , span [ class "label" ] [ text "Restart" ]
    ]

viewMission : String -> MissionListEntry -> Html UploadMsg
viewMission current miz =
  let
    buttonEnable = miz.filename == current ++ ".miz"
  in
  div [ class "split" ]
    [ text miz.filename
    , div [ class "button-group" ]
    [ button
        [ onClick (ClickedRun miz.index)
        , class "button button-secondary"
        , disabled buttonEnable
        ]
        [ i [ class "fas fa-play" ] []
        , span [ class "label" ] [ text "Run" ]
        ]
      , button
        [ onClick (ClickedDelete miz.index)
        , class "button button-secondary"
        , disabled buttonEnable
        ]
        [ i [ class "fas fa-trash" ] []
        , span [ class "label" ] [ text "Delete" ]
        ]
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
