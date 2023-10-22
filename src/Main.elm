module Main exposing (Task)

import Browser
import Html exposing (Html, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Platform exposing (Task)



-- todo app criterias:
-- List view that shows all todos:
-- on each row delete button
-- outside input and add button where we can add a new todo


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { tasks = []
    , inputText = ""
    }


type TaskStatus
    = Done
    | NotDone


type alias Task =
    { name : String
    , description : String
    , status : TaskStatus
    }



-- Model


type alias Model =
    { tasks : List Task
    , inputText : String
    }



-- MSG


type Msg
    = AddTask
    | DeleteTask String
    | ChangeInput String
    | UpdateTaskStatus String TaskStatus



-- UPDATE
-- we can introduce let/in for more complex logic for each case, if we need to idenfity local vars to be used in the in.
-- mayeb make msgs parametrized? whats the benefit? I mean we can access shit in the model right? the tracked state?
--  unless we can pass something that we dont neecessary track in hte model? info that will be used to update the model


removeTask : String -> List Task -> List Task
removeTask taskName tasks =
    let
        filterCondition task =
            task.name /= taskName
    in
    List.filter filterCondition tasks


updateTaskStatusHelper : TaskStatus -> String -> Task -> Maybe Task
updateTaskStatusHelper newStatus taskName task =
    if task.name == taskName then
        Just { task | status = newStatus }

    else
        Just task


renderTaskStatus : Task -> Html Msg
renderTaskStatus task =
    case task.status of
        Done ->
            button [ onClick (UpdateTaskStatus task.name NotDone) ] [ text "Mark as Not Done" ]

        NotDone ->
            button [ onClick (UpdateTaskStatus task.name Done) ] [ text "Mark Done" ]


renderTask : Task -> Html Msg
renderTask task =
    li []
        [ text task.name
        , button [ onClick (DeleteTask task.name) ] [ text "Delete Todo" ]
        , renderTaskStatus task
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTask ->
            { model | tasks = model.tasks ++ [ Task model.inputText "DefaultDescription" NotDone ] }

        DeleteTask taskName ->
            let
                _ =
                    Debug.log "tasks: " model.tasks
            in
            { model | tasks = removeTask taskName model.tasks }

        ChangeInput newInput ->
            { model | inputText = newInput }

        UpdateTaskStatus taskName newStatus ->
            { model | tasks = List.filterMap (updateTaskStatusHelper newStatus taskName) model.tasks }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "text", value model.inputText, onInput ChangeInput ] []
            , button [ onClick AddTask ] [ text "Add" ]
            ]
        , div []
            [ h1 [] [ text "Tasks" ]
            , ul [] (List.map renderTask model.tasks)
            ]
        ]
