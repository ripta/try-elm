module Todo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Json
import Signal exposing (Address, Signal)
import String
import Window

type alias Model =
  { id         : Int
  , tasks      : List Task
  , field      : String
  , visibility : String }

type alias Task =
  { description : String
  , isDone      : Bool
  , isEditing   : Bool
  , id          : Int }

task : (String, Int) -> Task
task (description, id) =
  { description = description
  , isDone      = False
  , isEditing   = False
  , id          = id }

emptyModel : Model
emptyModel =
  { id         = 0
  , tasks      = []
  , field      = ""
  , visibility = "All" }

type Action =
    Noop
  | UpdateField String
  | EditTask Int Bool
  | UpdateTask Int String
  | AddTask
  | DeleteTask Int
  | DeleteDoneTasks
  | Mark Int Bool
  | MarkAll Bool
  | UpdateVisibility String

update : Action -> Model -> Model
update action model =
  case action of
    Noop -> model
    UpdateField message ->
      { model |
        field <- message }
    EditTask id isEditing ->
      let
        update t =
          if t.id == id
            then { t | isEditing <- isEditing }
            else t
      in
        { model |
          tasks <- List.map update model.tasks }
    UpdateTask id message ->
      let
        update t =
          if t.id == id
            then { t | description <- message }
            else t
      in
        { model |
          tasks <- List.map update model.tasks }
    AddTask ->
      { model |
        id <- model.id + 1
      , field <- ""
      , tasks <-
          if String.isEmpty model.field
            then model.tasks
            else model.tasks ++ [task (model.field, model.id)]
      }
    DeleteTask id ->
      let matcher t = t.id /= id
      in
        { model |
          tasks <- List.filter matcher model.tasks }
    DeleteDoneTasks ->
      let matcher t = not t.isDone
      in
        { model |
          tasks <- List.filter matcher model.tasks }
    Mark id isDone ->
      let
        update t =
          if t.id == id
            then { t | isDone <- isDone }
            else t
      in
        { model |
          tasks <- List.map update model.tasks }
    MarkAll isDone ->
      let update t = { t | isDone <- isDone }
      in
        { model |
          tasks <- List.map update model.tasks }
    UpdateVisibility visibility ->
      { model |
        visibility <- visibility }

view : Address Action -> Model -> Html
view address model =
  div
    [ class "todo-app-wrapper"
    , style [ ("visibility", "hidden") ] ]
    [ section
      [ id "todo_app" ]
      [ lazy2 taskEntry address model.field
      , lazy3 taskList address model.visibility model.tasks
      , lazy3 controls address model.visibility model.tasks ]
    , infoFooter
    ]

onEnter : Address a -> a -> Attribute
onEnter address value =
  on "keydown"
    (Json.customDecoder keyCode is13)
    (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13
    then Ok ()
    else Err "not Enter"

taskEntry : Address Action -> String -> Html
taskEntry address task =
  header
    [ id "header" ]
    [ h1 []
      [ text "todos"]
    , input 
      [ id "new-todo"
      , placeholder "What do you need to do?"
      , autofocus True
      , value task
      , name "newTodo"
      , on "input" targetValue (Signal.message address << UpdateField)
      , onEnter address AddTask ]
      []
    ]

taskList : Address Action -> String -> List Task -> Html
taskList address visibility tasks =
  let
    isVisible todo =
      case visibility of
        "Done"   -> todo.isDone
        "Active" -> not todo.isDone
        "All"    -> True
    allDone = List.all .isDone tasks
    cssVisibility =
      if List.isEmpty tasks
        then "hidden"
        else "visible"
  in
    section
      [ id "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ input
        [ id "toggle-all"
        , type' "checkbox"
        , name "toggle"
        , checked allDone
        , onClick address (MarkAll (not allDone))
        ]
        []
      , label
        [ for "toggle-all" ]
        [ text "Mark all as done" ]
      , ul
        [ id "todo-list" ]
        (List.map
          (todoItem address)
          (List.filter isVisible tasks)
        )
      ]

todoItem : Address Action -> Task -> Html
todoItem address todo =
  li
    [ classList
      [ ("done", todo.isDone)
      , ("editing", todo.isEditing)
      ]
    ]
    [ div
      [ class "view" ]
      [ input
        [ class "toggle"
        , type' "checkbox"
        , checked todo.isDone
        , onClick address (Mark todo.id (not todo.isDone))
        ]
        []
      , label
        [ onDoubleClick address (EditTask todo.id True) ]
        [ text todo.description ]
      , button
        [ class "destroy"
        , onClick address (DeleteTask todo.id)
        ]
        []
      ]
    , input
      [ class "edit"
      , value todo.description
      , name "title"
      , id ("todo-" ++ toString todo.id)
      , on "input" targetValue (Signal.message address << UpdateTask todo.id)
      , onBlur address (EditTask todo.id False)
      , onEnter address (EditTask todo.id False)
      ]
      []
    ]

controls : Address Action -> String -> List Task -> Html
controls address visibility tasks =
  let
    tasksDone = List.length (List.filter .isDone tasks)
    tasksLeft = List.length tasks - tasksDone
    item_     = if tasksLeft == 1 then " item" else " items"
  in
    footer
      [ id "footer"
      , hidden (List.isEmpty tasks)
      ]
      [ span
        [ id "todo-count" ]
        [ strong
          []
          [ text (toString tasksLeft) ]
        , text (item_ ++ " left")
        ]
      , ul
        [ id "filters" ]
        [ visibilitySwap address "#/" "All" visibility
        , text " "
        , visibilitySwap address "#/active" "Active" visibility
        , text " "
        , visibilitySwap address "#/done" "Done" visibility
        ]
      , button
        [ class "clear-done"
        , id "clear_done"
        , hidden (tasksDone == 0)
        , onClick address DeleteDoneTasks
        ]
        [ text ("Clear done (" ++ toString tasksDone ++ ")") ]
      ]

visibilitySwap : Address Action -> String -> String -> String -> Html
visibilitySwap address uri visibility actual =
  li
    [ onClick address (UpdateVisibility visibility) ]
    [ a
      [ href uri
      , classList [ ("selected", visibility == actual) ]
      ]
      [ text visibility ]
    ]

infoFooter =
  footer
    [ id "info" ]
    [ p [] [ text "Double-click to edit" ]
    , p [] [ text "Part of http://todomvc.com" ]
    ]

main : Signal Html
main =
  model
  |> Signal.map (view actions.address)

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

initialModel : Model
initialModel = Maybe.withDefault emptyModel getStorage

actions : Signal.Mailbox Action
actions = Signal.mailbox Noop

port focus : Signal String
port focus =
  let
    needsFocus action =
      case action of
        EditTask _ isEditing -> isEditing
        _ -> False
    toSelector (EditTask id _) = ("#todo-" ++ toString id)
  in
    actions.signal
    |> Signal.filter needsFocus (EditTask 0 True)
    |> Signal.map toSelector

port getStorage : Maybe Model

port setStorage : Signal Model
port setStorage = model
