module Read exposing (Languages, newArticeHtml, Sentence, SentenceEvents, IsEditing)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, keyCode)
import Debug

import List
import Json.Decode as Json

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form.Textarea as Textarea
import AutoExpand

type alias Sentence = List String
type alias Languages = List Sentence
type alias IsEditing = (Int -> Int -> Bool)

alias type Model =
  { AutoExpand.State editorState
  }

onKeyConsume : Int -> msg -> Attribute msg
onKeyConsume consumedCode msg =
    let
      isEnter code =
        if code == consumedCode then
          Json.succeed (msg, True)
        else
          Json.fail "Skip"
    in
        Html.Events.stopPropagationOn "keydown" (Json.andThen isEnter keyCode)

onEnterConsume = onKeyConsume 13


getReadingHtml edit s =
  [ p [ onClick edit ] [ text s ]
  ]

getEditingHtml input commit s =
  [ Textarea.textarea
      [ Textarea.value s
      --, Textarea.rows 0
      , Textarea.attrs
        [ style "height" "auto"
        , onEnterConsume commit
        , style "rows" ""
        , style "cols" ""
        ]
      , Textarea.onInput input
      , Textarea.id "edit"
      ]
  ]

getEditingHtml input commit s =
  [ Textarea.textarea
      [ Textarea.value s
      --, Textarea.rows 0
      , Textarea.attrs
        [ style "height" "auto"
        , onEnterConsume commit
        , style "rows" ""
        , style "cols" ""
        ]
      , Textarea.onInput input
      , Textarea.id "edit"
      ]
  ]

type alias SentenceEvents msg =
  { input : (String -> msg)
  , edit: (Int -> Int  -> msg)
  , commit : msg
  }

getCol : SentenceEvents msg -> IsEditing -> Int -> Int -> Sentence -> Grid.Column msg
getCol events is_editing x y sentence =
  let is_edit = is_editing x y
      get_html = if is_edit then
                   getEditingHtml events.input events.commit
                 else
                   getReadingHtml (events.edit x y)
      text = case sentence of
               s :: _ -> s
               [] -> ""
  in
    get_html text |> Grid.col [ Col.sm6, Col.lg6, Col.md6 ]

getRow : SentenceEvents msg -> IsEditing -> Int -> List Sentence -> Html msg
getRow events is_editing x items =
  let
    cells = List.indexedMap (getCol events is_editing x) items
  in
    Grid.row [] cells

newArticeHtml : SentenceEvents msg -> IsEditing -> List (List Sentence) -> Html msg
newArticeHtml events isCellEdit sentences =
  let
    getRowByX = getRow events isCellEdit
    rows = sentences |> List.indexedMap getRowByX
  in
    Grid.containerFluid
      [ style "height" "100%" ]
      rows


