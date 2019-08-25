module Read exposing (Languages, newArticeHtml, Sentence, SentenceEvents, IsEditing)
--import Html.Attributes exposing (..)
import Debug

import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Input as Input exposing (..)
import Element.Events as Events exposing (..)
import Element.Font as Font
import Element.Region as Region exposing (heading)

import Html.Attributes exposing (id)
import Html as H

import List

import Widgets exposing (..)

type alias Sentence = List String
type alias Languages = List Sentence
type alias IsEditing = (Int -> Int -> Bool)


getReadingHtml edit s =
  Element.paragraph
    [ onClick edit
    , width fill
    , padding 10
    ]
  [ Element.text s
  ]


getEditor input commit s =
  Input.multiline
    [ focusedOnLoad
    , width fill
    , onEnterConsume commit
    , htmlAttribute <| id "edit"
    , padding 10
    ]
  { onChange = input
  , text = s
  , placeholder = Nothing
  , label = labelHidden ""
  , spellcheck = False
  }


getEditingHtml input commit s =
  let
    editButtons = row [ width fill
                      , spacing 10 ]
                  [ styledButton okColor commit "Commit"
                  , styledButton okColor commit "Cancel"
                  ]
  in
    column [ width fill
           , spacing 10
           ] [ getEditor input commit s
             , editButtons
             ]

type alias SentenceEvents msg =
  { input : (String -> msg)
  , edit: (Int -> Int  -> msg)
  , commit : msg
  }

getCol : SentenceEvents msg -> IsEditing -> Int -> Int -> Sentence -> Element msg
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
    get_html text

getRow : SentenceEvents msg -> IsEditing -> Int -> List Sentence -> Element msg
getRow events is_editing x items =
  let
    cells = List.indexedMap (getCol events is_editing x) items
  in
    Element.row [ width (fill |> minimum 300) ] cells

headingHtml name =
  paragraph
    [ width fill
    , centerX
    , padding 20
    ]
    [ el
        [ heading 1
        , Font.size 24
        ]
        (Element.text name)
    ]

newArticeHtml : SentenceEvents msg -> IsEditing -> String -> List (List Sentence) -> Element msg
newArticeHtml events isCellEdit name sentences =
  let
    getRowByX = getRow events isCellEdit
    rows = sentences |> List.indexedMap getRowByX
    title = headingHtml name
  in
    Element.column
        []
        <| title :: rows


