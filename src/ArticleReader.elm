module ArticleReader exposing (newArticeHtml, SentenceEvents)
--import Html.Attributes exposing (..)
import Debug

import Array exposing (Array)
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

import Article exposing
  ( Article
  , Cell
  , Languages
  , Sentence
  )


type alias SentenceEvents msg =
  { input : Cell -> String -> msg
  , edit : Cell  -> msg
  , commit : msg
  }

getReadingHtml edit s =
  Element.paragraph
    [ onClick edit
    , width fill
    , padding 10
    ]
  [ Element.text s
  ]

getEditor : (String -> msg) -> msg -> String -> Element msg
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

getCol : SentenceEvents msg -> Cell -> Int -> Int -> Sentence -> Element msg
getCol events edit x y sentence =
  let
    cur_cell = { x = x, y = y}
    get_html = if cur_cell == edit then
                 getEditingHtml (events.input cur_cell) events.commit
               else
                 getReadingHtml (events.edit cur_cell)
    text = case sentence of
             s :: _ -> s
             [] -> ""
  in
    get_html text

getRow : SentenceEvents msg -> Cell -> Int -> List Sentence -> Element msg
getRow events edit x items =
  let
    cells = List.indexedMap (getCol events edit x) items
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

newArticeHtml : SentenceEvents msg -> Article -> Element msg
newArticeHtml events { name, edit, sentences } =
  let
    getRowByX = getRow events edit
    rows
      = Array.toList
        <| Array.indexedMap getRowByX sentences
    title = headingHtml name
  in
    Element.column
        []
        <| title :: rows


