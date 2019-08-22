module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, keyCode)
import List
import Task
import Tuple exposing (first, second)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Read exposing (..)

type alias Sentences = Array Languages
type alias Article = { name : String, sentences: Sentences }
type alias Edit = { x : Int, y : Int }
type alias Model =
  { article : Article
  , edit : Edit
  }


getSentences lines =
  List.map (\line -> List.map (\lang -> [lang]) line) lines |> Array.fromList

getInitialArticle : String -> List (List String) -> Model
getInitialArticle name sentences =
  beginEdit { article =
                { name = name
                , sentences = getSentences sentences
                }
            , edit = { x = 0, y = 1 }
            } 0 1

beginEditSentence : List String -> List String
beginEditSentence col =
    case col of
      x :: xs -> if Just x /= (List.head xs) then x :: x :: xs else col
      _ -> [""]

beginEditCol : Languages -> Int -> Maybe Languages
beginEditCol row y =
  let
    arr = Array.fromList row
    maybe_col = Array.get y arr
  in
    case maybe_col of
      Just col -> Array.set y (beginEditSentence col) arr |> Array.toList |> Just
      Nothing -> Nothing

beginEditRow : Article -> Int -> Int -> Maybe Article
beginEditRow article x y =
  let maybe_row = Array.get x article.sentences
  in
    case maybe_row of
      Just row ->
        let maybe_edited_row = beginEditCol row y
        in
          case maybe_edited_row of
            Just edited_row -> Just { article | sentences = Array.set x edited_row article.sentences }
            Nothing -> Nothing
      Nothing -> Nothing

beginEdit : Model -> Int -> Int -> Model
beginEdit model x y =
  let
    maybe_article = beginEditRow model.article x y
  in
    case maybe_article of
      Just article -> { model | article = article, edit = { x = x, y = y } }
      Nothing -> { model | edit = { x = -1, y = -1 } }

initialModel : () -> (Model, Cmd Msg)
initialModel _ =
  ( getInitialArticle
      "Some article"
      [ [ "a", "A" ]
      , [ "b", "B" ]
      ]
  , Cmd.none
  )

type Msg
  = TextChanged Int Int String
  | EditText Int Int
  | Nop
  | Commit

updateHead : String -> Sentence -> Sentence
updateHead value sentence =
  case sentence of
    _ :: xs -> value :: xs
    [] -> [value]

rowReplace : Int -> String -> Languages -> Languages
rowReplace i to items =
  List.indexedMap (\j original -> if i == j then (updateHead to original) else original) items

updatedArticleSentences article x y s =
  let maybe_row = Array.get x article.sentences
  in
    case maybe_row of
      Just row -> Array.set x (rowReplace y s row) article.sentences
      Nothing -> article.sentences

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TextChanged x y s ->
      let article = model.article
      in
        ( { model | article = { name = article.name, sentences = updatedArticleSentences article x y s } }
        , Cmd.none
        )
    EditText x y ->
      ( beginEdit model x y
      , Task.attempt (\_ -> Nop) (Browser.Dom.focus "edit")
      )
    Nop -> ( model, Cmd.none)
    Commit -> ( beginEdit model -1 -1, Cmd.none)

view : Model -> Html Msg
view model =
  let
    isEditing = \x y -> (x, y) == (model.edit.x, model.edit.y)
    onTextInput = TextChanged model.edit.x model.edit.y
    sentences = Array.toList model.article.sentences
    events =
      { input = onTextInput
      , edit = EditText
      , commit = Commit
      }
    rows = newArticeHtml events isEditing
  in
    div []
        [ h1 [] [ text model.article.name ]
        , CDN.stylesheet
        , newArticeHtml events isEditing sentences
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main =
  Browser.element
    { init = initialModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
