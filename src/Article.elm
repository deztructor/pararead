module Article exposing
  ( Article
  , beginEdit
  , Cell
  , Id
  , idString
  , Languages
  , Sentence
  , Sentences
  , toSentences
  , updateText
  , urlParser
  )

import Array exposing (Array)
import Url.Parser

type Id
  = Id String

idString (Id s) = s

urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "ARTICLE" (\str -> Just (Id str))


type alias Sentence = List String
type alias Languages = List Sentence
type alias Sentences = Array Languages
type alias Cell = { x : Int, y : Int }
type alias Article
  = { name : String
    , sentences: Sentences
    , edit: Cell
    }
type alias Model a =
  { a
    | article : Article
  }

updateHead : String -> Sentence -> Sentence
updateHead value sentence =
  case sentence of
    _ :: xs -> value :: xs
    [] -> [value]

replaceLang : Int -> String -> Languages -> Languages
replaceLang i to items =
  List.indexedMap
    ( \j sentence ->
        if i == j
        then (updateHead to sentence)
        else sentence
    ) items

updatedArticleSentences : Sentences -> Cell -> String -> Sentences
updatedArticleSentences sentences {x, y} s =
  let
    updateRow row = Array.set x (replaceLang y s row) sentences
  in
    Array.get x sentences
      |> Maybe.map updateRow
      |> Maybe.withDefault sentences

toSentences : List (List String) -> Sentences
toSentences lines =
  let
    langsAsStacks = List.map (\lang -> [lang])
  in
    lines
      |> List.map langsAsStacks
      |> Array.fromList

beginEditSentence : List String -> List String
beginEditSentence col =
    case col of
      x :: xs -> if Just x /= (List.head xs) then x :: x :: xs else col
      _ -> [""]

beginEditCol :  Int -> Languages -> Maybe Languages
beginEditCol y row =
  let
    arr = Array.fromList row
    setY col =
      Array.set y (beginEditSentence col) arr
        |> Array.toList
  in
    Array.get y arr
      |> Maybe.map setY

beginEditRow : Article -> Cell -> Maybe Article
beginEditRow article {x, y} =
  let
    sentences = article.sentences
    editColY = beginEditCol y
    updatedRowX row =
      { article
        | sentences = Array.set x row sentences
      }
  in
    Array.get x article.sentences
      |> Maybe.andThen editColY
      |> Maybe.map updatedRowX

beginEdit : Model a -> Cell -> Model a
beginEdit model edit =
  let
    current_article
      = model.article
    updated_article
      = beginEditRow model.article edit
        |> Maybe.map (\article -> { article | edit = edit })
        |> Maybe.withDefault { current_article | edit = { x = -1, y = -1 } }
  in
    { model | article = updated_article }

updateText : Article -> Cell -> String -> Article
updateText article edit s =
  { article
    | sentences = updatedArticleSentences article.sentences edit s
  }
