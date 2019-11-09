module NewArticle exposing
  (..)

import List
import Parser exposing (..)

type alias NewArticle
  = { title : String
    , text : String
    }

type alias Model a
  = { a
      | newArticle : NewArticle
    }

-- splitLines = String.trim |> String.split "\n"

type Part
  = Sentence String
  | Para

anyOf chars c = String.any (\a -> c == a) chars

oneOrMany fit =
  getChompedString
  <| succeed ()
    |. chompIf fit
    |. chompWhile fit

zeroOrMany fit =
  getChompedString
  <| succeed ()
    |. chompWhile fit

pair p1 p2 getP3 =
  succeed Tuple.pair
    |= p1
    |= p2
    |> andThen getP3

mergeStringPair fn =
  \(x, xs) -> succeed (fn <| x ++ xs)

isEOS = anyOf ".?!"
getEOS =
  pair (oneOrMany isEOS) (zeroOrMany (\c -> isEOS c || c == '"')) (mergeStringPair identity)

notEOS =
  getChompedString <| oneOrMany (not << isEOS)

getSentence =
  succeed Tuple.pair
    |= notEOS
    |= getEOS
    |> andThen (\(t, eos) -> succeed (Sentence <| t ++ eos))

getPara =
  succeed Para
    |. oneOrMany (\c -> c == '\n')

getPart =
  oneOf
  [ getPara
    , getSentence
    ]

-- getParts : List String -> Parser (Step (List Part) (List Part))
getParts revParts =
  oneOf
    [ succeed (\part -> Loop <| part :: revParts)
        |= getPart
    , succeed ()
      |> map (\_ -> Done <| List.reverse revParts)
    ]

parts : Parser (List Part)
parts =
  loop [] getParts
