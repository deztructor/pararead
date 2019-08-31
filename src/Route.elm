module Route exposing (Route(..), fromUrl)

import Html.Attributes as Attributes
import Html exposing (Attribute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)

import Article exposing (Id, idString)

type Route
  = Articles
  | Read Id
  | Compose


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Articles Parser.top
    , Parser.map Read <| s "read" </> Article.urlParser
    , Parser.map Compose <| s "new"
    ]

href : Route -> Attribute msg
href route =
    Attributes.href <| toString route

fromUrl : Url -> Maybe Route
fromUrl url =
  Parser.parse parser
    <| { url
         | path = Maybe.withDefault "" url.fragment
         , fragment = Nothing
       }

toString : Route -> String
toString route =
  let
    parts =
      case route of
        Articles -> []
        Read s -> [ "read", idString s ]
        Compose -> [ "new " ]
  in
    String.join "/" <| "#" :: parts
