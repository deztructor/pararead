module Widgets exposing
  ( okColor
  , onEnterConsume
  , onKeyConsume
  , styledButton
  )

import Element exposing (..)
import Element.Background as Background exposing (..)
import Element.Events as Events exposing (..)
import Element.Input as Input exposing (..)
import Json.Decode as Json
import Html.Events exposing (keyCode)

-- onKeyConsume : Int -> msg -> Attribute msg
onKeyConsume consumedCode msg =
    let
      isEnter code =
        if code == consumedCode then
          Json.succeed (msg, True)
        else
          Json.fail "Skip"
    in
        htmlAttribute
        <| Html.Events.stopPropagationOn "keydown"
        <| Json.andThen isEnter keyCode

onEnterConsume = onKeyConsume 13

okColor =
    Element.rgb255 238 238 255

styledButton color msg label =
  button
  [ Background.color color
  , padding 10
  ] { onPress = Just msg
    , label = Element.text label
    }
