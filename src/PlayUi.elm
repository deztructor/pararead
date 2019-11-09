module PlayUi exposing (..)

import Browser
import Element exposing (..)
import Element.Input exposing (..)


type alias Model =
  {
  }

type Msg = Nop

initialModel : () -> (Model, Cmd Msg)
initialModel _ =
  ( {}
   , Cmd.none
   )

translations =
  [ { fi = "Viime vuosina liikenne Suomen ja Venäjän välillä on vilkastunut merkittävästi. Alkuvuonna kasvu kuitenkin taittui."
    , en = "South-east Finland's border stations are located in Finland's most lively eastern border crossing."
    }
  , { fi = "Virolahden Vaalimalla, Lappeenrannan Nuijamaalla ja Vainikkalassa, Imatralla sekä Parikkalassa oli tammi-helmikuussa 1 038 000 rajanylitystä, mikä on 4 prosenttiä vähemmän kuin vuosi siten vastaavaan aikaan."
    , en = "Estonia the gulf of Cherishing, Lappeenranta, Nuijamaa and Vainikkala, Imatra and lappeenranta in January-February 1 038 000 border crossing, which is 4% less than a year, thus the corresponding time."
    }
  ]
view model =
   --div [] []
  Element.layout
    []
  <|
    Element.table
      [
      ]
      { data = translations
      , columns =
          [ { width = fill
            , header = Element.text "A"
            , view = \{fi} -> Element.paragraph [] [Element.text fi]
            }
          , { width = fill
            , header = Element.text "B"
            , view = \{en} -> Element.Input.multiline
                     [ Element.Input.focusedOnLoad ]
                     { onChange = \s -> Nop
                     , text = en
                     , placeholder = Nothing
                     , label = labelHidden ""
                     , spellcheck = False
                     }
            }
          ]
      }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      ( model
      , Cmd.none
      )

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
