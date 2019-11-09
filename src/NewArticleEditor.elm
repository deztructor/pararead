module NewArticleEditor exposing
  (
  )

import Element exposing (..)
import Element.Input as Input exposing (..)

type alias NewArticleEvents msg =
  { onUpdateArticle : String -> msg
  , onUpdateTitle : String -> msg
  }

editorHtml updateArticle text =
  Input.multiline
    [ width fill
    , focusedOnLoad
    , htmlAttribute <| id "edit"
    , padding 10
    ]
  { onChange = input
  , text = text
  , placeholder = Nothing
  , label = labelHidden ""
  , spellcheck = True
  }

titleHtml updateTitle text =
  Input.text
    [ width fill
    , padding 10
    ]
    { onChange = updateTitle
    , text = text
    , placeholder = Nothing
    , label : labelHidden ""
    }

newArticeEditor : NewArticle -> NewArticleEvents -> Element msg
newArticleEditor { title, text } { onUpdateArticle, onUpdateTitle } =
  Element.column
    [ width fill
    ]
    [ titleHtml onUpdateTitle
    , editorHtml onUpdateArticle
    ]
