module Main exposing (..)

import Array exposing (Array)

import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (focus)
import Browser.Navigation as Nav
import Element exposing (..)
import List
import Task
import Tuple exposing (first, second)
import Url exposing (Url)

import ArticleReader exposing (..)
import Article exposing
  ( Article
  , beginEdit
  , toSentences
  , updateText
  , Cell
  , Sentences
  )
import Route exposing (Route)

type alias Model =
  { article : Article
  , key : Nav.Key
  }


exampleSentences =
  [ [ "– Tapaturmat sekä vakavasti sairaat potilaat, jotka eivät voi odottaa terveysaseman avautumiseen asti, hoidetaan edelleen Tays Ensiapu Acutassa, sairaanhoitopiiri tiedottaa."
    , "– Accidents as well as seriously ill patients who cannot wait for the health station opening up, the treatment will continue to be Tays first aid Acutassa, hospital district information."
    ]
  , [ "Asiasta kertoi Pirkanmaan sairaanhoitopiiri keskiviikkona."
    , "The Pirkanmaa hospital district on Wednesday."
    ]
  ]

getInitialArticle : Nav.Key -> String -> List (List String) -> Model
getInitialArticle key name sentences =
  beginEdit { article =
                { name = name
                , sentences = toSentences sentences
                , edit = { x = 0, y = 1 }
                }
            , key = key
            } { x = 0, y = 1}


initialModel : () -> Url -> Nav.Key -> (Model, Cmd Msg)
initialModel _ _ key =
  ( getInitialArticle
      key
      "Tays suosittelee myös, että työnantajat hyväksyisivät väliaikaisesti lyhyitä, 3–5 vuorokauden sairauslomia ilman lääkärintodistusta tai että sairauslomaan riittäisi sairaanhoitajan arvio."
      exampleSentences
  , Cmd.none
  )

type Msg
  = TextChanged Cell String
  | EditText Cell
  | Nop
  | Commit
  | ClickedLink UrlRequest
  | ChangedUrl Url

switchToArticle model article =
  (model, Cmd.none)

switchToCompose model =
  (model, Cmd.none)

gotoPage : Maybe Route -> Model -> (Model, Cmd Msg)
gotoPage route model =
  let
    root = (model, Cmd.none)
    -- root = ( { model | page = Route.Articles }, Cmd.none)
  in
    case route of
      Nothing ->
        root
      Just Route.Articles ->
        root
      Just (Route.Read article) ->
        switchToArticle model article
      Just Route.Compose ->
        switchToCompose model

updateUrl : UrlRequest -> Model -> (Model, Cmd Msg)
updateUrl urlRequest model =
  case urlRequest of
    Internal url ->
      ( model
      , Nav.pushUrl model.key (Url.toString url)
      )
    External url ->
      ( model
      , Nav.load url
      )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClickedLink urlRequest ->
      updateUrl urlRequest model
    ChangedUrl url ->
      gotoPage (Route.fromUrl url) model
    TextChanged edit s ->
      ( { model | article = updateText model.article edit s }
      , Cmd.none
      )
    EditText edit ->
      ( beginEdit model edit
      , Task.attempt (\_ -> Nop) (focus "edit")
      )
    Nop -> ( model, Cmd.none)
    Commit -> ( beginEdit model { x = -1, y = -1}, Cmd.none)

viewArticle { article } = -- TODO
  let
    events =
      { input = TextChanged
      , edit = EditText
      , commit = Commit
      }
    readHtml = newArticeHtml events article
  in
    { title = article.name
    , body = [ Element.layout [] readHtml]
    }

view : Model -> Browser.Document Msg
view model =
  viewArticle model
  -- case model.page of
  --   Articles -> viewArticles model
  --   Read Id id -> viewArticle model id
  --   Compose -> viewCompose model

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main =
  Browser.application
    { init = initialModel
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = ClickedLink
    , onUrlChange = ChangedUrl
    }
