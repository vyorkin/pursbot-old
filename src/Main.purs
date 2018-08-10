module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import Global.Unsafe (unsafeEncodeURIComponent)
import Milkis (URL(..), defaultFetchOptions, fetch)
import Milkis as Milkis
import Milkis.Impl.Node (nodeFetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude.Unicode ((∘), (⊙), (◇))
import Simple.JSON (readJSON)
import TelegramBot (Bot)
import TelegramBot as Bot

type Config =
  { token ∷ String
  , chatId ∷ Int
  }

type SearchInfo =
  { module ∷ String
  , title ∷ String
  , type ∷ String
  , typeOrValue ∷ String
  , typeText ∷ String
  }

type SearchResult =
  { markup ∷ String
  , package ∷ String
  , text ∷ String
  , url ∷ String
  , version ∷ String
  , info ∷ SearchInfo
  }

type SearchResponse = Array SearchResult

newtype Query = Query String
derive instance queryNewtype ∷ Newtype Query _

newtype Output = Output String
derive instance outputNewtype ∷ Newtype Output _

main ∷ Effect Unit
main = launchAff_ do
  config ← readJSON ⊙ readTextFile UTF8 "./config.json"
  case config of
    Right cfg → do
      liftEffect $ runChocoPie main_ (drivers cfg)
    Left err → do
      liftEffect $ Console.log $ "Malformed config: " ◇ show err
  where
    main_ sources =
      { bot: sources.search
      , search: sources.bot
      }

    drivers config =
      { bot: bot config
      , search
      }

bot ∷ Config → Event Output → Effect (Event Query)
bot config outputs = do
  connection ← Bot.connect config.token
  void $ Event.subscribe outputs \(Output output) →
    Bot.sendMessage connection config.chatId output
  getMessages connection

getMessages ∷ Bot → Effect (Event Query)
getMessages connection = do
  { event, push } ← Event.create
  Bot.onMessage connection \fM → case runExcept fM of
    Right m | (Just text) ← m.text → push $ Query text
    _ → Console.log "Can't do shit cap'n"
  pure event

search ∷ Event Query → Effect (Event Output)
search queries = do
  { event, push } ← Event.create
  void $ Event.subscribe queries \query → do
    traceM query
    launchAff_ do
      response ← fetch nodeFetch (mkUrl query) opts
      result ← Milkis.text response
      traceM $ "RESPONSE: \n" ◇ result ◇ "\n\n"
      liftEffect ∘ push ∘ parse $ result
  pure event
  where
    opts =
      { headers: Milkis.makeHeaders { "accept": "application/json" }
      , method: Milkis.getMethod
      }

parse ∷ String → Output
parse result = Output
  case readJSON result of
    Right (response ∷ SearchResponse) →
      Array.intercalate "\n" $
        renderSearchResult ⊙ Array.take 5 (Array.reverse response)
    Left err →
      "Couldn't parse non-result JSON: " ◇ show result

renderSearchResult ∷ SearchResult → String
renderSearchResult r =
  r.info.title ◇ " :: " ◇ r.info.typeText ◇ "\n" ◇
  "in " ◇ r.info.module ◇ "\n" ◇
  "of " ◇ r.package ◇ " v" ◇ r.version ◇ "\n" ◇
  r.url ◇ "\n"

mkUrl ∷ Query → URL
mkUrl (Query s) = URL $ baseUrl ◇ unsafeEncodeURIComponent s

baseUrl ∷ String
baseUrl = "https://pursuit.purescript.org/search?q="