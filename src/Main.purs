module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import Prelude.Unicode ((∘), (⊙), (◇))
import PursBot.Config (Config)
import PursBot.Config as Config
import PursBot.Pursuit as Pursuit
import Simple.JSON (readJSON)
import TelegramBot (Bot)
import TelegramBot as Bot

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
  config ← Config.load
  case config of
    Right cfg → liftEffect $ runChocoPie main_ (drivers cfg)
    Left err  → liftEffect $ Console.log $ "Malformed config: " ◇ show err
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
  void $ Event.subscribe queries \(Query query) → do
    launchAff_ do
      response ← Pursuit.search query
      liftEffect ∘ push ∘ parse $ response
  pure event

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
