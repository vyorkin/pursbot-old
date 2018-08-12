module Main where

import Prelude

import ChocoPie (runChocoPie)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (ignoreCase) as Regex
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import Foreign (unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Prelude.Unicode ((∘), (◇))
import PursBot.Config (Config)
import PursBot.Config as Config
import PursBot.Pursuit as Pursuit
import PursBot.Pursuit.Search as Search
import TelegramBot (Bot, Chat)
import TelegramBot as Bot

type Query =
  { chat ∷ Chat
  , params ∷ Search.Params
  }

type Output =
  { chat ∷ Chat
  , text ∷ String
  }

main ∷ Effect Unit
main = launchAff_ do
  config ← Config.load
  liftEffect $
    case config of
      Right cfg → runChocoPie main' (mkDrivers cfg)
      Left err → Console.log $ "Malformed config: " ◇ show err

type Main
  = { bot ∷ Event Query
    , search ∷ Event Output
    }
  → { bot ∷ Event Output
    , search ∷ Event Query
    }

type Drivers =
  { bot ∷ Event Output → Effect (Event Query)
  , search ∷ Event Query → Effect (Event Output)
  }

main' ∷ Main
main' sources =
  { bot: sources.search
  , search: sources.bot
  }

mkDrivers ∷ Config → Drivers
mkDrivers config =
  { bot: bot config
  , search
  }

bot ∷ Config → Event Output → Effect (Event Query)
bot config outputs = do
  conn ← Bot.connect config.token
  void $ Event.subscribe outputs \{ chat, text } → do
    Bot.sendMessage conn (show chat.id) text (unsafeToForeign options)
  getMessages conn
  where
    options =
      { parse_mode: "Markdown"
      , disable_web_page_preview: true
      , disable_notification: true
      }

getMessages ∷ Bot → Effect (Event Query)
getMessages conn = do
  { event, push } ← Event.create
  Bot.onText conn searchPattern (handler push)
  pure event
  where
    handler push m m'
      | Right msg ← runExcept m
      , Right m'' ← runExcept m'
      , Just matches ← m''
        = push $ mkQuery msg.chat (unsafePartial $ fromJust $ Array.last matches)
      | otherwise
        = pure unit
    searchPattern = unsafePartial $
      fromRight $ regex "/search (.+)" Regex.ignoreCase

mkQuery ∷ Chat → String → Query
mkQuery chat text = { chat, params: Search.mkParams text }

search ∷ Event Query → Effect (Event Output)
search queries = do
  { event, push } ← Event.create
  void $ Event.subscribe queries \{ chat, params } → do
    launchAff_ do
      response ← Pursuit.search params
      let
        text = case response of
          Right results → Array.intercalate "\n" results
          Left err → Search.renderErrors params err
      liftEffect ∘ push $ { chat, text }
  pure event
