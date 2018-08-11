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
import Prelude.Unicode ((∘), (◇))
import PursBot.Config (Config)
import PursBot.Config as Config
import PursBot.Pursuit as Pursuit
import PursBot.Pursuit.Search as Search
import TelegramBot (Bot)
import TelegramBot as Bot

newtype Output = Output String
derive instance outputNewtype ∷ Newtype Output _

main ∷ Effect Unit
main = launchAff_ do
  config ← Config.load
  liftEffect $
    case config of
      Right cfg → runChocoPie main_ (drivers cfg)
      Left err → Console.log $ "Malformed config: " ◇ show err
  where
    main_ sources =
      { bot: sources.search
      , search: sources.bot
      }

    drivers config =
      { bot: bot config
      , search
      }

bot ∷ Config → Event Output → Effect (Event Search.Params)
bot config outputs = do
  conn ← Bot.connect config.token
  void $ Event.subscribe outputs \(Output output) →
    Bot.sendMessage conn config.chatId output
  getMessages conn

getMessages ∷ Bot → Effect (Event Search.Params)
getMessages conn = do
  { event, push } ← Event.create
  Bot.onMessage conn \msg → case runExcept msg of
    Right m | (Just query) ← m.text → push $ Search.mkParams query
    _ → Console.log "Can't do shit cap'n"
  pure event

search ∷ Event Search.Params → Effect (Event Output)
search queries = do
  { event, push } ← Event.create
  void $ Event.subscribe queries \params → do
    launchAff_ do
      response ← Pursuit.search params
      let
        output = case response of
          Right results → Array.intercalate "\n" results
          Left err → Search.renderErrors params err
      liftEffect ∘ push $ Output output
  pure event
