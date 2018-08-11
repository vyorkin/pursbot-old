module PursBot.Config
  ( Config
  , load
  ) where

import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Prelude.Unicode ((⊙))
import Simple.JSON (E, readJSON)

type Config =
  { token ∷ String
  , chatId ∷ Int
  }

load ∷ Aff (E Config)
load = readJSON ⊙ readTextFile UTF8 "./config.json"
