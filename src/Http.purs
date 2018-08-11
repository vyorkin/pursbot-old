module PursBot.Http (get) where

import Prelude
import Effect.Aff (Aff)
import Milkis (Response, URL, getMethod, makeHeaders)
import Milkis as Milkis
import Milkis.Impl.Node (nodeFetch)

get ∷ URL → Aff String
get url = Milkis.fetch nodeFetch url options >>= Milkis.text
  where
    options =
      { headers: makeHeaders { "accept": "application/json" }
      , method: getMethod
      }
