module PursBot.Pursuit (search) where

import Prelude

import Effect.Aff (Aff)
import Global.Unsafe (unsafeEncodeURIComponent)
import Milkis (Fetch, URL(..), getMethod, makeHeaders)
import Milkis as Milkis
import Milkis.Impl.Node (nodeFetch)
import Prelude.Unicode ((∘))

search ∷ String → Aff String
search q = fetch (mkUrl q) options >>= Milkis.text
  where
    options =
      { headers: makeHeaders { "accept": "application/json" }
      , method: getMethod
      }

mkUrl ∷ String → URL
mkUrl = URL ∘ (append baseUrl) ∘ unsafeEncodeURIComponent

baseUrl ∷ String
baseUrl = "https://pursuit.purescript.org/search?q="

fetch ∷ Fetch
fetch = Milkis.fetch nodeFetch
