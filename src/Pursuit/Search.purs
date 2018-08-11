module PursBot.Pursuit.Search
  ( Params(..)
  , mkParams
  , search
  , Result
  , Info
  , renderErrors
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Foreign (MultipleErrors, renderForeignError)
import Global.Unsafe (unsafeEncodeURIComponent)
import Milkis (URL(..))
import Prelude.Unicode ((∘), (⊙), (◇))
import PursBot.Format (inlineCode, inlineUrl_)
import PursBot.Http as Http
import PursBot.URI.Query as Query
import Simple.JSON (E, readJSON)

newtype Params = Params
  { query ∷ String
  , page ∷ Int
  , limit ∷ Int
  }

type Info =
  { module ∷ Maybe String
  , title ∷ Maybe String
  , type ∷ Maybe String
  , typeOrValue ∷ Maybe String
  , typeText ∷ Maybe String
  }

type Result =
  { markup ∷ String
  , package ∷ Maybe String
  , text ∷ String
  , url ∷ Maybe String
  , version ∷ Maybe String
  , info ∷ Info
  }

mkParams ∷ String → Params
mkParams query = Params { query, page: 1, limit: 3 }

search ∷ Params → Aff (E (Array String))
search params@(Params { limit }) = do
  response ← Http.get $ mkUrl params
  let results = Array.reverse ⊙ readJSON response
  pure $ rmap (map renderResult ∘ Array.take limit) results

mkUrl ∷ Params → URL
mkUrl = URL ∘ append baseUrl ∘ mkQuery

mkQuery ∷ Params → String
mkQuery (Params { query, page }) =
  Query.mkQuery
    [ Query.mkParam "q" (Just $ unsafeEncodeURIComponent query)
    , Query.mkParam "pages" (Just $ show page)
    ]

baseUrl ∷ String
baseUrl = "https://pursuit.purescript.org/search"

renderResult ∷ Result → String
renderResult r =
  maybe "" inlineCode r.info.title ◇
  maybe "" (append " :: " ∘ inlineCode) r.info.typeText ◇ "\n" ◇
  maybe "" (append "in " ∘ inlineCode) r.info.module ◇ "\n" ◇
  maybe "" (append "of " ∘ inlineCode) r.package ◇
  maybe "" (append " v") r.version ◇ "\n" ◇
  maybe "" (inlineUrl_ ∘ URL) r.url ◇ "\n"

renderErrors ∷ Params → MultipleErrors → String
renderErrors (Params params) errors =
  "Error processing search query : " ◇ show params ◇ "\n" ◇
  Array.intercalate "\n" (renderForeignError ⊙ errors)
