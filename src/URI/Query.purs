module PursBot.URI.Query
 ( QueryParam
 , mkQuery
 , mkParam
 , print
 ) where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude.Unicode ((∘), (⊙), (⊛))
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value, unsafeKeyFromString, unsafeValueFromString)
import URI.Extra.QueryPairs as NQP
import URI.Query as Query

type QueryParam = Maybe (Tuple Key (Maybe Value))

mkQuery ∷ Array QueryParam → String
mkQuery = print ∘ QueryPairs ∘ catMaybes

print ∷ QueryPairs Key Value → String
print = Query.print ∘ NQP.print identity identity

mkParam ∷ String → Maybe String → QueryParam
mkParam k v =
  Tuple
  ⊙ (const (unsafeKeyFromString k) ⊙ v)
  ⊛ (Just ⊙ unsafeValueFromString ⊙ v)
