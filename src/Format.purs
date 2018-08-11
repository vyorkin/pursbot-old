module PursBot.Format where

import Prelude
import Milkis (URL(..))
import Prelude.Unicode ((◇))

bold ∷ String → String
bold s = "*" ◇ s ◇ "*"

italic ∷ String → String
italic s = "_" ◇ s ◇ "_"

code ∷ String → String
code s = "```purescript\n" ◇ s ◇ "\n```"

inlineCode ∷ String → String
inlineCode s = "`" ◇ s ◇ "`"

inlineUrl_ ∷ URL → String
inlineUrl_ = flip inlineUrl ""

inlineUrl ∷ URL → String → String
inlineUrl (URL url) title = "[" ◇ url ◇ "]" ◇ "(" ◇ url ◇ ")"
