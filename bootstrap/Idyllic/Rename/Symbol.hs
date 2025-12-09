module Idyllic.Rename.Symbol where

import Data.Map (Map)
import Data.Text (Text)
import Idyllic.Typing.Ty (Ty)
import Idyllic.Utils.Span (Span)

data Symbol = Symbol
  { symbolId :: Int,
    symbolSpan :: Span
  }
  deriving (Show, Eq, Ord)
