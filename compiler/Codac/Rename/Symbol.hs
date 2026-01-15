module Codac.Rename.Symbol where

import Data.Map (Map)
import Data.Text (Text)
import Codac.Typing.Ty (Ty)
import Codac.Utils.Span (Span)

data Symbol = Symbol
  { symbolId :: Int,
    symbolSpan :: Span
  }
  deriving (Show, Eq, Ord)
