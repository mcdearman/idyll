module Codac.Utils.Located where

import Data.Text (Text)
import Codac.Utils.Span (Span)

data Located a = SynNode
  { synNodeItem :: a,
    synNodeFilename :: Text,
    synNodeSpan :: !Span
  }
  deriving (Show, Eq, Ord)