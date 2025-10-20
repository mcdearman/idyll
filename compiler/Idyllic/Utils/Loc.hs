module Idyllic.Utils.Loc (Loc (..), toPair, slice, Located (..)) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

data Loc = Loc
  { start :: {-# UNPACK #-} !Int,
    end :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Ord)

toPair :: Loc -> (Int, Int)
toPair (Loc s e) = (s, e)

slice :: Loc -> ByteString -> ByteString
slice (Loc s e) bs = BS.take (e - s) (BS.drop s bs)

-- instance Pretty Span where
--   pretty (Loc s e) = pack $ show s <> ".." <> show e

instance Semigroup Loc where
  Loc s1 e1 <> Loc s2 e2 = Loc (min s1 s2) (max e1 e2)

data Located a = Located {unLoc :: a, locOf :: Loc}
  deriving (Show, Eq, Ord)

instance Functor Located where
  fmap f (Located v s) = Located (f v) s
