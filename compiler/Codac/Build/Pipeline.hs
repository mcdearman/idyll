module Codac.Build.Pipeline (runPipelineIO) where

import Codac.Build.Pipeline.Effect
import Codac.Syn.Lexer (tokenize)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Debug.Trace (trace)
import Text.Pretty.Simple

pipelineMain :: Pipeline ()
pipelineMain = do
  env <- ask
  let src = pipelineSrc env
  let ts = tokenize (BL.fromStrict src)
  trace "tokens" $ pPrint ts
  -- lts <- runLayout (filter (not . tokenIsSpace) ts)
  pure ()

runPipelineIO :: Bool -> InputMode -> ByteString -> IO ()
runPipelineIO debug mode src = do
  env <- mkPipelineEnv debug mode src
  runReaderT pipelineMain env