{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Idyllic.Build.Driver (runDefaultDriver, runDriver) where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (runReaderT))
import Data.ByteString (ByteString)
import Idyllic.Build.Pipeline (runPipeline)
import Idyllic.Build.Pipeline.Effect (PipelineEnv)
import System.Console.Haskeline (InputT, Settings (..), defaultSettings, getInputLine, runInputT)

newtype Driver a = Driver {unDriver :: ReaderT DriverEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DriverEnv)

data DriverEnv = DriverEnv
  { driverArgs :: CliOptions,
    driverPaths :: SearchPaths,
    driverMode :: DriverMode
  }

data CliOptions = CliOptions
  { cliVerbose :: Bool
  }

data SearchPaths = SearchPaths
  { spSourcePaths :: [FilePath],
    spLibraryPaths :: [FilePath]
  }

data DriverMode
  = DriverModeFile FilePath
  | DriverModeREPL
  deriving (Show, Eq)

runDefaultDriver :: IO ()
runDefaultDriver = do
  undefined

runDriver :: DriverEnv -> Driver a -> IO a
runDriver env (Driver m) = runReaderT m env

-- runDriver :: IO ()
-- runDriver = do
--   env <- defaultPipelineEnv ""
--   runInputT settings (repl env)

repl :: PipelineEnv -> InputT IO ()
repl env = do
  -- putStrLn "Welcome to the Idyll REPL!"
  minput <- getMultilineInput ""
  case minput of
    Nothing -> return ()
    Just input -> do
      repl env

settings :: Settings IO
settings = defaultSettings {historyFile = Just ".repl_history"}

getMultilineInput :: String -> InputT IO (Maybe String)
getMultilineInput acc = do
  firstLine <- getInputLine "> "
  case firstLine of
    Nothing -> pure Nothing
    Just fl -> collectLines (acc ++ fl ++ "\n")

collectLines :: String -> InputT IO (Maybe String)
collectLines acc = do
  minput <- getInputLine ""
  case minput of
    Nothing -> pure Nothing
    Just "" -> pure $ Just (init acc)
    Just input -> collectLines (acc ++ input ++ "\n")

-- let (out, _) = runState (runPipeline (InputModeFile "main") (pack src)) defaultPipelineEnv
-- case out of
--   Left e ->
--     let diag = errorDiagnosticFromBundle Nothing ("Parse error on input" :: Text) Nothing e
--         diag' = addFile diag "main" src
--      in printDiagnostic stderr True True 2 defaultStyle diag'
--   Right prog -> putStrLn $ unpack . toStrict $ pShow prog