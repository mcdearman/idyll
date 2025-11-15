module Main where

import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Idyllic.Build.Driver (runDriver)
import Idyllic.Build.Effect (defaultPipelineEnv)
import Idyllic.Build.Pipeline
import Idyllic.Rename.Resolver (rename)
import Idyllic.Syn.Parser (parseTerm)
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pPrint, pShow)

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

repl :: InputT IO ()
repl = do
  minput <- getMultilineInput ""
  case minput of
    Nothing -> return ()
    Just input -> do
      case parseTerm (pack input) of
        Left err -> outputStrLn $ errorBundlePretty err
        Right expr -> do
          pPrint expr
          pPrint $ rename expr
      repl

run :: ByteString -> IO ()
run src = do
  env <- defaultPipelineEnv src
  out <- runDriver env src
  putStrLn . unpack . toStrict $ pShow out

-- let (out, _) = runState (runPipeline (InputModeFile "main") (pack src)) defaultPipelineEnv
-- case out of
--   Left e ->
--     let diag = errorDiagnosticFromBundle Nothing ("Parse error on input" :: Text) Nothing e
--         diag' = addFile diag "main" src
--      in printDiagnostic stderr True True 2 defaultStyle diag'
--   Right prog -> putStrLn $ unpack . toStrict $ pShow prog

main :: IO ()
main = do
  putStrLn "Welcome to the Idyll REPL!"
  runInputT settings repl
