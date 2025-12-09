module Idyllic.Syn.Layout (layout) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Idyllic.Syn.Token
-- Assuming Span constructor is available
import Idyllic.Utils.LineIndex (LineIndex, offsetToLineCol)
import Idyllic.Utils.Span (Span (..))

layout :: LineIndex -> ByteString -> [Token] -> [Token]
layout li src tokens = tokens

-- layout src tokens = go tokens [0] False
--   where
--     -- Helper to create synthetic tokens
--     makeToken :: TokenKind -> Span -> Token
--     makeToken k s = Token k s

--     go :: [Token] -> [Int] -> Bool -> [Token]

--     -- 1. End of File: Close all open blocks
--     go [] (0 : []) _ = []
--     go [] (m : ms) _ = makeToken TokenKindRBrace (Span 0 0) : go [] ms False
--     go [] [] _ = [] -- Should not happen if stack starts with [0]

--     -- 2. Trivia: Pass through, but track Newlines
--     go (t : ts) stack nl
--       | tokenKind t == TokenKindNewline = go ts stack True -- Note the newline
--       | isTrivia t = go ts stack nl -- Skip other trivia (comments/spaces)

--     -- 3. The "Trigger" Case: Keyword + Colon (e.g., "let" + ":")
--     -- We match the keyword, the colon, and the *next* real token to set indentation.
--     go (tKw : tCol : ts) stack nl
--       | isLayoutKeyword tKw src && tokenKind tCol == TokenKindColon =
--           let -- We need to find the next meaningful token to determine indentation
--               (trivia, nextReal : rest) = break (not . isTrivia) ts
--               indentCol = getColumn src nextReal

--               -- Create the LBrace
--               lBrace = makeToken TokenKindLBrace (tokenSpan tCol)
--            in -- The Keyword is emitted, the Colon is consumed/replaced by LBrace

--               -- Logic: Emit Keyword -> Emit LBrace -> Recurse
--               -- We Push the new indentation level to the stack
--               tKw : lBrace : trivia ++ go (nextReal : rest) (indentCol : stack) False
--     -- 4. Standard Token Processing
--     go (t : ts) stack@(top : rest) nl
--       | nl -- We just came from a newline, check indentation
--         =
--           let col = getColumn src t
--            in case compare col top of
--                 EQ ->
--                   -- Same level: Insert semicolon
--                   makeToken TokenKindSemi (tokenSpan t) : t : go ts stack False
--                 GT ->
--                   -- Indented: Just continue (it's a continuation line)
--                   t : go ts stack False
--                 LT ->
--                   -- Dedented: Close block(s)
--                   -- We emit RBrace and RECURSE on the SAME token 't'
--                   -- to see if we need to close more blocks or insert a semi.
--                   makeToken TokenKindRBrace (tokenSpan t) : go (t : ts) rest True
--       | otherwise -- No newline, just emit
--         =
--           t : go ts stack False

getColumn :: LineIndex -> Token -> Int
getColumn li tok = snd $ offsetToLineCol li startOffset
  where
    startOffset = spanStart (tokenSpan tok)