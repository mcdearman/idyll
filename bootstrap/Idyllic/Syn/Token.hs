module Idyllic.Syn.Token
  ( Token (..),
    TokenKind (..),
    isKeyword,
  )
where

import Data.ByteString (ByteString)

data Token = Token {tokenKind :: TokenKind, tokenText :: ByteString}
  deriving (Show, Eq, Ord)

data TokenKind
  = TokenKindError
  | TokenKindUppercaseIdent
  | TokenKindLowercaseIdent
  | TokenKindOpIdent
  | TokenKindConOpIdent
  | TokenKindInt
  | TokenKindString
  | TokenKindChar
  | TokenKindLParen
  | TokenKindRParen
  | TokenKindLBrace
  | TokenKindRBrace
  | TokenKindLBracket
  | TokenKindRBracket
  | TokenKindBang
  | TokenKindHash
  | TokenKindBackSlash
  | TokenKindColon
  | TokenKindSemi
  | TokenKindComma
  | TokenKindPeriod
  | TokenKindEq
  | TokenKindLArrow
  | TokenKindRArrow
  | TokenKindLFatArrow
  | TokenKindBar
  | TokenKindUnderscore
  deriving (Show, Eq, Ord)

isKeyword :: Token -> Bool
isKeyword t = case tokenKind t of
  TokenKindLowercaseIdent -> go $ tokenText t
  _ -> False
  where
    go "use" = True
    go "fun" = True
    go "def" = True
    go "let" = True
    go "in" = True
    go "where" = True
    go "if" = True
    go "then" = True
    go "else" = True
    go "match" = True
    go "with" = True
    go "record" = True
    go "data" = True
    go "type" = True
    go "ref" = True
    go "end" = True
    go _ = False