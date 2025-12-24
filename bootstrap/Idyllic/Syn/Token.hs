module Idyllic.Syn.Token
  ( Token (..),
    TokenKind (..),
    isTrivia,
    isSpace,
    isKeyword,
  )
where

import Data.ByteString (ByteString)

data Token = Token {tokenKind :: TokenKind, tokenText :: ByteString}
  deriving (Show, Eq, Ord)

data TokenKind
  = TokenKindError
  | TokenKindTab
  | TokenKindNewline
  | TokenKindWhitespace
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

isTrivia :: Token -> Bool
isTrivia = go . tokenKind
  where
    go TokenKindWhitespace = True
    go TokenKindNewline = True
    go TokenKindTab = True
    go _ = False

isSpace :: Token -> Bool
isSpace = go . tokenKind
  where
    go TokenKindWhitespace = True
    go _ = False

isKeyword :: Token -> Bool
isKeyword t = case tokenKind t of
  TokenKindLowercaseIdent -> go $ tokenText t
  _ -> False
  where
    go "use" = True
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
    go _ = False