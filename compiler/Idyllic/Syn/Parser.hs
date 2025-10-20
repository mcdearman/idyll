module Idyllic.Syn.Parser where

import Data.Text (Text, pack, unpack)
import Data.Void
import Idyllic.Syn.AST
import Idyllic.Utils.Span (Span (..), Spanned (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseTerm :: Text -> Either (ParseErrorBundle Text Void) Expr
parseTerm = runParser (sc *> exprParser <* eof) ""

exprParser :: Parser Expr
exprParser = app <|> atom
  where
    int = ExprInt . fromInteger <$> lexeme L.decimal
    var = ExprVar <$> ident
    atom = withSpan $ choice [int, let', if', var, lam, synNodeKind <$> parens exprParser]
    let' = try $ ExprLet <$> (symbol "let" *> bind) <*> (symbol "in" *> exprParser)
    if' = try $ ExprIf <$> (symbol "if" *> exprParser) <*> (symbol "then" *> exprParser) <*> (symbol "else" *> exprParser)
    lam = ExprLam <$> (symbol "\\" *> some ident) <*> (symbol "->" *> exprParser)
    app = try $ withSpan $ ExprApp <$> atom <*> some atom

bind :: Parser Bind
bind = funBind <|> nameBind
  where
    nameBind = BindName <$> (ident <* symbol "=") <*> exprParser
    funBind = try $ BindFun <$> ident <*> some ident <* symbol "=" <*> exprParser

parens :: Parser a -> Parser a
parens = lexeme <$> between (char '(') (char ')')

ident :: Parser Ident
ident = try $ withSpan $ lexeme $ do
  name <- pack <$> ((:) <$> identStartChar <*> many identChar) <* sc
  if name `elem` keywords
    then fail $ "keyword " ++ unpack name ++ " cannot be used in place of identifier"
    else pure name
  where
    identStartChar = lowerChar <|> char '_'
    identChar = alphaNumChar <|> char '_' <|> char '\''

    keywords :: [Text]
    keywords =
      [ "module",
        "import",
        "as",
        "let",
        "in",
        "where",
        "if",
        "then",
        "else",
        "match",
        "with",
        "record",
        "data",
        "type",
        "class",
        "instance",
        "do"
      ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

withSpan :: Parser a -> Parser (SynNode a)
withSpan p = do
  start <- getOffset
  x <- p
  SynNode x . Span start <$> getOffset