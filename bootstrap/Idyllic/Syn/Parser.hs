module Idyllic.Syn.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Text (Text, pack, unpack)
import Data.Void
import Idyllic.Syn.AST
import Idyllic.Utils.Span (Span (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- parseTerm :: Text -> Either (ParseErrorBundle Text Void) Expr
-- parseTerm = runParser (sc *> exprParser <* eof) ""

-- exprParser :: Parser Expr
-- exprParser = makeExprParser (app <|> atom) operatorTable
--   where
--     lit' = Lit <$> lit
--     var = Var <$> ident
--     atom = withSpan $ choice [lit', let', if', var, lam, synNodeKind <$> parens exprParser]
--     let' = try $ Let <$> (symbol "let" *> bind) <*> (symbol "in" *> exprParser)
--     if' = try $ If <$> (symbol "if" *> exprParser) <*> (symbol "then" *> exprParser) <*> (symbol "else" *> exprParser)
--     lam = Lam <$> (symbol "\\" *> some ident) <*> (symbol "->" *> exprParser)
--     app = try $ withSpan $ App <$> atom <*> some atom

-- lit :: Parser Lit
-- lit = choice [LitInt . fromInteger <$> lexeme L.decimal, boolLit, stringLit]
--   where
--     boolLit = (LitBool True <$ symbol "True") <|> (LitBool False <$ symbol "False")
--     stringLit = LitString . pack <$> between (char '"') (char '"') (manyTill L.charLiteral (lookAhead (char '"')))

-- bind :: Parser Bind
-- bind = funBind <|> nameBind
--   where
--     nameBind = BindName <$> (ident <* symbol "=") <*> exprParser
--     funBind = try $ BindFun <$> ident <*> some ident <* symbol "=" <*> exprParser

-- operatorTable :: [[Operator Parser Expr]]
-- operatorTable =
--   [ [prefix "-" (\s e -> SynNode (Neg e) s)],
--     [ binary "*" (\s l r -> SynNode (Infix l (SynNode "*" s) r) (synNodeSpan l <> synNodeSpan r)),
--       binary "/" (\s l r -> SynNode (Infix l (SynNode "/" s) r) (synNodeSpan l <> synNodeSpan r)),
--       binary "%" (\s l r -> SynNode (Infix l (SynNode "%" s) r) (synNodeSpan l <> synNodeSpan r))
--     ],
--     [ binary "+" (\s l r -> SynNode (Infix l (SynNode "+" s) r) (synNodeSpan l <> synNodeSpan r)),
--       binary "-" (\s l r -> SynNode (Infix l (SynNode "-" s) r) (synNodeSpan l <> synNodeSpan r))
--     ]
--   ]

-- binary :: Text -> (Span -> Expr -> Expr -> Expr) -> Operator Parser Expr
-- binary name f = InfixL (f . synNodeSpan <$> withSpan (symbol name))

-- prefix, postfix :: Text -> (Span -> Expr -> Expr) -> Operator Parser Expr
-- prefix name f = Prefix (f . synNodeSpan <$> withSpan (symbol name))
-- postfix name f = Postfix (f . synNodeSpan <$> withSpan (symbol name))

-- parens :: Parser a -> Parser a
-- parens = lexeme <$> between (char '(') (char ')')

-- ident :: Parser Ident
-- ident = try $ withSpan $ lexeme $ do
--   name <- pack <$> ((:) <$> identStartChar <*> many identChar) <* sc
--   if name `elem` keywords
--     then fail $ "keyword " ++ unpack name ++ " cannot be used in place of identifier"
--     else pure name
--   where
--     identStartChar = lowerChar <|> char '_'
--     identChar = alphaNumChar <|> char '_' <|> char '\''

--     keywords :: [Text]
--     keywords =
--       [ "module",
--         "import",
--         "as",
--         "let",
--         "in",
--         "where",
--         "if",
--         "then",
--         "else",
--         "match",
--         "with",
--         "record",
--         "data",
--         "type",
--         "class",
--         "instance",
--         "do"
--       ]

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme sc

-- symbol :: Text -> Parser Text
-- symbol = L.symbol sc

-- sc :: Parser ()
-- sc = L.space space1 lineComment empty

-- lineComment :: Parser ()
-- lineComment = L.skipLineComment "--"

-- withSpan :: Parser a -> Parser (SynNode a)
-- withSpan p = do
--   start <- getOffset
--   x <- p
--   SynNode x . Span start <$> getOffset