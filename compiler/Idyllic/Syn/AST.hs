module Idyllic.Syn.AST where

import Data.Text (Text)
import Idyllic.Utils.Span (Span)

data SynNode a = SynNode
  { synNodeKind :: a,
    synNodeSpan :: !Span
  }
  deriving (Show, Eq, Ord)

type Expr = SynNode ExprKind

data ExprKind
  = ExprLit Lit
  | ExprVar Ident
  | ExprLet Bind Expr
  | ExprIf Expr Expr Expr
  | ExprLam [Ident] Expr
  | ExprApp Expr [Expr]
  | ExprInfix Expr Ident Expr
  | Neg Expr
  deriving (Show, Eq, Ord)

data Bind = BindName Ident Expr | BindFun Ident [Ident] Expr deriving (Show, Eq, Ord)

type Ident = SynNode Text

data Lit
  = LitInt Int
  | LitBool Bool
  | LitString Text
  deriving (Show, Eq, Ord)