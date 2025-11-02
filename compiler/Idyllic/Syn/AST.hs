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
  = Lit Lit
  | Var Ident
  | Let Bind Expr
  | If Expr Expr Expr
  | Lam [Ident] Expr
  | App Expr [Expr]
  | Infix Expr Ident Expr
  | Neg Expr
  deriving (Show, Eq, Ord)

data Bind = BindName Ident Expr | BindFun Ident [Ident] Expr deriving (Show, Eq, Ord)

type Ident = SynNode Text

data Lit
  = LitInt Int
  | LitBool Bool
  | LitString Text
  deriving (Show, Eq, Ord)