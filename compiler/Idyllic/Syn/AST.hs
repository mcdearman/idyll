module Idyllic.Syn.AST where

import Data.Text (Text)
import Idyllic.Utils.Loc (Located)

type Expr = Located ExprKind

data ExprKind
  = ExprInt Int
  | ExprVar Ident
  | ExprLet Bind Expr
  | ExprIf Expr Expr Expr
  | ExprLam [Ident] Expr
  | ExprApp Expr [Expr]
  deriving (Show, Eq, Ord)

data Bind = BindName Ident Expr | BindFun Ident [Ident] Expr deriving (Show, Eq, Ord)

type Ident = Located Text
