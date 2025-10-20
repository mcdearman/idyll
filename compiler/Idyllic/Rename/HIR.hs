module Idyllic.Rename.HIR where

import Idyllic.Rename.Symbol (Symbol)
import Idyllic.Utils.Loc (Located)

type NodeId = Int

type Expr = Located ExprKind

data ExprKind
  = ExprInt Int
  | ExprVar Symbol
  | ExprLet Bind Expr
  | ExprIf Expr Expr Expr
  | ExprLam [Symbol] Expr
  | ExprApp Expr [Expr]
  | ExprError
  deriving (Show, Eq, Ord)

data Bind = BindName Symbol Expr | BindFun Symbol [Symbol] Expr deriving (Show, Eq, Ord)