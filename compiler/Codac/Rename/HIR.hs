module Codac.Rename.HIR where

import Data.Text (Text)
import Codac.Rename.Symbol (Symbol)
import Codac.Utils.Span (Span)

type NodeId = Int

type Expr = HirNode ExprKind

data HirNode a = HirNode
  { nodeId :: !NodeId,
    nodeKind :: a,
    nodeSpan :: !Span
  }
  deriving (Show, Eq, Ord)

data ExprKind
  = ExprLit Lit
  | ExprVar Symbol
  | ExprLet Bind Expr
  | ExprIf Expr Expr Expr
  | ExprLam [Symbol] Expr
  | ExprApp Expr [Expr]
  | ExprError
  deriving (Show, Eq, Ord)

data Bind = BindName Symbol Expr | BindFun Symbol [Symbol] Expr deriving (Show, Eq, Ord)

data Lit
  = LitInt Int
  | LitBool Bool
  | LitString Text
  deriving (Show, Eq, Ord)