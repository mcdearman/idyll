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
  | Sort Universe -- Type, Prop, Sort u
  | Pi [Binder] Expr -- (x : A) -> B, {x : A} -> B, A -> B
  | Ann Expr Expr -- e : A  (type annotation)
  | Hole (Maybe Ident) -- _ or ?name for user holes
  deriving (Show, Eq, Ord)

data Universe = UType | UProp | USort Int deriving (Show, Eq, Ord)

data Binder = Binder
  { binderIdent :: Ident,
    binderType :: Expr,
    binderImplicit :: Bool
  }
  deriving (Show, Eq, Ord)

data Bind = BindName Ident Expr | BindFun Ident [Ident] Expr deriving (Show, Eq, Ord)

type Ident = SynNode Text

data Lit
  = LitInt Int
  | LitBool Bool
  | LitString Text
  deriving (Show, Eq, Ord)
