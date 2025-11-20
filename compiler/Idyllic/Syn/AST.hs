module Idyllic.Syn.AST
  ( SynNode (..),
    Module (..),
    SDecl,
    Decl (..),
    STerm,
    Term (..),
    Universe (..),
    Binder (..),
    Bind (..),
    Ident,
    Lit (..),
  )
where

import Data.Text (Text)
import Idyllic.Utils.Span (Span)

data SynNode a = SynNode
  { synNodeKind :: a,
    synNodeSpan :: !Span
  }
  deriving (Show, Eq, Ord)

data Module = Module
  { moduleName :: Ident,
    moduleDecls :: [SDecl]
  }
  deriving (Show, Eq, Ord)

type SDecl = SynNode Decl

data Decl = Decl
  { declName :: Ident,
    declType :: Maybe STerm,
    declBody :: STerm
  }
  deriving (Show, Eq, Ord)

type STerm = SynNode Term

data Term
  = Lit Lit
  | Var Ident
  | Let Bind STerm
  | If STerm STerm STerm
  | Lam [Ident] STerm
  | App STerm [STerm]
  | Infix STerm Ident STerm
  | Neg STerm
  | Sort Universe -- Type, Prop, Sort u
  | Pi [Binder] STerm -- (x : A) -> B, {x : A} -> B, A -> B
  | Ann STerm STerm -- e : A  (type annotation)
  | Hole (Maybe Ident) -- _ or ?name for user holes
  deriving (Show, Eq, Ord)

data Universe = UType | UProp | USort Int deriving (Show, Eq, Ord)

data Binder = Binder
  { binderIdent :: Ident,
    binderType :: STerm,
    binderImplicit :: Bool
  }
  deriving (Show, Eq, Ord)

data Bind = BindName Ident STerm | BindFun Ident [Ident] STerm deriving (Show, Eq, Ord)

type Ident = SynNode Text

data Lit
  = LitInt Int
  | LitBool Bool
  | LitString Text
  deriving (Show, Eq, Ord)
