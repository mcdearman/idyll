module Idyllic.Syn.AST where

import Data.Text (Text)
import Idyllic.Utils.Located (Located)

data Module = Module
  { moduleName :: Ident,
    moduleDecls :: [LDecl]
  }
  deriving (Show, Eq, Ord)

type LDecl = Located Decl

data Decl
  = DeclDef LDef
  | DeclRecordDef LRecordDef
  deriving (Show, Eq, Ord)

type LDef = Located Def

data Def = Def
  { defBind :: LBind
  }
  deriving (Show, Eq, Ord)

type LRecordDef = Located RecordDef

data RecordDef = RecordDef
  { recordName :: !Ident,
    recordTypeParams :: [Ident],
    recordFields :: [(Ident, Expr)]
  }
  deriving (Show, Eq, Ord)

type LExpr = Located Expr

data Expr
  = Lit Lit
  | Var Ident
  | Let LBind LExpr
  | If LExpr LExpr LExpr
  | Lam [Pat] LExpr
  | App LExpr [LExpr]
  | Match LExpr [(LPat, LExpr)]
  | Infix LExpr Ident LExpr
  | Prefix LExpr
  deriving (Show, Eq, Ord)

type LBind = Located Bind

data Bind
  = BindPat PatBind
  | BindFun FunBind
  deriving (Show, Eq, Ord)

data PatBind = PatBind LPat LExpr deriving (Show, Eq, Ord)

data FunBind = FunBind Ident [LPat] LExpr deriving (Show, Eq, Ord)

data Guard = Guard
  { guardPat :: LPat,
    guardBody :: LExpr
  }
  deriving (Show, Eq, Ord)

type LPat = Located Pat

data Pat
  = PatWildcard
  | PatLit Lit
  | PatIdent Ident
  | PatCons Ident [LPat]
  | PatAs Ident LPat
  | PatList [LPat]
  | PatTuple [LPat]
  | PatUnit
  deriving (Show, Eq, Ord)

type Ident = Located Text

data Lit
  = LitInt
  | LitString
  | LitChar
  deriving (Show, Eq, Ord)
