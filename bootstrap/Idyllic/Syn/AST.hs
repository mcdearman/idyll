module Idyllic.Syn.AST where

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

data Decl
  = DeclClassDecl SWhereDecl
  | DeclRecordDef SRecordDef
  deriving (Show, Eq, Ord)

type SRecordDef = SynNode RecordDef

data RecordDef = RecordDef
  { recordName :: !Ident,
    recordTypeParams :: [Ident],
    recordFields :: [(Ident, STerm)]
  }
  deriving (Show, Eq, Ord)

type SClassDef = SynNode ClassDef

data ClassDef
  = ClassDef
  { className :: !Ident,
    classSuperclasses :: [Ident],
    classTypeParams :: [Ident],
    classDecls :: [SClassDecl]
  }
  deriving (Show, Eq, Ord)

type SClassDecl = SynNode ClassDecl

data ClassDecl
  = ClassDeclSig SSig
  | ClassDeclBind FunBind
  deriving (Show, Eq, Ord)

type SWhereDecl = SynNode WhereDecl

data WhereDecl = WhereDecl
  { whereDeclSig :: SSig,
    whereDeclBind :: SBind
  }
  deriving (Show, Eq, Ord)

type SInstancDecl = SynNode InstanceDecl

data InstanceDecl = InstanceDecl
  { instanceClass :: !Ident,
    instanceTypeParams :: [Ident],
    instanceAnn :: STerm,
    instanceDecls :: [SWhereDecl]
  }
  deriving (Show, Eq, Ord)

type SSig = SynNode Sig

data Sig = Sig
  { sigNames :: [Ident],
    sigAnn :: STerm
  }
  deriving (Show, Eq, Ord)

type STerm = SynNode Term

data Expr
  = Lit Lit
  | Var Ident
  | Let SBind STerm
  | If STerm STerm STerm
  | Lam [SPat] STerm
  | App STerm [STerm]
  | Match STerm [SAlt]
  | Infix STerm Ident STerm
  | Neg STerm
  deriving (Show, Eq, Ord)

data Param = Param
  { paramIdent :: Ident,
    paramType :: STerm,
    paramImplicit :: Bool
  }
  deriving (Show, Eq, Ord)

type SBind = SynNode Bind

data Bind
  = BindPat PatBind
  | BindFun FunBind
  deriving (Show, Eq, Ord)

data Rhs = RhsTerm STerm | RhsGuard [SGuard]
  deriving (Show, Eq, Ord)

data PatBind = PatBind
  { patBindPat :: SPat,
    patBindBody :: Rhs,
    patBindWhereDecls :: [SWhereDecl]
  }
  deriving (Show, Eq, Ord)

data FunBind = FunBind
  { funName :: !Ident,
    funAlts :: [SAlt],
    funWhereDecls :: [SWhereDecl]
  }
  deriving (Show, Eq, Ord)

type SAlt = SynNode Alt

data Alt = Alt
  { altPat :: SPat,
    altBody :: Rhs
  }
  deriving (Show, Eq, Ord)

type SGuard = SynNode Guard

data Guard = Guard
  { guardPat :: SPat,
    guardBody :: STerm
  }
  deriving (Show, Eq, Ord)

type SPat = SynNode Pat

data Pat
  = PatWildcard
  | PatLit Lit
  | PatIdent Ident
  | PatCons Ident [SPat]
  | PatAs Ident SPat
  | PatList [SPat]
  | PatTuple [SPat]
  | PatUnit
  deriving (Show, Eq, Ord)

type Ident = SynNode Text

type SLit = SynNode Lit

data Lit
  = LitInt
  | LitString
  | LitChar
  deriving (Show, Eq, Ord)
