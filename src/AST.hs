module AST where

import Prelude
import qualified Data.String

newtype Program = PProgram [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef
    = PFunDef Type Ident [Arg] Block
    | PClassDef Ident ClassDef
    | PClassDefExt Ident Ident ClassDef
  deriving (Eq, Ord, Show, Read)

data Arg = PArg Type Ident
  deriving (Eq, Ord, Show, Read)

newtype ClassDef = ClassDef [ClassElem]
  deriving (Eq, Ord, Show, Read)

data ClassElem
    = ClassAttrDef Type [ClassItem]
    | ClassMethodDef Type Ident [Arg] Block
  deriving (Eq, Ord, Show, Read)

newtype ClassItem = ClassItem Ident
  deriving (Eq, Ord, Show, Read)

newtype Block = SBlock [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SEmpty
    | SBStmt Block
    | SDecl Type [Item]
    | SAss Expr Expr
    | SIncr Expr
    | SDecr Expr
    | SRet Expr
    | SVRet
    | SCond Expr Stmt
    | SCondElse Expr Stmt Stmt
    | SWhile Expr Stmt
    | SFor Type Ident Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item
    = SNoInit Ident | SInit Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = TInt
    | TStr
    | TBool
    | TVoid
    | TArray Type
    | TClass Ident
    | TFun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EString String
    | ECastNull Type
    | EArrayElem Expr Expr
    | EClassAttr Expr Ident
    | EMethodCall Expr Ident [Expr]
    | EFunctionCall Ident [Expr]
    | EArrayNew Type Expr
    | EClassNew Ident
    | ENeg Expr
    | ENot Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = OPlus | OMinus
  deriving (Eq, Ord, Show, Read)

data MulOp = OTimes | ODiv | OMod
  deriving (Eq, Ord, Show, Read)

data RelOp = OLTH | OLE | OGTH | OGE | OEQU | ONE
  deriving (Eq, Ord, Show, Read)

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read, Data.String.IsString)