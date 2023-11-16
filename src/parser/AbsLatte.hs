-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language Latte.

module AbsLatte where

import Prelude (Integer, String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Int, Maybe(..)
  )
import qualified Data.String

type Program = Program' BNFC'Position
data Program' a = PProgram a [TopDef' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TopDef = TopDef' BNFC'Position
data TopDef' a = PFunDef a (Type' a) Ident [Arg' a] (Block' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Arg = Arg' BNFC'Position
data Arg' a = PArg a (Type' a) Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type ArrayElem = ArrayElem' BNFC'Position
data ArrayElem' a = ArrayElem a (Lvalue' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type ClassAttr = ClassAttr' BNFC'Position
data ClassAttr' a = ClassAttr a (Lvalue' a) Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MethodCall = MethodCall' BNFC'Position
data MethodCall' a = MethodCall a (Lvalue' a) Ident [Expr' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type FunctionCall = FunctionCall' BNFC'Position
data FunctionCall' a = FunctionCall a Ident [Expr' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Lvalue = Lvalue' BNFC'Position
data Lvalue' a
    = LIdent a Ident
    | LArrayElem a (ArrayElem' a)
    | LClassAttr a (ClassAttr' a)
    | LMethodCall a (MethodCall' a)
    | LFuntionCall a (FunctionCall' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Block = Block' BNFC'Position
data Block' a = SBlock a [Stmt' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Stmt = Stmt' BNFC'Position
data Stmt' a
    = SEmpty a
    | SBStmt a (Block' a)
    | SDecl a (Type' a) [Item' a]
    | SAss a (Lvalue' a) (Expr' a)
    | SIncr a (Lvalue' a)
    | SDecr a (Lvalue' a)
    | SRet a (Expr' a)
    | SVRet a
    | SCond a (Expr' a) (Stmt' a)
    | SCondElse a (Expr' a) (Stmt' a) (Stmt' a)
    | SWhile a (Expr' a) (Stmt' a)
    | SExp a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Item = Item' BNFC'Position
data Item' a = SNoInit a Ident | SInit a Ident (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Type = Type' BNFC'Position
data Type' a
    = TInt a
    | TStr a
    | TBool a
    | TVoid a
    | TArray a (Type' a)
    | TClass a Ident
    | TFun a (Type' a) [Type' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Expr = Expr' BNFC'Position
data Expr' a
    = EVar a Ident
    | ELitInt a Integer
    | ELitTrue a
    | ELitFalse a
    | EString a String
    | ECastNull a (Type' a)
    | EArrayElem a (ArrayElem' a)
    | EClassNew a Ident
    | EClassAttr a (ClassAttr' a)
    | EMethodCall a (MethodCall' a)
    | EFuntionCall a (FunctionCall' a)
    | ENeg a (Expr' a)
    | ENot a (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | EAdd a (Expr' a) (AddOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EAnd a (Expr' a) (Expr' a)
    | EOr a (Expr' a) (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type AddOp = AddOp' BNFC'Position
data AddOp' a = OPlus a | OMinus a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MulOp = MulOp' BNFC'Position
data MulOp' a = OTimes a | ODiv a | OMod a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type RelOp = RelOp' BNFC'Position
data RelOp' a = OLTH a | OLE a | OGTH a | OGE a | OEQU a | ONE a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition Program where
  hasPosition = \case
    PProgram p _ -> p

instance HasPosition TopDef where
  hasPosition = \case
    PFunDef p _ _ _ _ -> p

instance HasPosition Arg where
  hasPosition = \case
    PArg p _ _ -> p

instance HasPosition ArrayElem where
  hasPosition = \case
    ArrayElem p _ _ -> p

instance HasPosition ClassAttr where
  hasPosition = \case
    ClassAttr p _ _ -> p

instance HasPosition MethodCall where
  hasPosition = \case
    MethodCall p _ _ _ -> p

instance HasPosition FunctionCall where
  hasPosition = \case
    FunctionCall p _ _ -> p

instance HasPosition Lvalue where
  hasPosition = \case
    LIdent p _ -> p
    LArrayElem p _ -> p
    LClassAttr p _ -> p
    LMethodCall p _ -> p
    LFuntionCall p _ -> p

instance HasPosition Block where
  hasPosition = \case
    SBlock p _ -> p

instance HasPosition Stmt where
  hasPosition = \case
    SEmpty p -> p
    SBStmt p _ -> p
    SDecl p _ _ -> p
    SAss p _ _ -> p
    SIncr p _ -> p
    SDecr p _ -> p
    SRet p _ -> p
    SVRet p -> p
    SCond p _ _ -> p
    SCondElse p _ _ _ -> p
    SWhile p _ _ -> p
    SExp p _ -> p

instance HasPosition Item where
  hasPosition = \case
    SNoInit p _ -> p
    SInit p _ _ -> p

instance HasPosition Type where
  hasPosition = \case
    TInt p -> p
    TStr p -> p
    TBool p -> p
    TVoid p -> p
    TArray p _ -> p
    TClass p _ -> p
    TFun p _ _ -> p

instance HasPosition Expr where
  hasPosition = \case
    EVar p _ -> p
    ELitInt p _ -> p
    ELitTrue p -> p
    ELitFalse p -> p
    EString p _ -> p
    ECastNull p _ -> p
    EArrayElem p _ -> p
    EClassNew p _ -> p
    EClassAttr p _ -> p
    EMethodCall p _ -> p
    EFuntionCall p _ -> p
    ENeg p _ -> p
    ENot p _ -> p
    EMul p _ _ _ -> p
    EAdd p _ _ _ -> p
    ERel p _ _ _ -> p
    EAnd p _ _ -> p
    EOr p _ _ -> p

instance HasPosition AddOp where
  hasPosition = \case
    OPlus p -> p
    OMinus p -> p

instance HasPosition MulOp where
  hasPosition = \case
    OTimes p -> p
    ODiv p -> p
    OMod p -> p

instance HasPosition RelOp where
  hasPosition = \case
    OLTH p -> p
    OLE p -> p
    OGTH p -> p
    OGE p -> p
    OEQU p -> p
    ONE p -> p
