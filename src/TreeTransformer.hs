module TreeTransformer where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO          ( hPutStrLn, stderr, hPutStr )
import Control.Monad      ( when )

import qualified Latte.Abs as Abs
import Latte.Lex   ( Token, mkPosToken )
import Latte.Par   ( pProgram, myLexer )
import Latte.Print ( Print, printTree )
import Latte.Skel  ()

import Data.Map (Map, empty, fromList, union, member, lookup, insert, toList, keys, difference, intersection, elems, (!), intersectionWith)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List

import qualified AST

transformProgram :: Abs.Program -> AST.Program
transformProgram (Abs.PProgram _ topDefs) = AST.PProgram $ map transformTopDef topDefs

transformTopDef :: Abs.TopDef -> AST.TopDef
transformTopDef (Abs.PFunDef _ t ident args block) = AST.PFunDef (transformType t) (transformIIdent ident) (map transformArg args) (transformBlock block)
transformTopDef (Abs.PClassDef _ ident classDef) = AST.PClassDef (transformIIdent ident) (transformClassDef classDef)

transformArg :: Abs.Arg -> AST.Arg
transformArg (Abs.PArg _ t ident) = AST.PArg (transformType t) (transformIIdent ident)

transformClassDef :: Abs.ClassDef -> AST.ClassDef
transformClassDef (Abs.ClassDef _ classElems) = AST.ClassDef $ map transformClassElem classElems

transformClassElem :: Abs.ClassElem -> AST.ClassElem
transformClassElem (Abs.ClassAttrDef _ t classItems) = AST.ClassAttrDef (transformType t) (map transformClassItem classItems)

transformClassItem :: Abs.ClassItem -> AST.ClassItem
transformClassItem (Abs.ClassItem _ ident) = AST.ClassItem (transformIIdent ident)

transformBlock :: Abs.Block -> AST.Block
transformBlock (Abs.SBlock _ stmts) = AST.SBlock $ map transformStmt stmts

transformStmt :: Abs.Stmt -> AST.Stmt
transformStmt (Abs.SEmpty _) = AST.SEmpty
transformStmt (Abs.SBStmt _ block) = AST.SBStmt (transformBlock block)
transformStmt (Abs.SDecl _ t items) = AST.SDecl (transformType t) (map transformItem items)
transformStmt (Abs.SAss _ expr1 expr2) = AST.SAss (transformExpr expr1) (transformExpr expr2)
transformStmt (Abs.SIncr _ expr) = AST.SIncr (transformExpr expr)
transformStmt (Abs.SDecr _ expr) = AST.SDecr (transformExpr expr)
transformStmt (Abs.SRet _ expr) = AST.SRet (transformExpr expr)
transformStmt (Abs.SVRet _) = AST.SVRet
transformStmt (Abs.SCond _ expr stmt) = AST.SCond (transformExpr expr) (transformStmt stmt)
transformStmt (Abs.SCondElse _ expr stmt1 stmt2) = AST.SCondElse (transformExpr expr) (transformStmt stmt1) (transformStmt stmt2)
transformStmt (Abs.SWhile _ expr stmt) = AST.SWhile (transformExpr expr) (transformStmt stmt)
transformStmt (Abs.SFor _ t ident expr stmt) = AST.SFor (transformType t) (transformIIdent ident) (transformExpr expr) (transformStmt stmt)
transformStmt (Abs.SExp _ expr) = AST.SExp (transformExpr expr)

transformItem :: Abs.Item -> AST.Item
transformItem (Abs.SNoInit _ ident) = AST.SNoInit (transformIIdent ident)
transformItem (Abs.SInit _ ident expr) = AST.SInit (transformIIdent ident) (transformExpr expr)

transformType :: Abs.Type -> AST.Type
transformType (Abs.TInt _) = AST.TInt
transformType (Abs.TStr _) = AST.TStr
transformType (Abs.TBool _) = AST.TBool
transformType (Abs.TVoid _) = AST.TVoid
transformType (Abs.TArray _ t) = AST.TArray (transformType t)
transformType (Abs.TClass _ ident) = AST.TClass (transformIIdent ident)

transformExpr :: Abs.Expr -> AST.Expr
transformExpr (Abs.EVar _ ident) = AST.EVar (transformIIdent ident)
transformExpr (Abs.ELitInt _ integer) = AST.ELitInt integer
transformExpr (Abs.ELitTrue _) = AST.ELitTrue
transformExpr (Abs.ELitFalse _) = AST.ELitFalse
transformExpr (Abs.ECastNull _ t) = AST.ECastNull (transformType t)
transformExpr (Abs.EString _ string) = AST.EString string
transformExpr (Abs.EFunctionCall _ ident exprs) = AST.EFunctionCall (transformIIdent ident) (map transformExpr exprs)
transformExpr (Abs.EClassNew _ ident) = AST.EClassNew (transformIIdent ident)
transformExpr (Abs.EClassAttr _ expr ident) = AST.EClassAttr (transformExpr expr) (transformIIdent ident)
transformExpr (Abs.EMethodCall _ expr ident exprs) = AST.EMethodCall (transformExpr expr) (transformIIdent ident) (map transformExpr exprs)
transformExpr (Abs.EArrayNew _ t expr) = AST.EArrayNew (transformType t) (transformExpr expr)
transformExpr (Abs.EArrayElem _ expr expr') = AST.EArrayElem (transformExpr expr) (transformExpr expr')

transformIIdent :: Abs.IIdent -> AST.Ident
transformIIdent (Abs.IIdent _ (Abs.Ident ident)) = AST.Ident ident
