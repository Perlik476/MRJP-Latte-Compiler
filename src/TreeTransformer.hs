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
transformBlock (Abs.SBlock _ stmts) = AST.SBlock $ transformStmts stmts

transformStmt' :: Abs.Stmt -> AST.Stmt
transformStmt' (Abs.SEmpty _) = AST.SEmpty
transformStmt' (Abs.SBStmt _ block) = AST.SBStmt (transformBlock block)
transformStmt' (Abs.SDecl _ t _) = error "Multiple items in declaration"
transformStmt' (Abs.SAss _ expr1 expr2) = AST.SAss (transformExpr expr1) (transformExpr expr2)
transformStmt' (Abs.SIncr _ expr) = AST.SIncr (transformExpr expr)
transformStmt' (Abs.SDecr _ expr) = AST.SDecr (transformExpr expr)
transformStmt' (Abs.SRet _ expr) = AST.SRet (transformExpr expr)
transformStmt' (Abs.SVRet _) = AST.SVRet
transformStmt' (Abs.SCond _ expr stmt) = AST.SCond (transformExpr expr) (transformStmt' stmt)
transformStmt' (Abs.SCondElse _ expr stmt1 stmt2) = AST.SCondElse (transformExpr expr) (transformStmt' stmt1) (transformStmt' stmt2)
transformStmt' (Abs.SWhile _ expr stmt) = AST.SWhile (transformExpr expr) (transformStmt' stmt)
transformStmt' (Abs.SFor _ t ident expr stmt) = AST.SFor (transformType t) (transformIIdent ident) (transformExpr expr) (transformStmt' stmt)
transformStmt' (Abs.SExp _ expr) = AST.SExp (transformExpr expr)

transformStmts :: [Abs.Stmt] -> [AST.Stmt]
transformStmts (Abs.SDecl _ t (item:items):stmts) = 
  case item of
    Abs.SNoInit _ (Abs.IIdent _ (Abs.Ident ident)) -> AST.SDecl (transformType t) ident : transformStmts (Abs.SDecl undefined t items : stmts)
    Abs.SInit _ (Abs.IIdent _ (Abs.Ident ident)) val -> 
      AST.SDecl (transformType t) ident : 
      transformStmt' (Abs.SAss undefined (Abs.EVar undefined (Abs.IIdent undefined (Abs.Ident ident))) val) :
      transformStmts (Abs.SDecl undefined t items : stmts)
transformStmts (Abs.SDecl _ t []:stmts) = transformStmts stmts
transformStmts (Abs.SRet _ expr:stmts) = [transformStmt' (Abs.SRet undefined expr)]
transformStmts (Abs.SVRet _:stmts) = [transformStmt' (Abs.SVRet undefined)]
transformStmts (stmt:stmts) = transformStmt' stmt : transformStmts stmts
transformStmts [] = []

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
transformExpr (Abs.ENeg _ expr) = 
  let tExpr = transformExpr expr in
  case tExpr of
    AST.ELitInt integer -> AST.ELitInt (-integer)
    _ -> AST.ENeg tExpr
transformExpr (Abs.ENot _ expr) = 
  let tExpr = transformExpr expr in
  case tExpr of
    AST.ELitTrue -> AST.ELitFalse
    AST.ELitFalse -> AST.ELitTrue
    _ -> AST.ENot tExpr
transformExpr (Abs.EMul _ expr1 mulOp expr2) = 
  let tExpr1 = transformExpr expr1
      tExpr2 = transformExpr expr2 in
  case (tExpr1, tExpr2) of
    (AST.ELitInt integer1, AST.ELitInt integer2) -> 
      case mulOp of
        Abs.OTimes _ -> AST.ELitInt (integer1 * integer2)
        Abs.ODiv _ -> AST.ELitInt (integer1 `div` integer2)
        Abs.OMod _ -> AST.ELitInt (integer1 `mod` integer2)
    _ -> AST.EOp tExpr1 (transformMulOp mulOp) tExpr2
transformExpr (Abs.EAdd _ expr1 addOp expr2) =
  let tExpr1 = transformExpr expr1
      tExpr2 = transformExpr expr2 in
  case (tExpr1, tExpr2) of
    (AST.ELitInt integer1, AST.ELitInt integer2) -> 
      case addOp of
        Abs.OPlus _ -> AST.ELitInt (integer1 + integer2)
        Abs.OMinus _ -> AST.ELitInt (integer1 - integer2)
    _ -> AST.EOp tExpr1 (transformAddOp addOp) tExpr2
transformExpr (Abs.ERel _ expr1 relOp expr2) = 
  let tExpr1 = transformExpr expr1
      tExpr2 = transformExpr expr2 in
  case (tExpr1, tExpr2) of
    (AST.ELitInt integer1, AST.ELitInt integer2) -> 
      case relOp of
        Abs.OLTH _ -> toASTBool (integer1 < integer2)
        Abs.OLE _ -> toASTBool (integer1 <= integer2)
        Abs.OGTH _ -> toASTBool (integer1 > integer2)
        Abs.OGE _ -> toASTBool (integer1 >= integer2)
        Abs.OEQU _ -> toASTBool (integer1 == integer2)
        Abs.ONE _ -> toASTBool (integer1 /= integer2)
    (AST.ELitTrue, AST.ELitTrue) -> 
      case relOp of
        Abs.OEQU _ -> AST.ELitTrue
        Abs.ONE _ -> AST.ELitFalse
    (AST.ELitFalse, AST.ELitFalse) -> 
      case relOp of
        Abs.OEQU _ -> AST.ELitTrue
        Abs.ONE _ -> AST.ELitFalse
    (AST.ELitTrue, AST.ELitFalse) -> 
      case relOp of
        Abs.OEQU _ -> AST.ELitFalse
        Abs.ONE _ -> AST.ELitTrue
    (AST.ELitFalse, AST.ELitTrue) -> 
      case relOp of
        Abs.OEQU _ -> AST.ELitFalse
        Abs.ONE _ -> AST.ELitTrue
    _ -> AST.ERel tExpr1 (transformRelOp relOp) tExpr2
transformExpr (Abs.EAnd _ expr1 expr2) = 
  let tExpr1 = transformExpr expr1
      tExpr2 = transformExpr expr2 in
  case (tExpr1, tExpr2) of
    (AST.ELitTrue, b) -> b
    (b, AST.ELitTrue) -> b
    (_, AST.ELitFalse) -> AST.ELitFalse
    (AST.ELitFalse, _) -> AST.ELitFalse
    _ -> AST.EAnd tExpr1 tExpr2
transformExpr (Abs.EOr _ expr1 expr2) =
  let tExpr1 = transformExpr expr1
      tExpr2 = transformExpr expr2 in
  case (tExpr1, tExpr2) of
    (AST.ELitTrue, _) -> AST.ELitTrue
    (_, AST.ELitTrue) -> AST.ELitTrue
    (AST.ELitFalse, b) -> b
    (b, AST.ELitFalse) -> b
    _ -> AST.EOr tExpr1 tExpr2

toASTBool :: Bool -> AST.Expr
toASTBool True = AST.ELitTrue
toASTBool False = AST.ELitFalse

transformMulOp :: Abs.MulOp -> AST.ArithOp
transformMulOp (Abs.OTimes _) = AST.OTimes
transformMulOp (Abs.ODiv _) = AST.ODiv
transformMulOp (Abs.OMod _) = AST.OMod

transformAddOp :: Abs.AddOp -> AST.ArithOp
transformAddOp (Abs.OPlus _) = AST.OPlus
transformAddOp (Abs.OMinus _) = AST.OMinus

transformRelOp :: Abs.RelOp -> AST.RelOp
transformRelOp (Abs.OLTH _) = AST.OLTH
transformRelOp (Abs.OLE _) = AST.OLE
transformRelOp (Abs.OGTH _) = AST.OGTH
transformRelOp (Abs.OGE _) = AST.OGE
transformRelOp (Abs.OEQU _) = AST.OEQU
transformRelOp (Abs.ONE _) = AST.ONE

transformIIdent :: Abs.IIdent -> AST.Ident
transformIIdent (Abs.IIdent _ (Abs.Ident ident)) = ident
