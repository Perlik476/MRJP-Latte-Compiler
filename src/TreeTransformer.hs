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

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List

import qualified AST

data TMState = TMState {
  getNames :: Map String Integer,
  getEnv :: Map String String
}

type TM a = State TMState a


getNewName :: String -> TM String
getNewName name = do
  names <- gets getNames
  case Map.lookup name names of
    Nothing -> do
      modify $ \s -> s { getNames = Map.insert name 1 names }
      return $ name ++ ".0"
    Just n -> do
      modify $ \s -> s { getNames = Map.insert name (n + 1) names }
      return $ name ++ "." ++ show n

transformTree :: Abs.Program -> AST.Program
transformTree prog = evalState (transformProgram prog) (TMState Map.empty Map.empty)

transformProgram :: Abs.Program -> TM AST.Program
transformProgram (Abs.PProgram _ topDefs) = AST.PProgram <$> mapM transformTopDef topDefs

transformTopDef :: Abs.TopDef -> TM AST.TopDef
transformTopDef (Abs.PFunDef _ t ident args block) = do
  mapM_ (\(Abs.PArg _ t (Abs.IIdent _ (Abs.Ident ident))) -> do
    newIdent <- getNewName ident
    modify (\s -> s { getEnv = Map.insert ident newIdent (getEnv s) })) args
  AST.PFunDef <$> transformType t <*> transformIIdentFun ident <*> mapM transformArg args <*> transformBlock block
transformTopDef (Abs.PClassDef _ ident classDef) = 
  AST.PClassDef <$> transformIIdentClass ident <*> transformClassDef classDef
transformArg :: Abs.Arg -> TM AST.Arg
transformArg (Abs.PArg _ t ident) = AST.PArg <$> transformType t <*> transformIIdent ident

transformClassDef :: Abs.ClassDef -> TM AST.ClassDef
transformClassDef (Abs.ClassDef _ classElems) = AST.ClassDef <$> mapM transformClassElem classElems

transformClassElem :: Abs.ClassElem -> TM AST.ClassElem
transformClassElem (Abs.ClassAttrDef _ t classItems) = AST.ClassAttrDef <$> transformType t <*> mapM transformClassItem classItems

transformClassItem :: Abs.ClassItem -> TM AST.ClassItem
transformClassItem (Abs.ClassItem _ ident) = AST.ClassItem <$> transformIIdent ident

transformBlock :: Abs.Block -> TM AST.Block
transformBlock (Abs.SBlock _ stmts) = do
  oldEnv <- gets getEnv
  tStmts <- transformStmts stmts
  modify (\s -> s { getEnv = oldEnv })
  return $ AST.SBlock tStmts

transformStmt' :: Abs.Stmt -> TM AST.Stmt
transformStmt' (Abs.SEmpty _) = pure AST.SEmpty
transformStmt' (Abs.SBStmt _ block) = AST.SBStmt <$> transformBlock block
transformStmt' (Abs.SDecl _ t _) = error "Multiple items in declaration"
transformStmt' (Abs.SAss _ expr1 expr2) = AST.SAss <$> transformExpr expr1 <*> transformExpr expr2
transformStmt' (Abs.SIncr _ expr) = AST.SIncr <$> transformExpr expr
transformStmt' (Abs.SDecr _ expr) = AST.SDecr <$> transformExpr expr
transformStmt' (Abs.SRet _ expr) = AST.SRet <$> transformExpr expr
transformStmt' (Abs.SVRet _) = pure AST.SVRet
transformStmt' (Abs.SCond _ expr stmt) = do
  tExpr <- transformExpr expr
  case tExpr of
    AST.ELitTrue -> transformStmt' stmt
    AST.ELitFalse -> return AST.SEmpty
    _ -> AST.SCond tExpr <$> transformStmt' stmt
transformStmt' (Abs.SCondElse _ expr stmt1 stmt2) = do
  tExpr <- transformExpr expr
  case tExpr of
    AST.ELitTrue -> transformStmt' stmt1
    AST.ELitFalse -> transformStmt' stmt2
    _ -> AST.SCondElse tExpr <$> transformStmt' stmt1 <*> transformStmt' stmt2
transformStmt' (Abs.SWhile _ expr stmt) = do
  tExpr <- transformExpr expr
  case tExpr of
    AST.ELitTrue -> AST.SWhile tExpr <$> transformStmt' stmt
    AST.ELitFalse -> return AST.SEmpty
    _ -> AST.SWhile tExpr <$> transformStmt' stmt
transformStmt' (Abs.SFor _ t ident expr stmt) = AST.SFor <$> transformType t <*> transformIIdent ident <*> transformExpr expr <*> transformStmt' stmt
transformStmt' (Abs.SExp _ expr) = AST.SExp <$> transformExpr expr

transformStmts :: [Abs.Stmt] -> TM [AST.Stmt]
transformStmts (Abs.SDecl _ t (item:items):stmts) = 
  case item of
    Abs.SNoInit _ (Abs.IIdent _ (Abs.Ident ident)) -> do
      t' <- transformType t
      newIdent <- getNewName ident
      modify (\s -> s { getEnv = Map.insert ident newIdent (getEnv s) })
      tStmts <- transformStmts (Abs.SDecl undefined t items : stmts)
      return $ AST.SDecl t' newIdent (defaultValue t') : tStmts
    Abs.SInit _ (Abs.IIdent _ (Abs.Ident ident)) expr -> do
      t' <- transformType t
      tExpr <- transformExpr expr
      newIdent <- getNewName ident
      modify (\s -> s { getEnv = Map.insert ident newIdent (getEnv s) })
      tStmts <- transformStmts (Abs.SDecl undefined t items : stmts)
      return $ AST.SDecl t' newIdent tExpr : tStmts
transformStmts (Abs.SDecl _ t []:stmts) = transformStmts stmts
transformStmts (Abs.SRet _ expr:stmts) = (:[]) <$> transformStmt' (Abs.SRet undefined expr)
transformStmts (Abs.SVRet _:stmts) = (:[]) <$> transformStmt' (Abs.SVRet undefined)
transformStmts (stmt:stmts) = (:) <$> transformStmt' stmt <*> transformStmts stmts
transformStmts [] = pure []

defaultValue :: AST.Type -> AST.Expr
defaultValue AST.TInt = AST.ELitInt 0
defaultValue AST.TStr = AST.EString ""
defaultValue AST.TBool = AST.ELitFalse


transformType :: Abs.Type -> TM AST.Type
transformType (Abs.TInt _) = pure AST.TInt
transformType (Abs.TStr _) = pure AST.TStr
transformType (Abs.TBool _) = pure AST.TBool
transformType (Abs.TVoid _) = pure AST.TVoid
transformType (Abs.TArray _ t) = AST.TArray <$> transformType t
transformType (Abs.TClass _ ident) = AST.TClass <$> transformIIdent ident

transformExpr :: Abs.Expr -> TM AST.Expr
transformExpr (Abs.EVar _ ident) = AST.EVar <$> transformIIdent ident
transformExpr (Abs.ELitInt _ integer) = pure $ AST.ELitInt integer
transformExpr (Abs.ELitTrue _) = pure AST.ELitTrue
transformExpr (Abs.ELitFalse _) = pure AST.ELitFalse
transformExpr (Abs.ECastNull _ t) = AST.ECastNull <$> transformType t
transformExpr (Abs.EString _ string) = pure $ AST.EString string
transformExpr (Abs.EFunctionCall _ ident exprs) = AST.EFunctionCall <$> transformIIdentFun ident <*> mapM transformExpr exprs
transformExpr (Abs.EClassNew _ ident) = AST.EClassNew <$> transformIIdentClass ident
transformExpr (Abs.EClassAttr _ expr ident) = AST.EClassAttr <$> transformExpr expr <*> transformIIdent ident  -- TODO
transformExpr (Abs.EMethodCall _ expr ident exprs) = AST.EMethodCall <$> transformExpr expr <*> transformIIdent ident <*> mapM transformExpr exprs
transformExpr (Abs.EArrayNew _ t expr) = AST.EArrayNew <$> transformType t <*> transformExpr expr
transformExpr (Abs.EArrayElem _ expr expr') = AST.EArrayElem <$> transformExpr expr <*> transformExpr expr'
transformExpr (Abs.ENeg _ expr) = do
  tExpr <- transformExpr expr
  return $ case tExpr of
    AST.ELitInt integer -> AST.ELitInt (-integer)
    _ -> AST.ENeg tExpr
transformExpr (Abs.ENot _ expr) = do
  tExpr <- transformExpr expr
  return $ case tExpr of
    AST.ELitTrue -> AST.ELitFalse
    AST.ELitFalse -> AST.ELitTrue
    _ -> AST.ENot tExpr
transformExpr (Abs.EMul _ expr1 mulOp expr2) = do
  tExpr1 <- transformExpr expr1
  tExpr2 <- transformExpr expr2
  return $ case (tExpr1, tExpr2) of
    (AST.ELitInt integer1, AST.ELitInt integer2) -> 
      case mulOp of
        Abs.OTimes _ -> AST.ELitInt (integer1 * integer2)
        Abs.ODiv _ -> AST.ELitInt (integer1 `div` integer2)
        Abs.OMod _ -> AST.ELitInt (integer1 `mod` integer2)
    _ -> AST.EOp tExpr1 (transformMulOp mulOp) tExpr2
transformExpr (Abs.EAdd _ expr1 addOp expr2) = do
  tExpr1 <- transformExpr expr1
  tExpr2 <- transformExpr expr2
  return $ case (tExpr1, tExpr2) of
    (AST.ELitInt integer1, AST.ELitInt integer2) -> 
      case addOp of
        Abs.OPlus _ -> AST.ELitInt (integer1 + integer2)
        Abs.OMinus _ -> AST.ELitInt (integer1 - integer2)
    _ -> AST.EOp tExpr1 (transformAddOp addOp) tExpr2
transformExpr (Abs.ERel _ expr1 relOp expr2) = do
  tExpr1 <- transformExpr expr1
  tExpr2 <- transformExpr expr2
  return $ case (tExpr1, tExpr2) of
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
transformExpr (Abs.EAnd _ expr1 expr2) = do
  tExpr1 <- transformExpr expr1
  tExpr2 <- transformExpr expr2
  return $ case (tExpr1, tExpr2) of
    (AST.ELitTrue, _) -> tExpr2
    (_, AST.ELitTrue) -> tExpr1
    (_, AST.ELitFalse) -> AST.EAnd tExpr1 tExpr2 -- TODO check if correct semantics
    (AST.ELitFalse, _) -> AST.ELitFalse
    _ -> AST.EAnd tExpr1 tExpr2
transformExpr (Abs.EOr _ expr1 expr2) = do
  tExpr1 <- transformExpr expr1
  tExpr2 <- transformExpr expr2
  return $ case (tExpr1, tExpr2) of
    (AST.ELitTrue, _) -> AST.ELitTrue
    (_, AST.ELitTrue) -> AST.ELitTrue
    (AST.ELitFalse, _) -> tExpr2
    (_, AST.ELitFalse) -> AST.EOr tExpr1 tExpr2
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

transformIIdent :: Abs.IIdent -> TM AST.Ident
transformIIdent (Abs.IIdent _ (Abs.Ident "main")) = return "main"
transformIIdent (Abs.IIdent _ (Abs.Ident ident)) = do
  env <- gets getEnv
  case Map.lookup ident env of
    Just newIdent -> return newIdent
    Nothing -> error $ "Variable " ++ ident ++ " not found in environment." ++ "Env: " ++ show env
  
transformIIdent' :: String -> Abs.IIdent -> TM AST.Ident
transformIIdent' s (Abs.IIdent _ (Abs.Ident ident)) = return $ s ++ "." ++ ident

transformIIdentClass :: Abs.IIdent -> TM AST.Ident
transformIIdentClass = transformIIdent' "class"

transformIIdentFun :: Abs.IIdent -> TM AST.Ident
transformIIdentFun = transformIIdent' "fun"

fromIIdent :: Abs.IIdent -> String
fromIIdent (Abs.IIdent _ (Abs.Ident ident)) = ident