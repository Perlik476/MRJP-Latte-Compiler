module Generator where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO          ( hPutStrLn, stderr, hPutStr )
import Control.Monad      ( when )

import Latte.Lex   ( Token, mkPosToken )
import Latte.Par   ( pProgram, myLexer )
import Latte.Print ( Print, printTree )
import Latte.Skel  ()

import Data.Map (Map, empty, fromList, union, member, lookup, insert, toList, keys, difference, intersection, elems, (!), intersectionWith)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List

import AST
import Utils
import Emitter


compile :: Program -> IO String
compile ast =
  let initState = GenState {
    getInstrs = [],
    getRegEnv = empty,
    getRegCount = 0,
    getFEnv = empty,
    getCVenv = empty
    -- TODO
  } in do
    result <- runStateT (genProgram ast) initState
    let instrs = getInstrs $ snd result
    return $ unlines instrs

genProgram :: Program -> GenM ()
genProgram (PProgram topDefs) = do
  mapM_ genTopDef topDefs

genTopDef :: TopDef -> GenM ()
genTopDef (PFunDef t ident args block) = do
  emit $ "define " ++ showType (toCompType t) ++ " @" ++ ident ++ "(" ++ concatMap showArg args ++ ") {"
  genBlock block
  emit "}"

showArg :: Arg -> String
showArg (PArg t ident) = show t ++ " " ++ ident

genBlock :: Block -> GenM ()
genBlock (SBlock stmts) = do
  regEnv <- gets getRegEnv
  mapM_ genStmt stmts
  modify $ \s -> s { getRegEnv = regEnv }

genStmt :: Stmt -> GenM ()
genStmt (SExp expr) = do
  genExpr expr
  return ()
genStmt (SDecl t ident expr) = do
  addr <- genExpr expr
  modify $ \s -> s { getRegEnv = insert ident addr (getRegEnv s) }
  return ()
genStmt (SAss expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  case addr1 of
    AImmediate val t -> do
      let ident = getVarName expr1
      modify $ \s -> s { getRegEnv = insert ident addr2 (getRegEnv s) }
    ARegister _ t -> do
      let ident = getVarName expr1
      modify $ \s -> s { getRegEnv = insert ident addr2 (getRegEnv s) }
  return ()
genStmt (SRet expr) = do
  addr <- genExpr expr
  emit $ "ret " ++ showType (getAddrType addr) ++ " " ++ show addr
  return ()

getVarName :: Expr -> String
getVarName (EVar ident) = ident
getVarName _ = error "Not a variable"

toCompType :: Type -> CType
toCompType TInt = CInt
toCompType TBool = CBool
toCompType TVoid = CVoid
toCompType TStr = CString
-- TODO

genRhs, genExpr :: Expr -> GenM Address
genRhs = genExpr
genExpr (ELitInt n) = return $ AImmediate n CInt
genExpr (EVar ident) = do
  regEnv <- gets getRegEnv
  case Data.Map.lookup ident regEnv of
    Just addr -> return addr
    Nothing ->
      error $ "Variable " ++ ident ++ " not found in environment."
genExpr (EOp expr1 op expr2) = genBinOp op expr1 expr2

genBinOp :: ArithOp -> Expr -> Expr -> GenM Address
genBinOp op e1 e2 = do
  addr1 <- genExpr e1
  addr2 <- genExpr e2
  addr <- freshReg (getAddrType addr1)
  emitBinOp addr addr1 op addr2
  return addr

genLhs :: Expr -> GenM Address
genLhs (EVar ident) = getAddr ident

getAddr :: Ident -> GenM Address
getAddr ident = do
  env <- gets getRegEnv
  case Data.Map.lookup ident env of
    Just addr -> return addr
    Nothing ->
      error $ "Variable " ++ ident ++ " not found in environment."

freshReg :: CType -> GenM Address
freshReg t = do
  n <- gets getRegCount
  modify $ \s -> s { getRegCount = n + 1 }
  return $ ARegister n t

getAddrType :: Address -> CType
getAddrType (AImmediate _ t) = t
getAddrType (ARegister _ t) = t
