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

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List

import AST
import Utils


compile :: Program -> IO String
compile ast =
  let initState = GenState {
    getCurrentBasicBlock = nothingBlock,
    getLabelCount = 0,
    getBasicBlockEnv = Map.empty,
    getFunctions = Map.empty,
    getVEnv = Map.empty,
    getRegCount = 0,
    getFEnv = Map.empty,
    getCVenv = Map.empty
    -- TODO
  } in do
    result <- runStateT (genProgram ast) initState
    let funs = Map.elems $ getFunctions $ snd result
    return $ unlines $ map show funs ++ ["define i32 @main() {\n%r = call i32 @fun.main()\nret i32 %r\n}"]


emitInstr :: Instr -> GenM ()
emitInstr = addInstr

emitTerminator :: Instr -> GenM ()
emitTerminator = addTerminator

emitBasicBlock :: GenM ()
emitBasicBlock = do
  instrs <- getInstrs
  term <- getTerminator
  case term of
    Just t -> do
      label <- getLabel
      modify $ \s -> s { getBasicBlockEnv = Map.insert label (getCurrentBasicBlock s) (getBasicBlockEnv s) }
    Nothing -> do
      error "No terminator in block"


genProgram :: Program -> GenM ()
genProgram (PProgram topDefs) = do
  mapM_ genTopDef topDefs

genTopDef :: TopDef -> GenM ()
genTopDef (PFunDef t ident args block) = do
  -- TODO args
  label <- freshLabel
  newBasicBlock label
  genBlock block
  basicBlocks <- gets getBasicBlockEnv
  modify $ \s -> s {
    getBasicBlockEnv = Map.empty,
    getFunctions =
      Map.insert ident (FunBlock ident (toCompType t) (map (\(PArg t ident) -> (ident, toCompType t)) args) (Map.elems basicBlocks)) (getFunctions s)
  }
  return ()

showArg :: Arg -> String
showArg (PArg t ident) = show t ++ " " ++ ident

genBlock :: Block -> GenM ()
genBlock (SBlock stmts) = do
  regEnv <- gets getVEnv
  mapM_ genStmt stmts
  modify $ \s -> s { getVEnv = regEnv }

genStmt :: Stmt -> GenM ()
genStmt (SExp expr) = do
  genExpr expr
  return ()
genStmt (SBStmt block) = do
  genBlock block
  return ()
genStmt (SDecl t ident expr) = do
  addr <- genExpr expr
  modify $ \s -> s { getVEnv = Map.insert ident addr (getVEnv s) }
  return ()
genStmt (SAss expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  case addr1 of
    AImmediate val t -> do
      let ident = getVarName expr1
      modify $ \s -> s { getVEnv = Map.insert ident addr2 (getVEnv s) }
    ARegister _ t -> do
      let ident = getVarName expr1
      modify $ \s -> s { getVEnv = Map.insert ident addr2 (getVEnv s) }
  return ()
genStmt (SRet expr) = do
  addr <- genExpr expr
  emitTerminator $ IRet addr
  emitBasicBlock
  return ()
genStmt s = error $ "Not implemented " ++ show s

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
  regEnv <- gets getVEnv
  case Map.lookup ident regEnv of
    Just addr -> return addr
    Nothing ->
      error $ "Variable " ++ ident ++ " not found in environment."
genExpr (EOp expr1 op expr2) = genBinOp op expr1 expr2

genBinOp :: ArithOp -> Expr -> Expr -> GenM Address
genBinOp op e1 e2 = do
  addr1 <- genExpr e1
  addr2 <- genExpr e2
  addr <- freshReg (getAddrType addr1)
  emitInstr $ IBinOp addr addr1 op addr2
  return addr

genLhs :: Expr -> GenM Address
genLhs (EVar ident) = getAddr ident

getAddr :: Ident -> GenM Address
getAddr ident = do
  env <- gets getVEnv
  case Map.lookup ident env of
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
