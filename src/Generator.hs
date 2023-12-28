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
    getCVenv = Map.empty,
    getSealedBlocks = [],
    getIncompletePhis = Map.empty,
    getPhiCount = 0,
    getPhiEnv = Map.empty,
    getVarType = Map.empty
    -- TODO
  } in do
    result <- runStateT (genProgram ast) initState
    let funs = Map.elems $ getFunctions $ snd result
    return $ unlines $ map show funs ++ ["define i32 @main() {\n  %r = call i32 @fun.main()\n  ret i32 %r\n}"]


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
      block <- gets getCurrentBasicBlock
      instrs <- getInstrs
      let phiInstrs = filter isPhiInstr instrs
      let instrs' = filter (not . isPhiInstr) instrs
      phiInstrs' <- mapM (\instr -> case instr of
        IPhi' reg phiId -> do
          phi <- gets $ (Map.! phiId) . getPhiEnv
          let (APhi _ _ operands) = phi
          return $ IPhi reg operands
        _ -> return instr
        ) phiInstrs
      -- TODO remove trivial phis
      let block' = block { getBlockInstrs = phiInstrs' ++ instrs' }
      modify $ \s -> s { getBasicBlockEnv = Map.insert label block' (getBasicBlockEnv s), getCurrentBasicBlock = nothingBlock }
    Nothing -> do
      error "No terminator in block"


isPhiInstr :: Instr -> Bool
isPhiInstr (IPhi' _ _) = True
isPhiInstr (IPhi _ _) = True
isPhiInstr _ = False



genProgram :: Program -> GenM ()
genProgram (PProgram topDefs) = do
  mapM_ genTopDef topDefs

genTopDef :: TopDef -> GenM ()
genTopDef (PFunDef t ident args block) = do
  -- TODO args
  label <- freshLabel
  newBasicBlock label []
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
  addVarAddr ident addr
  addVarType ident (toCompType t)
  return ()
genStmt (SAss expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  case addr1 of
    AImmediate val -> do
      let ident = getVarName expr1
      addVarAddr ident addr2
    ARegister _ t -> do
      let ident = getVarName expr1
      addVarAddr ident addr2
    APhi {} -> do
      -- error "Cannot assign to phi"
      return ()
    -- TODO
  return ()
genStmt (SRet expr) = do
  addr <- genExpr expr
  emitTerminator $ IRet addr
  emitBasicBlock
  return ()
genStmt (SCondElse expr thenStmt elseStmt) = do
  currentLabel <- getLabel
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  endLabel <- freshLabel

  genIfThenElseBlocks expr thenLabel elseLabel
  sealBlock thenLabel
  sealBlock elseLabel

  newBasicBlock thenLabel [currentLabel]
  genStmt thenStmt
  emitJump endLabel

  newBasicBlock elseLabel [currentLabel]
  genStmt elseStmt
  emitJump endLabel

  sealBlock endLabel
  newBasicBlock endLabel [thenLabel, elseLabel]

  return ()
genStmt s = error $ "Not implemented " ++ show s

emitJump :: Label -> GenM ()
emitJump label = do
  emitTerminator $ IJmp label
  emitBasicBlock

emitBranch :: Address -> Label -> Label -> GenM ()
emitBranch addr label1 label2 = do
  emitTerminator $ IBr addr label1 label2 -- TODO
  emitBasicBlock

genIfThenElseBlocks :: Expr -> Label -> Label -> GenM ()
genIfThenElseBlocks (EAnd expr1 expr2) thenLabel elseLabel = do
  currentLabel <- getLabel
  interLabel <- freshLabel
  genIfThenElseBlocks expr1 interLabel elseLabel -- TODO seal?
  newBasicBlock interLabel [currentLabel]
  genIfThenElseBlocks expr2 thenLabel elseLabel
genIfThenElseBlocks (EOr expr1 expr2) thenLabel elseLabel = do
  currentLabel <- getLabel
  interLabel <- freshLabel
  genIfThenElseBlocks expr1 thenLabel interLabel
  newBasicBlock interLabel [currentLabel]
  genIfThenElseBlocks expr2 thenLabel elseLabel
genIfThenElseBlocks ELitFalse thenLabel elseLabel = emitJump elseLabel
genIfThenElseBlocks ELitTrue thenLabel elseLabel = emitJump thenLabel
genIfThenElseBlocks expr thenLabel elseLabel = do
  addr <- genExpr expr
  emitBranch addr thenLabel elseLabel


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
genExpr (ELitInt n) = return $ AImmediate $ EVInt n
genExpr (EVar ident) = do
  label <- getLabel
  readVar ident label
genExpr (EOp expr1 op expr2) = genBinOp op expr1 expr2
genExpr (ERel expr1 op expr2) = genRelOp op expr1 expr2

genBinOp :: ArithOp -> Expr -> Expr -> GenM Address
genBinOp op e1 e2 = do
  addr1 <- genExpr e1
  addr2 <- genExpr e2
  addr <- freshReg (getAddrType addr1)
  emitInstr $ IBinOp addr addr1 op addr2
  return addr

genRelOp :: RelOp -> Expr -> Expr -> GenM Address
genRelOp op e1 e2 = do
  addr1 <- genExpr e1
  addr2 <- genExpr e2
  addr <- freshReg CBool
  emitInstr $ IRelOp addr addr1 op addr2
  return addr

genLhs :: Expr -> GenM Address
genLhs (EVar ident) = do
  label <- getLabel
  readVar ident label

freshReg :: CType -> GenM Address
freshReg t = do
  n <- gets getRegCount
  modify $ \s -> s { getRegCount = n + 1 }
  return $ ARegister n t

freshLabel :: GenM String
freshLabel = do
  modify $ \s -> s { getLabelCount = getLabelCount s + 1 }
  gets $ idToLabel . getLabelCount

freshPhi :: Label -> CType -> GenM (Address, PhiID)
freshPhi label t = do
  block <- gets $ (Map.! label) . getBasicBlockEnv
  modify $ \s -> s { getPhiCount = getPhiCount s + 1 }
  phiId <- gets getPhiCount
  let phi = APhi phiId t []
  modify $ \s -> s { getPhiEnv = Map.insert phiId phi (getPhiEnv s) }
  newReg <- freshReg (getAddrType phi)
  emitInstr $ IPhi' newReg phiId
  return (newReg, phiId)

addVarAddr :: String -> Address -> GenM ()
addVarAddr name addr = do
  label <- getLabel
  writeVar name label addr

addVarType :: String -> CType -> GenM ()
addVarType name t = do
  modify $ \s -> s { getVarType = Map.insert name t (getVarType s) }


hasOnePred :: Label -> GenM Bool
hasOnePred label = do
  block <- gets $ (Map.! label) . getBasicBlockEnv
  return $ length (getBlockPredecessors block) == 1

writeVar :: Ident -> Label -> Address -> GenM ()
writeVar ident label addr = do
  venv <- gets getVEnv
  case Map.lookup ident venv of
    Just m -> modify $ \s -> s { getVEnv = Map.insert ident (Map.insert label addr m) venv }
    Nothing -> modify $ \s -> s { getVEnv = Map.insert ident (Map.singleton label addr) venv }

readVar :: Ident -> Label -> GenM Address
readVar ident label = do
  venv <- gets getVEnv
  case Map.lookup ident venv of
    Just m -> case Map.lookup label m of
      Just addr -> return addr
      Nothing -> readVarRec ident label
    Nothing ->
      error $ "Variable " ++ ident ++ " not found in environment."

readVarRec :: Ident -> Label -> GenM Address
readVarRec ident label = do
  sealedBlocks <- gets getSealedBlocks
  addr <- if label `notElem` sealedBlocks then do
        liftIO $ print $ "if " ++ ident ++ " " ++ label
        t <- gets $ (Map.! ident) . getVarType
        (addr', phiId) <- freshPhi label t
        modify $ \s -> s {
          getIncompletePhis = Map.insert label (Map.insert ident phiId (Map.findWithDefault Map.empty label (getIncompletePhis s))) (getIncompletePhis s)
        }
        return addr'
      else do
        b <- hasOnePred label
        if b then do
          liftIO $ print $ "else if " ++ ident ++ " " ++ label
          blockEnv <- gets getBasicBlockEnv
          case Map.lookup label blockEnv of
            Nothing -> error $ "Block " ++ label ++ " not found in environment."
            _ -> return ()
          block <- gets $ (Map.! label) . getBasicBlockEnv
          readVar ident (head $ getBlockPredecessors block)
        else do
          liftIO $ print $ "else " ++ ident ++ " " ++ label
          t <- gets $ (Map.! ident) . getVarType
          (addr', phiId) <- freshPhi label t
          writeVar ident label addr'
          addPhiOperands ident phiId label
          return addr'
  liftIO $ print addr
  writeVar ident label addr
  return addr

addPhiOperands :: Ident -> PhiID -> Label -> GenM ()
addPhiOperands ident phiId label = do
  preds <- gets $ getBlockPredecessors . getCurrentBasicBlock
  newOperands <- mapM (\pred -> do
    addr' <- readVar ident pred
    return (pred, addr')
    ) preds
  phi <- gets $ (Map.! phiId) . getPhiEnv
  let (APhi phiId t oldOperands) = phi
  let newPhi = APhi phiId t $ oldOperands ++ newOperands
  modify $ \s -> s { getPhiEnv = Map.insert phiId newPhi (getPhiEnv s) }
  -- TODO

sealBlock :: Label -> GenM ()
sealBlock label = do
  incompletePhis <- gets $ Map.findWithDefault Map.empty label . getIncompletePhis
  mapM_ (\(var, phiId) -> do
      addPhiOperands var phiId label
    ) $ Map.toList incompletePhis
  modify $ \s -> s { getSealedBlocks = label : getSealedBlocks s }