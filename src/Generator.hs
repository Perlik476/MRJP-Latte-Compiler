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

import Data.Map (Map)
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
    getCurrentLabel = "",
    getCurrentFunLabels = [],
    getCurrentFunName = "",
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
    getVarType = Map.empty,
    getStringPool = Map.empty,
    getStringPoolCount = 0
    -- TODO
  } in do
    result <- runStateT (genProgram ast) initState
    let funs = Map.elems $ getFunctions $ snd result
    let stringPool = getStringPool $ snd result
    let lines = [
          "declare void @fun.printInt(i32)",
          "declare i32 @fun.readInt()",
          "declare void @fun.printString(i8*)",
          "declare i8* @fun.readString()",
          "declare void @fun.error()",
          "declare i8* @fun.internal.concatStrings(i8*, i8*)",
          "declare i1 @fun.internal.compareStrings(i8*, i8*)",
          ""
          ] ++
          map showStrPool (Map.toList stringPool) ++ ["\n"] ++
          map show funs ++
          ["define i32 @main() {", "  %r = call i32 @fun.main()", "  ret i32 %r", "}"]
    return $ unlines lines


isPhiInstr :: Instr -> Bool
isPhiInstr (IPhi' _ _) = True
isPhiInstr (IPhi _ _) = True
isPhiInstr _ = False



genProgram :: Program -> GenM ()
genProgram (PProgram topDefs) = do
  addStdLib
  mapM_ addFunsAndClassesToEnvs topDefs
  mapM_ genTopDef topDefs
  funs <- gets getFunctions
  funs' <- mapM translatePhis funs
  modify $ \s -> s { getFunctions = funs' }


addStdLib :: GenM ()
addStdLib = do
  fenv <- gets getFEnv
  let fenv' = Map.union fenv $ Map.fromList [
        ("fun.printInt", FunType "fun.printInt" CVoid [(ARegister 0 CInt, CInt)]),
        ("fun.readInt", FunType "fun.readInt" CInt []),
        ("fun.printString", FunType "fun.printString" CVoid [(ARegister 0 CString, CString)]),
        ("fun.readString", FunType "fun.readString" CString []),
        ("fun.error", FunType "fun.error" CVoid [])
        ]
  modify $ \s -> s { getFEnv = fenv' }


addFunsAndClassesToEnvs :: TopDef -> GenM ()
addFunsAndClassesToEnvs (PFunDef t ident args block) = do
  funEntry <- freshLabel
  setCurrentLabel funEntry
  args' <- mapM (\(PArg t ident') -> do
    addr <- freshReg (toCompType t)
    addVarAddr ident' addr
    addVarType ident' (toCompType t)
    return (addr, toCompType t)
    ) args
  modify $ \s -> s {
    getFEnv = Map.insert ident (FunType funEntry (toCompType t) args') (getFEnv s),
    getCurrentFunLabels = []
  }
addFunsAndClassesToEnvs _ = error "Classes not implemented"

translatePhis :: FunBlock -> GenM FunBlock
translatePhis fun = do
  blocks <- mapM translatePhis' $ getFunBlocks fun
  return $ fun { getFunBlocks = blocks }

translatePhis' :: BasicBlock -> GenM BasicBlock
translatePhis' blocks = do
  let instrs = getBlockInstrs blocks
  instrs' <- translatePhis'' instrs
  return $ blocks { getBlockInstrs = instrs' }

translatePhis'' :: [Instr] -> GenM [Instr]
translatePhis'' instrs = do
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
  return $ phiInstrs' ++ instrs'

genTopDef :: TopDef -> GenM ()
genTopDef (PFunDef t ident args block) = do
  funType <- gets $ (Map.! ident) . getFEnv
  let funEntry = getFunTypeEntryLabel funType
  modify $ \s -> s { getCurrentFunLabels = [funEntry], getCurrentFunName = ident }
  setCurrentLabel funEntry
  genBlock block
  emitBasicBlock
  basicBlocks <- gets getBasicBlockEnv
  currentFunLabelsRev <- gets getCurrentFunLabels
  let currentFunLabels = reverse currentFunLabelsRev
  funBasicBlocks <- mapM (\label -> gets $ (Map.! label) . getBasicBlockEnv) currentFunLabels
  let args' = getFunTypeArgs funType
  modify $ \s -> s {
    getCurrentFunLabels = [],
    getFunctions =
      Map.insert ident (FunBlock ident (toCompType t) args' funBasicBlocks) (getFunctions s)
  }
  return ()

showArg :: Arg -> String
showArg (PArg t ident) = show t ++ " " ++ ident

genBlock :: Block -> GenM ()
genBlock (SBlock stmts) = mapM_ genStmt stmts



genStmt :: Stmt -> GenM ()
genStmt SEmpty = pure ()
genStmt (SExp expr) = do
  genExpr expr
  return ()
genStmt (SBStmt block) = do
  genBlock block
  return ()
genStmt (SDecl t ident expr) = do
  addr <- genExpr expr
  addVarAddr ident addr
  addVarType ident (getAddrType addr)
  return ()
genStmt (SAss expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  case addr1 of
    AImmediate val -> do
      let ident = getVarName expr1
      addVarAddr ident addr2
    ARegister _ t -> do
      liftIO $ putStrLn $ "type of addr1 is " ++ show t ++ " and type of expr2 is " ++ show (getAddrType addr2)
      case t of
        CPtr _ -> emitInstr $ IStore addr2 addr1
        _ -> do
          let ident = getVarName expr1
          addVarAddr ident addr2
    APhi {} -> error "Cannot assign to phi"
    -- TODO
  return ()
genStmt (SIncr expr) = genStmt (SAss expr (EOp expr OPlus (ELitInt 1)))
genStmt (SDecr expr) = genStmt (SAss expr (EOp expr OMinus (ELitInt 1)))
genStmt (SRet expr) = do
  addr <- genExpr expr
  emitRet addr
  return ()
genStmt SVRet = do
  emitVRet
  return ()
genStmt (SCondElse expr thenStmt elseStmt) = do
  thenLabel <- freshLabel
  elseLabel <- freshLabel
  endLabel <- freshLabel

  emitIfThenElseBlocks expr thenLabel elseLabel
  sealBlock thenLabel
  sealBlock elseLabel

  setCurrentLabel thenLabel
  genStmt thenStmt
  emitJumpIfNoTerminator endLabel

  setCurrentLabel elseLabel
  genStmt elseStmt
  emitJumpIfNoTerminator endLabel

  sealBlock endLabel
  setCurrentLabel endLabel
  return ()
genStmt (SCond expr thenStmt) = do
  thenLabel <- freshLabel
  endLabel <- freshLabel

  emitIfThenElseBlocks expr thenLabel endLabel
  sealBlock thenLabel

  setCurrentLabel thenLabel
  genStmt thenStmt
  emitJumpIfNoTerminator endLabel

  sealBlock endLabel
  setCurrentLabel endLabel
  return ()
genStmt (SWhile expr stmt) = do
  condLabel <- freshLabel
  bodyLabel <- freshLabel
  endLabel <- freshLabel

  emitJump condLabel
  setCurrentLabel condLabel
  emitIfThenElseBlocks expr bodyLabel endLabel
  sealBlock bodyLabel

  setCurrentLabel bodyLabel
  genStmt stmt
  emitJumpIfNoTerminator condLabel

  sealBlock condLabel
  sealBlock endLabel
  setCurrentLabel endLabel
  return ()
genStmt s = error $ "Not implemented " ++ show s



emitInstr :: Instr -> GenM ()
emitInstr instr = do
  label <- getLabel
  addInstr label instr
  return ()

emitTerminator :: Instr -> GenM ()
emitTerminator = addTerminator

emitBasicBlock :: GenM ()
emitBasicBlock = do
  label <- getLabel
  liftIO $ putStrLn $ "Trying to emit block " ++ label
  instrs <- getInstrs
  term <- getTerminator
  if label == "" then
    return ()
  else do
    block <- getCurrentBasicBlock
    preds <- getPreds
    funName <- gets getCurrentFunName
    entryLabel <- gets $ getFunTypeEntryLabel . (Map.! funName) . getFEnv
    if label == entryLabel then do
      liftIO $ putStrLn "Entry block"
      emitBasicBlock'
    else if null preds then do
      liftIO $ putStrLn "No predecessors"
      skipEmitBasicBlock'
    else case term of
      Nothing -> do
        liftIO $ putStrLn "No terminator"
        funRetType <- gets $ getFunTypeRet . (Map.! funName) . getFEnv
        if funRetType == CVoid then do
          liftIO $ putStrLn "Void function"
          emitRet $ AImmediate EVVoid
        else do
          liftIO $ putStrLn "Non-void function"
          emitRet $ AImmediate $ EVUndef funRetType
      _ -> emitBasicBlock'

emitBasicBlock' :: GenM ()
emitBasicBlock' = do
  label <- getLabel
  instrs <- getInstrs
  block <- getCurrentBasicBlock
  liftIO $ putStrLn $ "Emitting block " ++ label
  liftIO $ putStrLn $ "Block " ++ label ++ " has instructions " ++ show instrs
  modify $ \s -> s { getBasicBlockEnv = Map.insert label block (getBasicBlockEnv s), getCurrentLabel = "" }

skipEmitBasicBlock' :: GenM ()
skipEmitBasicBlock' = do
  label <- getLabel
  instrs <- getInstrs
  liftIO $ putStrLn $ "Skipping block " ++ label
  liftIO $ putStrLn $ "No predecessors in block " ++ label ++ " with instructions " ++ show instrs
  modify $ \s -> s {
    getBasicBlockEnv = Map.delete label (getBasicBlockEnv s),
    getCurrentLabel = "",
    getCurrentFunLabels = Data.List.delete label (getCurrentFunLabels s)
  }

emitJump :: Label -> GenM ()
emitJump label = do
  addPredToBlock label =<< getLabel
  emitTerminator $ IJmp label
  emitBasicBlock

emitJumpIfNoTerminator :: Label -> GenM ()
emitJumpIfNoTerminator label = do
  currentLabel <- getLabel
  if currentLabel == "" then
    return ()
  else
    emitJump label

emitBranch :: Address -> Label -> Label -> GenM ()
emitBranch addr label1 label2 = do
  addPredToBlock label1 =<< getLabel
  addPredToBlock label2 =<< getLabel
  emitTerminator $ IBr addr label1 label2
  emitBasicBlock

emitRet :: Address -> GenM ()
emitRet addr = do
  emitTerminator $ IRet addr
  emitBasicBlock

emitVRet :: GenM ()
emitVRet = do
  emitTerminator IVRet
  emitBasicBlock

emitIfThenElseBlocks :: Expr -> Label -> Label -> GenM ()
emitIfThenElseBlocks (EAnd expr1 expr2) thenLabel elseLabel = do
  currentLabel <- getLabel
  interLabel <- freshLabel
  sealBlock interLabel
  emitIfThenElseBlocks expr1 interLabel elseLabel -- TODO seal?
  setCurrentLabel interLabel
  emitIfThenElseBlocks expr2 thenLabel elseLabel
emitIfThenElseBlocks (EOr expr1 expr2) thenLabel elseLabel = do
  currentLabel <- getLabel
  interLabel <- freshLabel
  sealBlock interLabel
  emitIfThenElseBlocks expr1 thenLabel interLabel
  setCurrentLabel interLabel
  emitIfThenElseBlocks expr2 thenLabel elseLabel
emitIfThenElseBlocks ELitFalse thenLabel elseLabel = emitJump elseLabel >> return ()
emitIfThenElseBlocks ELitTrue thenLabel elseLabel = emitJump thenLabel >> return ()
emitIfThenElseBlocks expr thenLabel elseLabel = do
  addr <- genExpr expr
  emitBranch addr thenLabel elseLabel >> return ()


getVarName :: Expr -> String
getVarName (EVar ident) = ident
getVarName _ = error "Not a variable"

toCompType :: Type -> CType
toCompType TInt = CInt
toCompType TBool = CBool
toCompType TVoid = CVoid
toCompType TStr = CString
toCompType (TArray t) = CStruct ["length", "data"] $ Map.fromList [("length", CInt), ("data", CPtr $ toCompType t)]
-- TODO


newStringConst :: String -> GenM Integer
newStringConst str = do
  stringPoolCount <- gets getStringPoolCount
  modify $ \s -> s { getStringPoolCount = stringPoolCount + 1, getStringPool = Map.insert str stringPoolCount (getStringPool s) }
  return stringPoolCount


genRhs, genExpr :: Expr -> GenM Address
genRhs = genExpr
genExpr (ELitInt n) = return $ AImmediate $ EVInt n
genExpr ELitTrue = return $ AImmediate $ EVBool True
genExpr ELitFalse = return $ AImmediate $ EVBool False
genExpr (EString str) = do
  let strLen = 1 + toInteger (length str)
  pool <- gets getStringPool
  case Map.lookup str pool of
    Just strNum -> do
      addr <- freshReg CString
      emitInstr $ IString addr strNum strLen
      return addr
    Nothing -> do
      strNum <- newStringConst str
      addr <- freshReg CString
      emitInstr $ IString addr strNum strLen
      return addr
genExpr (EVar ident) = do
  label <- getLabel
  readVar ident label
genExpr (EFunctionCall ident exprs) = do
  args <- mapM genExpr exprs
  funType <- gets $ (Map.! ident) . getFEnv
  let retType = getFunTypeRet funType
  if retType == CVoid then do
    emitInstr $ IVCall ident args
    return $ AImmediate EVVoid
  else do
    addr <- freshReg retType
    emitInstr $ ICall addr ident args
    return addr
genExpr (ENeg expr) = genExpr (EOp (ELitInt 0) OMinus expr)
genExpr (ENot expr) = genBoolExpr False expr
genExpr (EOp expr1 op expr2) = genBinOp op expr1 expr2
genExpr (ERel expr1 op expr2) = genRelOp op expr1 expr2
genExpr expr@(EAnd _ _) = genBoolExpr True expr
genExpr expr@(EOr _ _) = genBoolExpr True expr
genExpr (ECastNull t) = pure $ AImmediate $ EVNull (toCompType t)
genExpr (EArrayNew t expr) = do
  addr <- genExpr expr
  let ct = toCompType t
  let t' = TArray t
  let ct' = toCompType t'
  addr' <- freshReg (CPtr ct)
  emitInstr $ IAlloca addr' ct (Just addr)
  genStruct ct' ["length", "data"] $ Map.fromList [("length", addr), ("data", addr')]
genExpr (EArrayElem expr1 expr2) = do
  addr1 <- genExpr expr1
  addr2 <- genExpr expr2
  let t = getAddrType addr1
  let (CPtr (CStruct _ fields)) = t
  let t' = CPtr $ fields Map.! "data"
  addr <- freshReg t'
  emitInstr $ IGetElementPtr addr addr1 [AImmediate $ EVInt 0, AImmediate $ EVInt 1]
  let (CPtr t'') = t'
  addr' <- freshReg t''
  emitInstr $ ILoad addr' addr
  addr'' <- freshReg t''
  emitInstr $ IGetElementPtr addr'' addr' [addr2]
  let (CPtr t''') = t''
  addr''' <- freshReg t'''
  emitInstr $ ILoad addr''' addr''
  return addr'''

genStruct :: CType -> [String] -> Map String Address -> GenM Address
genStruct t fieldNames fields = do
  addr <- freshReg (CPtr t)
  emitInstr $ IAlloca addr t Nothing
  mapM_ (\(name, n) -> do
    let addr' = fields Map.! name
    let t' = getAddrType addr'
    liftIO $ putStrLn $ "Type of " ++ name ++ " is " ++ show t'
    let t'' = CPtr t'
    addr'' <- freshReg t''
    emitInstr $ IAlloca addr'' t' Nothing
    addr''' <- freshReg t''
    emitInstr $ IGetElementPtr addr''' addr [AImmediate $ EVInt 0, AImmediate $ EVInt n]
    emitInstr $ IStore addr' addr'''
    ) (fieldNames `zip` [0..])
  return addr


genBoolExpr :: Bool -> Expr -> GenM Address
genBoolExpr b expr = do
  labelTrue <- freshLabel
  labelFalse <- freshLabel
  labelEnd <- freshLabel

  emitIfThenElseBlocks expr labelTrue labelFalse
  sealBlock labelTrue
  sealBlock labelFalse

  setCurrentLabel labelTrue
  emitJump labelEnd

  setCurrentLabel labelFalse
  emitJump labelEnd

  sealBlock labelEnd
  setCurrentLabel labelEnd

  (addr, phiId) <- freshPhi labelEnd CBool
  let phi = APhi phiId CBool [(labelTrue, AImmediate $ EVBool b), (labelFalse, AImmediate $ EVBool $ not b)]
  modify $ \s -> s { getPhiEnv = Map.insert phiId phi (getPhiEnv s) }

  return addr


genBinOp :: ArithOp -> Expr -> Expr -> GenM Address
genBinOp op e1 e2 = do
  addr1 <- genExpr e1
  addr2 <- genExpr e2
  case getAddrType addr1 of
    CInt -> do
      addr <- freshReg (getAddrType addr1)
      emitInstr $ IBinOp addr addr1 op addr2
      return addr
    CString -> do
      addr <- freshReg (getAddrType addr1)
      emitInstr $ ICall addr (case op of
        OPlus -> "fun.internal.concatStrings"
        _ -> error "Not implemented"
        ) [addr1, addr2]
      return addr
    _ -> error "Not implemented"
genRelOp :: RelOp -> Expr -> Expr -> GenM Address
genRelOp op e1 e2 = do
  addr1 <- genExpr e1
  addr2 <- genExpr e2
  addr <- freshReg CBool
  case getAddrType addr1 of
    CString -> do
      case op of
        OEQU -> emitInstr $ ICall addr "fun.internal.compareStrings" [addr1, addr2]
        ONE -> do
          addr' <- freshReg CBool
          emitInstr $ ICall addr' "fun.internal.compareStrings" [addr1, addr2]
          emitInstr $ IBinOp addr addr' OMinus (AImmediate $ EVInt 1) -- TODO
        _ -> error "Not implemented"
    _ -> emitInstr $ IRelOp addr addr1 op addr2
  return addr

genLhs :: Expr -> GenM Address
genLhs (EVar ident) = do
  label <- getLabel
  readVar ident label
genLhs (EArrayElem expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  let t = getAddrType addr1
  let (CPtr (CStruct _ fields)) = t
  let t' = fields Map.! "data"
  addr <- freshReg t'
  emitInstr $ IGetElementPtr addr addr1 [AImmediate $ EVInt 0, AImmediate $ EVInt 1]
  let (CPtr t'') = t'
  addr' <- freshReg t'  -- TODO ?
  emitInstr $ ILoad addr' addr
  addr'' <- freshReg t'  -- TODO ?
  emitInstr $ IGetElementPtr addr'' addr' [addr2]
  return addr''

freshReg :: CType -> GenM Address
freshReg t = do
  n <- gets getRegCount
  modify $ \s -> s { getRegCount = n + 1 }
  return $ ARegister n t

freshLabel :: GenM String
freshLabel = do
  modify $ \s -> s { getLabelCount = getLabelCount s + 1 }
  label <- gets $ idToLabel . getLabelCount
  modify $ \s -> s {
    getBasicBlockEnv = Map.insert label (BasicBlock label [] Nothing [] Map.empty) (getBasicBlockEnv s),
    getCurrentFunLabels = label : getCurrentFunLabels s
  }
  return label

freshPhi :: Label -> CType -> GenM (Address, PhiID)
freshPhi label t = do
  block <- gets $ (Map.! label) . getBasicBlockEnv
  modify $ \s -> s { getPhiCount = getPhiCount s + 1 }
  phiId <- gets getPhiCount
  let phi = APhi phiId t []
  modify $ \s -> s { getPhiEnv = Map.insert phiId phi (getPhiEnv s) }
  newReg <- freshReg (getAddrType phi)
  addInstr label $ IPhi' newReg phiId
  return (newReg, phiId)

setCurrentLabel :: String -> GenM ()
setCurrentLabel label = do
  modify $ \s -> s { getCurrentLabel = label }
  return ()

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
  liftIO $ putStrLn $ "Reading " ++ ident ++ " from " ++ label
  venv <- gets getVEnv
  case Map.lookup ident venv of
    Just m -> do
      addr' <- case Map.lookup label m of
        Just addr -> return addr
        Nothing -> readVarRec ident label
      liftIO $ putStrLn $ "Read " ++ ident ++ " from " ++ label ++ " has type " ++ showAddrType addr'
      return addr'
    Nothing ->
      error $ "Variable " ++ ident ++ " not found in environment."

readVarRec :: Ident -> Label -> GenM Address
readVarRec ident label = do
  sealedBlocks <- gets getSealedBlocks
  liftIO $ putStrLn $ "Reading (rec) " ++ ident ++ " from " ++ label
  addr <- if label `notElem` sealedBlocks then do
        liftIO $ putStrLn $ "Block " ++ label ++ " not sealed"
        t <- gets $ (Map.! ident) . getVarType
        (addr', phiId) <- freshPhi label t
        modify $ \s -> s {
          getIncompletePhis = Map.insert label (Map.insert ident phiId (Map.findWithDefault Map.empty label (getIncompletePhis s))) (getIncompletePhis s)
        }
        return addr'
      else do
        b <- hasOnePred label
        if b then do
          liftIO $ putStrLn $ "Block " ++ label ++ " sealed and has one predecessor"
          blockEnv <- gets getBasicBlockEnv
          case Map.lookup label blockEnv of
            Nothing -> error $ "Block " ++ label ++ " not found in environment."
            _ -> return ()
          block <- gets $ (Map.! label) . getBasicBlockEnv
          liftIO $ putStrLn $ "Next reading " ++ ident ++ " from " ++ label ++ " with address " ++ show (getBlockInstrs block)
          readVar ident (head $ getBlockPredecessors block)
        else do
          liftIO $ putStrLn $ "Block " ++ label ++ " sealed and has more than one predecessor"
          t <- gets $ (Map.! ident) . getVarType
          (addr', phiId) <- freshPhi label t
          liftIO $ putStrLn $ "Created phi " ++ show phiId ++ " for " ++ ident ++ " in " ++ label ++ " with address " ++ show addr'
          writeVar ident label addr'
          addPhiOperands ident phiId label
          return addr'
  writeVar ident label addr
  return addr

addPhiOperands :: Ident -> PhiID -> Label -> GenM ()
addPhiOperands ident phiId label = do
  venv <- gets getVEnv
  block <- gets $ (Map.! label) . getBasicBlockEnv
  let preds = getBlockPredecessors block -- TODO czy nie powinno to iść rekurencyjnie?
  newOperands <- mapM (\pred -> do
    addr' <- readVar ident pred
    return (pred, addr')
    ) preds
  phi <- gets $ (Map.! phiId) . getPhiEnv
  let (APhi phiId t oldOperands) = phi
  let newPhi = APhi phiId t $ oldOperands ++ newOperands
  modify $ \s -> s { getPhiEnv = Map.insert phiId newPhi (getPhiEnv s) }

sealBlock :: Label -> GenM ()
sealBlock label = do
  incompletePhis <- gets $ Map.findWithDefault Map.empty label . getIncompletePhis
  mapM_ (\(var, phiId) -> do
      addPhiOperands var phiId label
    ) $ Map.toList incompletePhis
  modify $ \s -> s { getSealedBlocks = label : getSealedBlocks s }