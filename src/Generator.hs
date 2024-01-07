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


compile :: Options -> Program -> IO String
compile options ast =
  let initState = GenState {
    getCurrentLabel = "None",
    getCurrentFunLabels = [],
    getCurrentFunName = "NoneFun",
    getLabelCount = 0,
    getBasicBlockEnv = Map.fromList [("None", newBlock "None")],
    getFunctions = Map.empty,
    getVEnv = Map.empty,
    getRegCount = 0,
    getFEnv = Map.empty,
    getCEnv = Map.empty,
    getSealedBlocks = [],
    getIncompletePhis = Map.empty,
    getPhiCount = 0,
    getPhiToLabel = Map.empty,
    getAddrToPhi = Map.empty,
    getVarType = Map.empty,
    getStringPool = Map.empty,
    getStringPoolCount = 0,
    getInternalVarIdentCount = 0,
    getOptions = options
  } in do
    result <- runStateT (genProgram ast) initState
    let funs = Map.elems $ getFunctions $ snd result
    let stringPool = getStringPool $ snd result
    let cenv = getCEnv $ snd result
    let lines = [
          "declare i8* @calloc(i32, i32)",
          "declare void @fun.printInt(i32)",
          "declare i32 @fun.readInt()",
          "declare void @fun.printString(i8*)",
          "declare i8* @fun.readString()",
          "declare void @fun.error()",
          "declare i8* @fun.internal.concatStrings(i8*, i8*)",
          "declare i1 @fun.internal.compareStrings(i8*, i8*)",
          "declare i1 @fun.internal.compareStringsNeq(i8*, i8*)",
          ""
          ] ++
          map showClass (Map.toList cenv) ++
          [""] ++
          map showStrPool (Map.toList stringPool) ++
          [""] ++
          map show funs ++
          ["define i32 @main() {", "  %r = call i32 @fun.main()", "  ret i32 %r", "}"]
    return $ unlines lines



genProgram :: Program -> GenM ()
genProgram (PProgram topDefs) = do
  addStdLib
  mapM_ addClassToCEnv topDefs
  mapM_ addFunToFEnv topDefs
  mapM_ genTopDef topDefs
  funs <- gets getFunctions
  modify $ \s -> s { getFunctions = funs }


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


addClassToCEnv :: TopDef-> GenM ()
addClassToCEnv (PClassDef ident (ClassDef classItems)) = do
  let classFields = filter (\classItem -> case classItem of
        ClassAttrDef _ _ -> True
        _ -> False
        ) classItems
  classFields' <- mapM (\classItem -> case classItem of
    ClassAttrDef t ident' -> do
      t' <- case t of
            TArray _ -> CPtr <$> toCompType t
            TClass ident' -> return $ CPtr $ CClass ident'
            _ -> toCompType t
      return (ident', t')
    ) classFields
  let classType = CStruct (map fst classFields') $ Map.fromList classFields'
  modify $ \s -> s { getCEnv = Map.insert ident classType (getCEnv s) }
addClassToCEnv (PClassDefExt {}) = error "Inheritence not implemented"
addClassToCEnv _ = pure ()

addFunToFEnv :: TopDef -> GenM ()
addFunToFEnv (PFunDef t ident args block) = do
  funEntry <- freshLabel
  setCurrentLabel funEntry
  args' <- mapM (\(PArg t ident') -> do
    t' <- case t of
          TArray _ -> CPtr <$> toCompType t
          TClass ident' -> return $ CPtr $ CClass ident'
          _ -> toCompType t
    addr <- freshReg t'
    addVarAddr ident' addr
    addVarType ident' t'
    return (addr, t')
    ) args
  t' <- case t of
        TArray _ -> CPtr <$> toCompType t
        TClass ident' -> return $ CPtr $ CClass ident'
        _ -> toCompType t
  modify $ \s -> s {
    getFEnv = Map.insert ident (FunType funEntry t' args') (getFEnv s),
    getCurrentFunLabels = []
  }
addFunToFEnv _ = pure ()

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
  let t' = getFunTypeRet funType
  modify $ \s -> s {
    getCurrentFunLabels = [],
    getFunctions =
      Map.insert ident (FunBlock ident t' args' funBasicBlocks) (getFunctions s)
  }
  return ()
genTopDef (PClassDef ident (ClassDef classItems)) = pure ()

showArg :: Arg -> String
showArg (PArg t ident) = show t ++ " " ++ ident

genBlock :: Block -> GenM ()
genBlock (SBlock stmts) = mapM_ genStmt stmts



genStmt :: Stmt -> GenM ()
genStmt s = do
  addComments <- gets $ optComments . getOptions
  when addComments $ emitInstr $ IComment $ show s
  genStmt' s
  when addComments $ emitInstr $ IComment $ show s ++ " done"

genStmt' :: Stmt -> GenM ()
genStmt' SEmpty = pure ()
genStmt' (SExp expr) = do
  genExpr expr
  return ()
genStmt' (SBStmt block) = do
  genBlock block
  return ()
genStmt' (SDecl t ident expr) = do
  addr <- genExpr expr
  addVarAddr ident addr
  addVarType ident (getAddrType addr)
  printDebug $ "Declared " ++ ident ++ " with type " ++ show (getAddrType addr)
  return ()
genStmt' (SAss expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  case addr1 of
    AImmediate val -> do
      let ident = getVarName expr1
      addVarAddr ident addr2
    ARegister _ t1 -> do
      let t2 = getAddrType addr2
      printDebug $ "addr1 in SAss is " ++ show addr1 ++ " of type " ++ show t1
      printDebug $ "addr2 in SAss is " ++ show addr2 ++ " of type " ++ show t2
      if t1 == CPtr t2 then do -- TODO check if t1 and t2 are compatible (structs/classes)
        printDebug $ "emitting store " ++ show addr2 ++ " of type " ++ show t2 ++ " to " ++ show addr1 ++ " of type " ++ show t1
        emitInstr $ IStore addr2 addr1
      else do
        unless (t1 == t2) $ error "Types must match in assignment"
        let ident = getVarName expr1
        addVarAddr ident addr2
      printDebug $ "Assignment: " ++ show addr2 ++ " of type " ++ show t2 ++ " to " ++ show addr1 ++ " of type " ++ show t1
  return ()
genStmt' (SIncr expr) = genStmt' (SAss expr (EOp expr OPlus (ELitInt 1)))
genStmt' (SDecr expr) = genStmt' (SAss expr (EOp expr OMinus (ELitInt 1)))
genStmt' (SRet expr) = do
  addr <- genExpr expr
  emitRet addr
  return ()
genStmt' SVRet = do
  emitVRet
  return ()
genStmt' (SCondElse expr thenStmt elseStmt) = do
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
genStmt' (SCond expr thenStmt) = do
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
genStmt' (SWhile expr stmt) = do
  condLabel <- freshLabel
  bodyLabel <- freshLabel
  endLabel <- freshLabel

  emitJump condLabel
  setCurrentLabel condLabel
  emitIfThenElseBlocks expr bodyLabel endLabel
  sealBlock bodyLabel
  sealBlock endLabel

  setCurrentLabel bodyLabel
  genStmt stmt
  emitJumpIfNoTerminator condLabel
  sealBlock condLabel

  setCurrentLabel endLabel
  return ()
genStmt' (SFor t ident expr stmt) = do
  let iterIdent = "iter." ++ ident
  genStmt' (SDecl t iterIdent (AST.ELitInt 0))
  tempArrayIdent <- freshInternalVarIdent "temp.array"
  genStmt' (SDecl (AST.TArray t) tempArrayIdent expr)
  tempArrayLength <- freshInternalVarIdent "temp.array.length"
  genStmt' (SDecl AST.TInt tempArrayLength (AST.EClassAttr (AST.EVar tempArrayIdent) "attr.length"))
  genStmt' (SWhile (AST.ERel (AST.EVar iterIdent) AST.OLTH (AST.EVar tempArrayLength))
    (AST.SBStmt $ AST.SBlock [
      AST.SDecl t ident (AST.EArrayElem (AST.EVar tempArrayIdent) (AST.EVar iterIdent)),
      stmt,
      AST.SIncr (AST.EVar iterIdent)
    ])
    )


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
  printDebug $ "Trying to emit block " ++ label
  instrs <- getInstrs
  term <- getTerminator
  if label == "None" then
    return ()
  else do
    block <- getCurrentBasicBlock
    preds <- getPreds
    funName <- gets getCurrentFunName
    entryLabel <- gets $ getFunTypeEntryLabel . (Map.! funName) . getFEnv
    if label == entryLabel then do
      printDebug "Entry block"
      emitBasicBlock'
    else if null preds then do
      printDebug "No predecessors"
      skipEmitBasicBlock'
    else case term of
      Nothing -> do
        printDebug "No terminator"
        funRetType <- gets $ getFunTypeRet . (Map.! funName) . getFEnv
        if funRetType == CVoid then do
          printDebug "Void function"
          emitRet $ AImmediate EVVoid
        else do
          printDebug "Non-void function"
          emitRet $ AImmediate $ EVUndef funRetType
      _ -> emitBasicBlock'

emitBasicBlock' :: GenM ()
emitBasicBlock' = do
  label <- getLabel
  instrs <- getInstrs
  block <- getCurrentBasicBlock
  printDebug $ "Emitting block " ++ label
  printDebug $ "Block " ++ label ++ " has phis " ++ show (getBlockPhis block)
  printDebug $ "Block " ++ label ++ " has instructions " ++ show instrs
  modify $ \s -> s { getBasicBlockEnv = Map.insert label block (getBasicBlockEnv s), getCurrentLabel = "None" }

skipEmitBasicBlock' :: GenM ()
skipEmitBasicBlock' = do
  label <- getLabel
  instrs <- getInstrs
  printDebug $ "Skipping block " ++ label
  printDebug $ "No predecessors in block " ++ label ++ " with instructions " ++ show instrs
  modify $ \s -> s {
    getBasicBlockEnv = Map.delete label (getBasicBlockEnv s),
    getCurrentLabel = "None",
    getCurrentFunLabels = Data.List.delete label (getCurrentFunLabels s)
  }
  blocks <- gets getBasicBlockEnv
  printDebug $ "Blocks " ++ show (Map.keys blocks)
  let blocks'' = filter (\block -> label `elem` getBlockPredecessors block) $ Map.elems blocks
  printDebug "OK1"
  mapM_ (\block -> do
      printDebug $ "Processing block " ++ getBlockLabel block ++ " with predecessors " ++ show (getBlockPredecessors block)
      let preds = getBlockPredecessors block
      let preds' = Data.List.delete label preds
      printDebug $ "Now predecessors are " ++ show preds'
      let phis = getBlockPhis block
      let phis' = Map.map (\phi -> phi { getPhiOperands = filter (\(label', _) -> label' /= label) (getPhiOperands phi) }) phis
      printDebug $ "Now phis are " ++ show phis'
      modify $ \s -> s { getBasicBlockEnv = Map.insert (getBlockLabel block) (block { getBlockPhis = phis', getBlockPredecessors = preds' }) (getBasicBlockEnv s) }
    ) blocks''
  printDebug "OK end"


emitJump :: Label -> GenM ()
emitJump label = do
  addPredToBlock label =<< getLabel
  emitTerminator $ IJmp label
  emitBasicBlock

emitJumpIfNoTerminator :: Label -> GenM ()
emitJumpIfNoTerminator label = do
  currentLabel <- getLabel
  if currentLabel == "None" then
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
  emitIfThenElseBlocks expr1 interLabel elseLabel
  sealBlock interLabel
  setCurrentLabel interLabel
  emitIfThenElseBlocks expr2 thenLabel elseLabel
emitIfThenElseBlocks (EOr expr1 expr2) thenLabel elseLabel = do
  currentLabel <- getLabel
  interLabel <- freshLabel
  emitIfThenElseBlocks expr1 thenLabel interLabel
  sealBlock interLabel
  setCurrentLabel interLabel
  emitIfThenElseBlocks expr2 thenLabel elseLabel
emitIfThenElseBlocks ELitFalse thenLabel elseLabel = emitJump elseLabel >> return ()
emitIfThenElseBlocks ELitTrue thenLabel elseLabel = emitJump thenLabel >> return ()
emitIfThenElseBlocks expr thenLabel elseLabel = do
  addr <- genExpr expr
  emitBranch addr thenLabel elseLabel >> return ()


getVarName :: Expr -> String
getVarName (EVar ident) = ident
getVarName expr = error $ "Not a variable " ++ show expr

toCompType :: Type -> GenM CType
toCompType TInt = pure CInt
toCompType TBool = pure CBool
toCompType TVoid = pure CVoid
toCompType TStr = pure CString
toCompType (TArray t) = do
  ct <- toCompType t
  let ct' = case ct of
        CClass _ -> CPtr ct
        _ -> ct
  return $ CStruct ["attr.length", "attr.data"] $ Map.fromList [("attr.length", CInt), ("attr.data", CPtr ct')]
toCompType (TClass ident) = pure (CClass ident)


newStringConst :: String -> GenM Integer
newStringConst str = do
  stringPoolCount <- gets getStringPoolCount
  modify $ \s -> s { getStringPoolCount = stringPoolCount + 1, getStringPool = Map.insert str stringPoolCount (getStringPool s) }
  return stringPoolCount


genExpr :: Expr -> GenM Address
genExpr expr = do
  addComments <- gets $ optComments . getOptions
  when addComments $ emitInstr $ IComment $ "Generating genExpr " ++ show expr
  addr <- genExpr' expr
  when addComments $ emitInstr $ IComment $ "Generating genExpr " ++ show expr ++ " done"
  return addr

genExpr' :: Expr -> GenM Address
genExpr' (ELitInt n) = return $ AImmediate $ EVInt n
genExpr' ELitTrue = return $ AImmediate $ EVBool True
genExpr' ELitFalse = return $ AImmediate $ EVBool False
genExpr' (EString str) = do
  case str of
    "" -> return $ AImmediate $ EVNull CString
    _ -> do
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
genExpr' (EVar ident) = do
  label <- getLabel
  readVar ident label
genExpr' (EFunctionCall ident exprs) = do
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
genExpr' (ENeg expr) = genExpr (EOp (ELitInt 0) OMinus expr)
genExpr' (ENot expr) = genBoolExpr False expr
genExpr' (EOp expr1 op expr2) = genBinOp op expr1 expr2
genExpr' (ERel expr1 op expr2) = genRelOp op expr1 expr2
genExpr' expr@(EAnd _ _) = genBoolExpr True expr
genExpr' expr@(EOr _ _) = genBoolExpr True expr
genExpr' (ECastNull t) = AImmediate . EVNull . CPtr <$> toCompType t
genExpr' (EArrayNew t expr) = do
  addr <- genExpr expr
  let t' = TArray t
  ct' <- toCompType t'
  let (CStruct _ fields) = ct'
  let ct = fields Map.! "attr.data"
  addr' <- freshReg CInt
  emitInstr $ IBinOp addr' addr OTimes (AImmediate $ EVInt $ getTypeSize ct)
  addr'' <- genAllocate ct addr'
  genStruct (CPtr ct') ["attr.length", "attr.data"] $ Map.fromList [("attr.length", addr), ("attr.data", addr'')]
genExpr' (EArrayElem expr1 expr2) = do
  addr1 <- genExpr expr1
  addr2 <- genExpr expr2
  t <- getStructPtrFromClassPtr $ getAddrType addr1
  let (CPtr (CStruct _ fields)) = t
  let t' = CPtr $ fields Map.! "attr.data"
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
genExpr' (EClassAttr expr ident) = do
  addr <- genExpr expr
  t <- getStructPtrFromClassPtr $ getAddrType addr
  let (CPtr (CStruct fieldNames fields)) = t
  let t' = CPtr $ fields Map.! ident
  let (Just fieldNum) = Data.List.elemIndex ident fieldNames
  addr' <- freshReg t'
  emitInstr $ IGetElementPtr addr' addr [AImmediate $ EVInt 0, AImmediate $ EVInt $ toInteger fieldNum]
  let (CPtr t'') = t'
  addr'' <- freshReg t''
  emitInstr $ ILoad addr'' addr'
  return addr''
genExpr' (EClassNew ident) = do
  cenv <- gets getCEnv
  let classType@(CStruct fieldNames fields) = cenv Map.! ident
  let t = CPtr $ CClass ident
  genStruct t fieldNames Map.empty


getStructPtrFromClassPtr :: CType -> GenM CType
getStructPtrFromClassPtr (CPtr (CClass ident)) = do
  cenv <- gets getCEnv
  return $ CPtr $ cenv Map.! ident
getStructPtrFromClassPtr t@(CPtr (CStruct _ _)) = pure t
getStructPtrFromClassPtr t = error $ "Cannot get class from type " ++ show t

genDereferencePtrIfDoublePtr :: Address -> GenM Address
genDereferencePtrIfDoublePtr addr = do
  let (CPtr t) = getAddrType addr
  case t of
    CPtr _ -> genDereferencePtr addr
    _ -> return addr

genDereferencePtr :: Address -> GenM Address
genDereferencePtr addr = do
  let (CPtr t) = getAddrType addr
  addr' <- freshReg t
  emitInstr $ ILoad addr' addr
  return addr'

genAllocate :: CType -> Address -> GenM Address
genAllocate t sizeAddr = do
  -- assuming t is a pointer type
  addr <- freshReg (CPtr CChar)
  emitInstr $ ICall addr "calloc" [AImmediate $ EVInt 1, sizeAddr]
  addr' <- freshReg t
  emitInstr $ IBitcast addr' addr
  return addr'

genStruct :: CType -> [String] -> Map String Address -> GenM Address
genStruct t fieldNames fields = do
  -- assuming t is a pointer type
  addr <- genAllocate t (AImmediate $ EVInt $ getTypeSize t)
  mapM_ (\(name, n) -> do
    case Map.lookup name fields of
      Just addr' -> do
        let t' = getAddrType addr'
        addr'' <- freshReg (CPtr t')
        emitInstr $ IGetElementPtr addr'' addr [AImmediate $ EVInt 0, AImmediate $ EVInt $ toInteger n]
        emitInstr $ IStore addr' addr''
      Nothing -> return ()
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

  phi <- freshPhi labelEnd CBool
  let phi' = phi { getPhiOperands = [(labelTrue, AImmediate $ EVBool b), (labelFalse, AImmediate $ EVBool $ not b)] }
  labelEndBlock <- gets $ (Map.! labelEnd) . getBasicBlockEnv
  updateBlockPhi labelEnd (getPhiId phi) phi'
  return $ getPhiAddr phi


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
        ONE -> emitInstr $ ICall addr "fun.internal.compareStringsNeq" [addr1, addr2]
        _ -> error "Not implemented"
    _ -> emitInstr $ IRelOp addr addr1 op addr2
  return addr

genLhs :: Expr -> GenM Address
genLhs expr = do
  addComments <- gets $ optComments . getOptions
  when addComments $ emitInstr $ IComment $ "Generating genLhs " ++ show expr
  addr <- genLhs' expr
  when addComments $ emitInstr $ IComment $ "Generating genLhs " ++ show expr ++ " done"
  return addr

genLhs' :: Expr -> GenM Address
genLhs' (EVar ident) = do
  label <- getLabel
  readVar ident label
genLhs' (EArrayElem expr1 expr2) = do
  addr1 <- genLhs expr1
  addr2 <- genExpr expr2
  addr1' <- genDereferencePtrIfDoublePtr addr1
  t <- getStructPtrFromClassPtr $ getAddrType addr1'
  printDebug $ "Generating genLhs EArrayElem" ++ show t
  let (CPtr (CStruct _ fields)) = t
  let t' = fields Map.! "attr.data"
  addr <- freshReg t'
  emitInstr $ IGetElementPtr addr addr1' [AImmediate $ EVInt 0, AImmediate $ EVInt 1]
  let (CPtr t'') = t'
  addr' <- freshReg t'
  emitInstr $ ILoad addr' addr
  addr'' <- freshReg t'
  emitInstr $ IGetElementPtr addr'' addr' [addr2]
  return addr''
genLhs' (EClassAttr expr ident) = do
  addr <- genLhs expr
  addr' <- genDereferencePtrIfDoublePtr addr
  t <- getStructPtrFromClassPtr $ getAddrType addr'
  printDebug $ "Generating genLhs EClassAttr " ++ ident ++ " of expr type " ++ show t
  let (CPtr (CStruct fieldNames fields)) = t
  let t' = fields Map.! ident
  let (Just fieldNum) = Data.List.elemIndex ident fieldNames
  addr'' <- freshReg $ CPtr t'
  emitInstr $ IGetElementPtr addr'' addr' [AImmediate $ EVInt 0, AImmediate $ EVInt $ toInteger fieldNum]
  return addr''
genLhs' (EFunctionCall ident exprs) = genExpr (EFunctionCall ident exprs)
genLhs' (EArrayNew t expr) = genExpr (EArrayNew t expr)
genLhs' (ECastNull t) = genExpr (ECastNull t)
genLhs' (EClassNew ident) = genExpr (EClassNew ident)
genLhs' expr = error $ "Not an lvalue: " ++ show expr


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
    getBasicBlockEnv = Map.insert label (newBlock label) (getBasicBlockEnv s),
    getCurrentFunLabels = label : getCurrentFunLabels s
  }
  return label

freshPhi :: Label -> CType -> GenM Phi
freshPhi label t = do
  block <- gets $ (Map.! label) . getBasicBlockEnv
  modify $ \s -> s { getPhiCount = getPhiCount s + 1 }
  phiId <- gets getPhiCount
  newReg <- freshReg t
  let phi = Phi newReg phiId t []
  modify $ \s -> s { getPhiToLabel = Map.insert phiId label (getPhiToLabel s) }
  updateBlockPhi label phiId phi
  return phi

updateBlockPhi :: Label -> PhiID -> Phi -> GenM ()
updateBlockPhi label phiId phi = do
  block <- gets $ (Map.! label) . getBasicBlockEnv
  printDebug $ "Updating block " ++ label ++ " with phi " ++ show phiId ++ " with address " ++ show (getPhiAddr phi)
  modify $ \s -> s {
    getBasicBlockEnv = Map.insert label (block { getBlockPhis = Map.insert phiId phi (getBlockPhis block) }) (getBasicBlockEnv s)
  }

freshInternalVarIdent :: Ident -> GenM Ident
freshInternalVarIdent ident = do
  n <- gets getInternalVarIdentCount
  modify $ \s -> s { getInternalVarIdentCount = getInternalVarIdentCount s + 1 }
  return $ ident ++ "." ++ show n

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

-- The following code is based on the algorithm described in
-- https://link.springer.com/content/pdf/10.1007/978-3-642-37051-9_6.pdf

writeVar :: Ident -> Label -> Address -> GenM ()
writeVar ident label addr = do
  venv <- gets getVEnv
  printDebug $ "Writing " ++ ident ++ " to " ++ label ++ " with address " ++ show addr
  printDebug $ "VEnv before is " ++ show venv
  case Map.lookup ident venv of
    Just m -> modify $ \s -> s { getVEnv = Map.insert ident (Map.insert label addr m) venv }
    Nothing -> modify $ \s -> s { getVEnv = Map.insert ident (Map.singleton label addr) venv }
  printDebug $ "VEnv after is " ++ show venv


readVar :: Ident -> Label -> GenM Address
readVar ident label = do
  printDebug $ "Reading " ++ ident ++ " from " ++ label
  venv <- gets getVEnv
  case Map.lookup ident venv of
    Just m -> do
      addr' <- case Map.lookup label m of
        Just addr -> return addr
        Nothing -> readVarRec ident label
      printDebug $ "Read " ++ ident ++ " from " ++ label ++ " has type " ++ showAddrType addr' ++ " and address " ++ show addr'
      return addr'
    Nothing ->
      error $ "Variable " ++ ident ++ " not found in environment."

readVarRec :: Ident -> Label -> GenM Address
readVarRec ident label = do
  sealedBlocks <- gets getSealedBlocks
  printDebug $ "Reading (rec) " ++ ident ++ " from " ++ label
  addr <- if label `notElem` sealedBlocks then do
        printDebug $ "Block " ++ label ++ " not sealed"
        t <- gets $ (Map.! ident) . getVarType
        phi <- freshPhi label t
        modify $ \s -> s {
          getIncompletePhis = Map.insert label (Map.insert ident (getPhiId phi) (Map.findWithDefault Map.empty label (getIncompletePhis s))) (getIncompletePhis s)
        }
        return $ getPhiAddr phi
      else do
        b <- hasOnePred label
        if b then do
          printDebug $ "Block " ++ label ++ " sealed and has one predecessor"
          blockEnv <- gets getBasicBlockEnv
          case Map.lookup label blockEnv of
            Nothing -> error $ "Block " ++ label ++ " not found in environment."
            _ -> return ()
          block <- gets $ (Map.! label) . getBasicBlockEnv
          printDebug $ "Next reading " ++ ident ++ " from " ++ label ++ " with address " ++ show (getBlockInstrs block)
          readVar ident (head $ getBlockPredecessors block)
        else do
          printDebug $ "Block " ++ label ++ " sealed and has more than one predecessor"
          t <- gets $ (Map.! ident) . getVarType
          phi <- freshPhi label t
          printDebug $ "Created phi " ++ show (getPhiId phi) ++ " for " ++ ident ++ " in " ++ label ++ " with address " ++ show (getPhiAddr phi)
          writeVar ident label (getPhiAddr phi)
          addr' <- addPhiOperands ident (getPhiId phi) label
          venv <- gets getVEnv
          printDebug $ "VEnv is " ++ show venv
          return addr'
  writeVar ident label addr
  return addr

addPhiOperands :: Ident -> PhiID -> Label -> GenM Address
addPhiOperands ident phiId label = do
  venv <- gets getVEnv
  block <- gets $ (Map.! label) . getBasicBlockEnv
  let preds = getBlockPredecessors block
  newOperands <- mapM (\pred -> do
    addr' <- readVar ident pred
    return (pred, addr')
    ) preds
  let phi = getBlockPhis block Map.! phiId
  let oldOperands = getPhiOperands phi
  let newPhi = phi { getPhiOperands = oldOperands ++ newOperands }
  updateBlockPhi label phiId newPhi
  tryRemoveTrivialPhi newPhi


tryRemoveTrivialPhi :: Phi -> GenM Address
tryRemoveTrivialPhi phi = do
  optRemoveTrivialPhis <- gets (optRemoveTrivialPhis . getOptions)
  if not optRemoveTrivialPhis then
    return $ getPhiAddr phi
  else do
    let operandsWithoutSelf = filter (\(_, addr) -> addr /= getPhiAddr phi) $ getPhiOperands phi
    let uniqueOperandsWithoutSelf = Data.List.nubBy (\(_, addr1) (_, addr2) -> addr1 == addr2) operandsWithoutSelf
    if length uniqueOperandsWithoutSelf == 1 then do
      printDebug $ "Removing trivial phi " ++ show (getPhiId phi) ++ " with addr " ++ show (getPhiAddr phi) ++ " and operands " ++ show (getPhiOperands phi) ++ " and unique operands " ++ show uniqueOperandsWithoutSelf
      let addr = snd $ head uniqueOperandsWithoutSelf
      blockEnv <- gets getBasicBlockEnv
      allPhis <- foldM (\acc block -> do
          let phis = getBlockPhis block
          return $ acc ++ Map.elems phis
        ) [] (Map.elems blockEnv)
      phiUses <- foldM (\acc phi' -> do
          let phiId = getPhiId phi'
          let operands = getPhiOperands phi'
          let phis = filter (\(_, addr') -> addr' == getPhiAddr phi) operands
          mapM (\(_, addr') -> do
              let phiId' = getPhiId phi'
              return phiId'
            ) phis
        ) [] allPhis
      phiBlockLabel <- gets $ (Map.! getPhiId phi) . getPhiToLabel
      phiBlock <- gets $ (Map.! phiBlockLabel) . getBasicBlockEnv
      replacePhiByAddr phi addr
      mapM_ (\phiId -> do
          printDebug $ "Processing phi " ++ show phiId
          mPhi <- tryGetPhi phiId
          case mPhi of
            Nothing -> return ()
            Just phi' -> do
              tryRemoveTrivialPhi phi'
              return ()
        ) phiUses
      printDebug $ "Removed trivial phi " ++ show (getPhiId phi) ++ ", returning addr " ++ show addr
      return addr
    else
      return $ getPhiAddr phi

replacePhiByAddr :: Phi -> Address -> GenM ()
replacePhiByAddr phi addr = do
  printDebug $ "Replacing phi " ++ show (getPhiId phi) ++ " with addr " ++ show (getPhiAddr phi) ++ " by addr " ++ show addr
  venv <- gets getVEnv
  printDebug $ "VEnv is " ++ show venv
  blockEnv <- gets getBasicBlockEnv
  let phiId = getPhiId phi
  label <- gets $ (Map.! phiId) . getPhiToLabel
  block <- gets $ (Map.! label) . getBasicBlockEnv
  printDebug $ "Phi block " ++ label ++ " has instrs " ++ show (getBlockInstrs block)
  mapM_ (\block' -> do
      let label' = getBlockLabel block'
      printDebug $ "Replacing phi " ++ show (getPhiId phi) ++ " with addr " ++ show (getPhiAddr phi) ++ " by addr " ++ show addr ++ " in block " ++ label'
      let instrs = getBlockInstrs block'
      printDebug $ "Block " ++ label' ++ " has instrs " ++ show instrs
      instrs' <- mapM (\instr' -> do
          printDebug $ "Replacing instr " ++ show instr'
          let instr'' = replaceAddrByAddrInInstr (getPhiAddr phi) addr instr'
          printDebug $ "By instr " ++ show instr''
          return instr''
        ) instrs
      modify $ \s -> s {
        getBasicBlockEnv = Map.insert label' (block' { getBlockInstrs = instrs' }) (getBasicBlockEnv s)
      }
      block'' <- gets $ (Map.! label') . getBasicBlockEnv
      let instrs'' = getBlockInstrs block''
      printDebug $ "Block " ++ label' ++ " now has instrs " ++ show instrs''
    ) blockEnv
  allPhis <- foldM (\acc block -> do
      let phis = getBlockPhis block
      return $ acc ++ Map.elems phis
    ) [] (Map.elems blockEnv)
  phiUses <- foldM (\acc phi' -> do
      let phiId = getPhiId phi'
      let operands = getPhiOperands phi'
      let phis = filter (\(_, addr') -> addr' == getPhiAddr phi) operands
      mapM (\(_, addr') -> do
          let phiId' = getPhiId phi'
          return phiId'
        ) phis
    ) [] allPhis
  printDebug $ "Phi addr " ++ show (getPhiAddr phi) ++ " has phi uses " ++ show phiUses
  mapM_ (\phiId' -> do
      label' <- gets $ (Map.! phiId') . getPhiToLabel
      printDebug $ "Replacing phi " ++ show (getPhiId phi) ++ " with addr " ++ show (getPhiAddr phi) ++ " by addr " ++ show addr ++ " in block " ++ label' ++ " at phi " ++ show phiId'
      block <- gets $ (Map.! label') . getBasicBlockEnv
      printDebug $ "Block " ++ label' ++ " has phis " ++ show (getBlockPhis block)
      let phis = getBlockPhis block
      case Map.lookup phiId' phis of
        Nothing -> printDebug $ "Phi " ++ show phiId' ++ " not found in block " ++ label'
        Just phi' -> do
          printDebug $ "Phi " ++ show phiId' ++ " had operands " ++ show (getPhiOperands phi')
          newOperands <- mapM (\(label'', addr'') -> do
                printDebug $ "addr'' is " ++ show addr'' ++ " and phi addr is " ++ show (getPhiAddr phi)
                if addr'' == getPhiAddr phi then do
                  printDebug "addr'' == phi addr"
                  return (label'', addr)
                else
                  return (label'', addr'')
              ) (getPhiOperands phi')
          let phi'' = phi' { getPhiOperands = newOperands }
          printDebug $ "Updated phi " ++ show phiId' ++ " has operands " ++ show (getPhiOperands phi'')
          updateBlockPhi label' phiId' phi''
          phi''' <- getPhi phiId'
          printDebug $ "Phi " ++ show phiId' ++ " now has operands " ++ show (getPhiOperands phi''')
        ) $ filter (/= phiId) phiUses
  block' <- gets $ (Map.! label) . getBasicBlockEnv
  let phis = getBlockPhis block'
  let phis' = Map.delete phiId phis
  printDebug $ "Phi block " ++ label ++ " before has phis " ++ show (getBlockPhis block')
  modify $ \s -> s {
    getBasicBlockEnv = Map.insert label (block' { getBlockPhis = phis' }) (getBasicBlockEnv s),
    getPhiToLabel = Map.delete phiId (getPhiToLabel s),
    getIncompletePhis = Map.map (Map.filter (/= getPhiId phi)) (getIncompletePhis s)
  }
  block'' <- gets $ (Map.! label) . getBasicBlockEnv
  printDebug $ "Phi block " ++ label ++ " after has phis " ++ show (getBlockPhis block'')
  -- change address used in venv
  venv <- gets getVEnv
  printDebug $ "VEnv before is " ++ show venv
  let venv' = Map.map (Map.map (\addr' -> if addr' == getPhiAddr phi then addr else addr')) venv
  printDebug $ "VEnv after is " ++ show venv'
  modify $ \s -> s { getVEnv = venv' }

sealBlock :: Label -> GenM ()
sealBlock label = do
  preds <- getBlockPredecessors <$> gets ((Map.! label) . getBasicBlockEnv)
  printDebug $ "Sealing block " ++ label ++ " with preds " ++ show preds
  incompletePhis <- gets $ Map.findWithDefault Map.empty label . getIncompletePhis
  printDebug $ "Incomplete phis are " ++ show incompletePhis
  mapM_ (\(var, phiId) -> do
      addPhiOperands var phiId label
    ) $ Map.toList incompletePhis
  modify $ \s -> s { getSealedBlocks = label : getSealedBlocks s }
