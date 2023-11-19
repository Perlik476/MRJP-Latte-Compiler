-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- | Program to test parser.

module Main where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )
import Control.Monad      ( when )

import Latte.Abs
import Latte.Lex   ( Token, mkPosToken )
import Latte.Par   ( pProgram, myLexer )
import Latte.Print ( Print, printTree )
import Latte.Skel  ()

import Data.Map (Map, empty, fromList, union, member, lookup, insert, toList)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List
import Control.Exception (try)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV v s = when (v > 1) $ putStrLn s

runFile v p f = putStrLn f >> readFile f >>= run v p

run v p s =
  case p ts of
    Left err -> do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr "Parse failed."
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
      val <- runReaderT (runExceptT (checkProgram tree)) (Data.Map.empty, Data.Map.empty)
      case val of
        Right _ -> putStrLn "Frontend check successful."
        Left err -> do
          hPutStrLn stderr "ERROR"
          hPutStrLn stderr "Frontend check failed."
          hPutStrLn stderr $ "error: " ++ show err
          exitFailure
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> usage
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

type FMonad' a = ExceptT Error (ReaderT Env IO) a
type FMonad = FMonad' (Maybe Type)
type EMonad = FMonad' (Maybe ExprVal)
type LMonad = FMonad' (Maybe RType)
type RType = (Type, Bool)  -- (Type, isAssignable)
data ExprVal = VInt Integer | VBool Bool
  deriving (Eq, Ord, Show, Read)

type Error = String

type VEnv = Map Ident Type
type CEnv = Map Ident (ClassType, VEnv)
type ClassType = ([ClassElem], Maybe Ident)
type Env = (VEnv, CEnv)
emptyEnv = (Data.Map.empty, Data.Map.empty)

checkProgram :: Program -> FMonad
checkProgram (PProgram _ topDefs) = do
  let idents = map getTopDefIdent topDefs
  checkNoDuplicateIdents idents
  checkMain topDefs
  let venv = functionDeclarationsToVEnv topDefs
  let cenv = classDeclarationsToCEnv topDefs
  let env = (venv, cenv)
  local (const env) (mapM_ checkTopDef topDefs)
  return Nothing

getTopDefIdent :: TopDef -> Ident
getTopDefIdent (PFunDef _ _ ident _ _) = ident
getTopDefIdent (PClassDef _ ident _) = ident
getTopDefIdent (PClassDefExt _ ident _ _) = ident

checkNoDuplicateIdents :: [Ident] -> FMonad
checkNoDuplicateIdents idents = do
  if length idents == length (Data.List.nub idents) then return Nothing else throwError "Duplicate idents"

checkMain :: [TopDef] -> FMonad
checkMain topDefs = do
  let mains = filter (\def -> getTopDefIdent def == Ident "main") topDefs
  when (null mains) $ throwError "No main function"
  when (length mains > 1) $ throwError "More than one main function"
  case head mains of
    PFunDef _ t ident args _ ->
      when (case t of {TInt _ -> False; _ -> True} || args /= []) $ throwError "Wrong main function"
    _ -> throwError "Main is not a functions"
  return Nothing

functionDeclarationsToVEnv :: [TopDef] -> VEnv
functionDeclarationsToVEnv topDefs =
  Data.Map.fromList $ map (\(PFunDef pos t ident args _) -> (ident, TFun pos t $ map argToType args)) (filter isFunDef topDefs)
  where
    isFunDef PFunDef {} = True
    isFunDef _ = False
    argToType (PArg _ t _) = t

classDeclarationsToCEnv :: [TopDef] -> CEnv
classDeclarationsToCEnv topDefs =
  Data.Map.fromList $ map f (filter isClassDef topDefs)
  where
    f (PClassDef _ ident (ClassDef _ elems)) =
      let elemIdents = map getClassElemIdent elems
          elemTypes = map getClassElemType elems
          venv = Data.Map.fromList $ zip elemIdents elemTypes in
      (ident, ((elems, Nothing), venv))
    f (PClassDefExt _ ident ident' (ClassDef _ elems)) =
      -- TODO
      let elemIdents = map getClassElemIdent elems
          elemTypes = map getClassElemType elems
          venv = Data.Map.fromList $ zip elemIdents elemTypes in
      (ident, ((elems, Just ident'), venv))
    f _ = error "classDeclarationsToCEnv: impossible"

    isClassDef PClassDef {} = True
    isClassDef PClassDefExt {} = True
    isClassDef _ = False


checkTopDef :: TopDef -> FMonad
checkTopDef (PFunDef _ t ident args block) = do
  let argTypes = map (\(PArg _ t _) -> t) args
  let argIdents = map (\(PArg _ _ ident) -> ident) args
  let envFun = \(venv, cenv) -> (Data.Map.union venv $ Data.Map.fromList $ zip argIdents argTypes, cenv)
  mt' <- local envFun (checkBlock block t)
  case mt' of
    Just t' -> if sameType t t' then return Nothing else throwError "Wrong return type"
    Nothing -> if sameType t (TVoid $ hasPosition t) then return Nothing else throwError "Wrong return type"
checkTopDef (PClassDef _ ident (ClassDef _ elems)) = do
  let elemIdents = map getClassElemIdent elems
  checkNoDuplicateIdents elemIdents
  let elemTypes = map getClassElemType elems
  mapM_ (uncurry tryInsertToVEnv) (elemIdents `zip` elemTypes)
  let envClass = \(venv, cenv) -> (Data.Map.union venv $ Data.Map.fromList $ zip elemIdents elemTypes, cenv)
  local envClass (mapM_ checkClassElem elems)
  return Nothing
checkTopDef (PClassDefExt pos ident ident' (ClassDef pos' elems)) = do
  -- TODO
  checkTopDef (PClassDef pos ident (ClassDef pos' elems))

checkClassElem :: ClassElem -> FMonad
checkClassElem (ClassAttrDef _ t ident) = return Nothing
checkClassElem (ClassMethodDef pos t ident args block) = do
  checkTopDef (PFunDef pos t ident args block)

getClassElemIdent :: ClassElem -> Ident
getClassElemIdent (ClassAttrDef _ t ident) = ident
getClassElemIdent (ClassMethodDef _ t ident args _) = ident

getClassElemType :: ClassElem -> Type
getClassElemType (ClassAttrDef _ t _) = t
getClassElemType (ClassMethodDef pos t _ args _) = TFun pos t $ map (\(PArg _ t _) -> t) args


checkBlock :: Block -> Type -> FMonad
checkBlock (SBlock _ stmts) = checkStmts stmts

checkStmts :: [Stmt] -> Type -> FMonad
checkStmts [] _ = return Nothing
checkStmts (SEmpty _:stmts) t = checkStmts stmts t
checkStmts [SBStmt _ block] t = do
  checkBlock block t
checkStmts (SBStmt _ block:stmts) t = do
  error "SBStmt: impossible"
checkStmts (SDecl _ t (item:items):stmts) t' = do
  tryInsertToVEnv ident t
  local (insertToEnv ident t) (checkStmts stmts' t')
  where
    stmts' = case item of
      SNoInit {} -> SDecl (hasPosition item) t items:stmts
      SInit pos _ expr -> SAss pos (LVar pos ident) expr:SDecl pos t items:stmts
    ident = getIdent item
    getIdent (SNoInit _ ident) = ident
    getIdent (SInit _ ident _) = ident
checkStmts (SDecl _ _ []:stmts) t = checkStmts stmts t
checkStmts (SAss _ lvalue expr:stmts) t'' = do
  Just (t, ass) <- checkLvalue lvalue
  unless ass $ throwError "Not assignable"
  Just t' <- checkExpr expr
  unless (sameType t t') $ throwError "Wrong type"
  checkStmts stmts t''
checkStmts (SIncr pos lvalue:stmts) t' = do
  Just (t, ass) <- checkLvalue lvalue
  unless ass $ throwError "Not assignable"
  unless (sameType t (TInt pos)) $ throwError "Wrong type"
  checkStmts stmts t'
checkStmts (SDecr pos lvalue:stmts) t' = do
  Just (t, ass) <- checkLvalue lvalue
  unless ass $ throwError "Not assignable"
  unless (sameType t (TInt pos)) $ throwError "Wrong type"
  checkStmts stmts t'
checkStmts (SRet _ expr:stmts) t = do
  Just t' <- checkExpr expr
  unless (sameType t t') $ throwError "Wrong type"
  mt'' <- checkStmts stmts t
  case mt'' of
    Just t'' -> if sameType t t'' then return $ Just t else throwError "Wrong return type"
    Nothing -> return $ Just t
checkStmts (SVRet pos:stmts) t = do
  unless (sameType t (TVoid pos)) $ throwError "Wrong type"
  mt'' <- checkStmts stmts t
  case mt'' of
    Just t'' -> if sameType t t'' then return $ Just t else throwError "Wrong return type"
    Nothing -> return $ Just t
checkStmts (SCond pos expr stmt:stmts) t = do
  Just t' <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError "Wrong type"
  mt1 <- checkStmts [stmt] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt'') of
        (_, Just _) -> return $ Just t
        _ -> throwError "If branch doesn't return and there is no return after"
    Just (VBool True) -> return mt1
    Just (VBool False) -> return Nothing
    _ -> error "checkStmts: impossible"
checkStmts (SCondElse pos expr stmt1 stmt2:stmts) t = do
  Just t' <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError "Wrong type"
  mt1 <- checkStmts [stmt1] t
  mt2 <- checkStmts [stmt2] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt2, mt'') of
        (Just _, Just _, _) -> return $ Just t
        (_, _, Just _) -> return $ Just t
        _ -> throwError "If else branches don't return and there is no return after"
    Just (VBool True) -> return mt1
    Just (VBool False) -> return mt2
    _ -> error "checkStmts: impossible"
checkStmts (SWhile pos expr stmt:stmts) t = do
  Just t' <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError "Wrong type"
  mt1 <- checkStmts [stmt] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt'') of
        (_, Just _) -> return $ Just t
        _ -> throwError "No return after while loop with undetermined conditions"
    Just (VBool True) -> return mt1
    Just (VBool False) -> return mt''
    _ -> error "checkStmts: impossible"
checkStmts (SFor pos t' ident expr stmt:stmts) t = do
  Just t'' <- checkExpr expr
  unless (sameType t'' (TArray pos t')) $ throwError "Wrong type"
  tryInsertToVEnv ident t'
  local (insertToEnv ident t') (checkStmts [stmt] t)
  checkStmts stmts t
checkStmts (SExp _ expr:stmts) t = do
  checkExpr expr
  checkStmts stmts t

tryEvalExpr :: Expr -> EMonad
tryEvalExpr (ELitInt pos n) = return $ Just $ VInt n
tryEvalExpr (ELitTrue pos) = return $ Just $ VBool True
tryEvalExpr (ELitFalse pos) = return $ Just $ VBool False
tryEvalExpr (ENeg pos expr) = do
  mn <- tryEvalExpr expr
  case mn of
    Just (VInt n) -> return $ Just $ VInt (-n)
    _ -> return Nothing
tryEvalExpr (ENot pos expr) = do
  mb <- tryEvalExpr expr
  case mb of
    Just (VBool b) -> return $ Just $ VBool (not b)
    _ -> return Nothing
tryEvalExpr (EMul pos expr1 op expr2) = do
  mn1 <- tryEvalExpr expr1
  mn2 <- tryEvalExpr expr2
  case (mn1, mn2) of
    (Just (VInt n1), Just (VInt n2)) -> return $ Just $ VInt $ case op of
      OTimes _ -> n1 * n2
      ODiv _ -> n1 `div` n2
      OMod _ -> n1 `mod` n2
    _ -> return Nothing
tryEvalExpr (EAdd pos expr1 op expr2) = do
  mn1 <- tryEvalExpr expr1
  mn2 <- tryEvalExpr expr2
  case (mn1, mn2) of
    (Just (VInt n1), Just (VInt n2)) -> return $ Just $ VInt $ case op of
      OPlus _ -> n1 + n2
      OMinus _ -> n1 - n2
    _ -> return Nothing
tryEvalExpr (ERel pos expr1 op expr2) = do
  mn1 <- tryEvalExpr expr1
  mn2 <- tryEvalExpr expr2
  case (mn1, mn2) of
    (Just (VInt n1), Just (VInt n2)) -> return $ Just $ VBool $ case op of
      OLTH _ -> n1 < n2
      OLE _ -> n1 <= n2
      OGTH _ -> n1 > n2
      OGE _ -> n1 >= n2
      OEQU _ -> n1 == n2
      ONE _ -> n1 /= n2
    (Just (VBool b1), Just (VBool b2)) -> return $ Just $ VBool $ case op of
      OEQU _ -> b1 == b2
      ONE _ -> b1 /= b2
      _ -> error "tryEvalExpr: impossible"
    _ -> return Nothing
tryEvalExpr (EAnd pos expr1 expr2) = do
  mb1 <- tryEvalExpr expr1
  mb2 <- tryEvalExpr expr2
  case (mb1, mb2) of
    (Just (VBool b1), Just (VBool b2)) -> return $ Just $ VBool $ b1 && b2
    _ -> return Nothing
tryEvalExpr (EOr pos expr1 expr2) = do
  mb1 <- tryEvalExpr expr1
  mb2 <- tryEvalExpr expr2
  case (mb1, mb2) of
    (Just (VBool b1), Just (VBool b2)) -> return $ Just $ VBool $ b1 || b2
    _ -> return Nothing
tryEvalExpr _ = return Nothing


checkExpr :: Expr -> FMonad
checkExpr (EVar _ ident) = do
  (venv, cenv) <- ask
  case Data.Map.lookup ident venv of
    Just t -> return $ Just t
    Nothing -> throwError "Unknown ident"
checkExpr (ELitInt pos _) = return $ Just $ TInt pos
checkExpr (ELitTrue pos) = return $ Just $ TBool pos
checkExpr (ELitFalse pos) = return $ Just $ TBool pos
checkExpr (EString pos _) = return $ Just $ TStr pos
checkExpr (ECastNull pos t) = do
  case t of
    TClass _ ident -> do
      (venv, cenv) <- ask
      case Data.Map.lookup ident cenv of
        Just _ -> return $ Just t
        Nothing -> throwError "Unknown class"
    TArray {} -> return $ Just t
    _ -> throwError "Wrong type"
checkExpr (EArrayNew pos t expr) = do
  Just t' <- checkExpr expr  -- TODO check if expr is not nothing
  unless (sameType t' (TInt pos)) $ throwError "Wrong type"
  return $ Just $ TArray pos t
checkExpr (EArrayElem pos arrayElem) = do
  Just (t, _) <- checkLvalue (LArrayElem pos arrayElem)
  return $ Just t
checkExpr (EClassAttr pos (ClassAttr _ lvalue ident)) = do
  Just (t, ass) <- checkLvalue lvalue
  case t of
    TClass _ ident' -> do
      (venv, cenv) <- ask
      case Data.Map.lookup ident' cenv of
        Just (_, cvenv) -> do
          case Data.Map.lookup ident cvenv of
            Just t -> return $ Just t
            Nothing -> throwError "Unknown class attribute"
        Nothing -> throwError "Unknown class"
    _ -> throwError "Wrong type"
checkExpr (EClassNew pos ident) = do
  (_, cenv) <- ask
  case Data.Map.lookup ident cenv of
    Just _ -> return $ Just $ TClass pos ident
    Nothing -> throwError "Class not defined"
checkExpr (EMethodCall pos (MethodCall _ lvalue ident exprs)) = do
  Just (t, ass) <- checkLvalue lvalue
  case t of
    TClass _ ident' -> do
      (venv, cenv) <- ask
      case Data.Map.lookup ident' cenv of
        Just (_, cvenv) -> do
          case Data.Map.lookup ident cvenv of
            Just (TFun pos' t' ts) -> do
              let methodEnv = (\(venv, cenv) -> (cvenv, cenv))
              local methodEnv $ checkExpr (EFuntionCall pos (FunctionCall pos ident exprs))
            Just _ -> throwError "Not a method"
            Nothing -> throwError "Unknown class attribute"
        Nothing -> throwError "Unknown class"
    _ -> throwError "Wrong type"
checkExpr (EFuntionCall pos (FunctionCall _ ident exprs)) = do
  (venv, _) <- ask
  case Data.Map.lookup ident venv of
    Just (TFun _ t' ts) -> do
      when (length ts /= length exprs) $ throwError "Wrong number of arguments"
      margTypes <- mapM checkExpr exprs
      when (Nothing `elem` margTypes) $ throwError "Wrong type of arguments"
      let argTypes = map (\(Just t) -> t) margTypes
      if all (== True) $ zipWith sameType argTypes ts then return $ Just t' else throwError "Wrong type of arguments"
    _ -> throwError "Unknown function"
checkExpr (ENeg pos expr) = do
  Just t <- checkExpr expr
  unless (sameType t (TInt pos)) $ throwError "Wrong type"
  return $ Just t
checkExpr (ENot pos expr) = do
  Just t <- checkExpr expr
  unless (sameType t (TBool pos)) $ throwError "Wrong type"
  return $ Just t
checkExpr (EMul pos expr1 op expr2) = do
  Just t1 <- checkExpr expr1
  Just t2 <- checkExpr expr2
  unless (sameType t1 (TInt pos) && sameType t2 (TInt pos)) $ throwError "Wrong type"
  return $ Just $ TInt pos
checkExpr (EAdd pos expr1 op expr2) = do
  Just t1 <- checkExpr expr1
  Just t2 <- checkExpr expr2
  unless (sameType t1 (TInt pos) && sameType t2 (TInt pos)) $ throwError "Wrong type"
  return $ Just $ TInt pos
checkExpr (ERel pos expr1 op expr2) = do
  Just t1 <- checkExpr expr1
  Just t2 <- checkExpr expr2
  unless (sameType t1 t2 && not (sameType t1 (TVoid pos))) $ throwError "Wrong type"
  case op of
    OLTH {} -> return $ Just $ TBool pos
    OLE {} -> return $ Just $ TBool pos
    _ -> if sameType t1 (TInt pos) then return $ Just $ TBool pos else throwError "Only ints can be compared"
  return $ Just $ TBool pos
checkExpr (EAnd pos expr1 expr2) = do
  Just t1 <- checkExpr expr1
  Just t2 <- checkExpr expr2
  unless (sameType t1 (TBool pos) && sameType t2 (TBool pos)) $ throwError "Wrong type"
  return $ Just $ TBool pos
checkExpr (EOr pos expr1 expr2) = do
  Just t1 <- checkExpr expr1
  Just t2 <- checkExpr expr2
  unless (sameType t1 (TBool pos) && sameType t2 (TBool pos)) $ throwError "Wrong type"
  return $ Just $ TBool pos


checkLvalue :: Lvalue -> LMonad
checkLvalue (LVar _ ident) = do
  (venv, cenv) <- ask
  case Data.Map.lookup ident venv of
    Just t -> return $ Just (t, True)
    Nothing -> throwError "Unknown ident"
checkLvalue (LArrayElem _ (ArrayElem pos lvalue expr)) = do
  Just (t, _) <- checkLvalue lvalue
  Just t' <- checkExpr expr
  unless (sameType t' (TInt pos)) $ throwError "Wrong type"
  case t of
    TArray _ t'' -> return $ Just (t'', True)
    _ -> throwError "Wrong type"
checkLvalue (LClassAttr pos attr@(ClassAttr _ lvalue ident)) = do
  Just t <- checkExpr (EClassAttr pos attr)
  return $ Just (t, True)
checkLvalue (LMethodCall pos method@(MethodCall _ lvalue ident exprs)) = do
  Just t <- checkExpr (EMethodCall pos method)
  return $ Just (t, referenceType t)
checkLvalue (LFuntionCall pos fun) = do
  Just t <- checkExpr (EFuntionCall pos fun)
  return $ Just (t, referenceType t)

referenceType :: Type -> Bool
referenceType TArray {} = True
referenceType TClass {} = True
referenceType _ = False


tryInsertToVEnv :: Ident -> Type -> FMonad
tryInsertToVEnv ident t = do
  (venv, cenv) <- ask
  when (Data.Map.member ident venv) $ throwError "Duplicate ident"
  when (Data.Map.member ident cenv) $ throwError "Duplicate ident"
  when (sameType t (TVoid $ hasPosition t)) $ throwError "Void type"
  return Nothing

insertToEnv :: Ident -> Type -> Env -> Env
insertToEnv ident t (venv, cenv) = (Data.Map.insert ident t venv, cenv)


sameType :: Type -> Type -> Bool
sameType (TInt _) (TInt _) = True
sameType (TStr _) (TStr _) = True
sameType (TBool _) (TBool _) = True
sameType (TVoid _) (TVoid _) = True
sameType (TClass _ ident) (TClass _ ident') = ident == ident'
sameType (TArray _ t) (TArray _ t') = sameType t t'
sameType _ _ = False