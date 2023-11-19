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
      val <- runReaderT (runExceptT (checkProgram tree)) (Data.Map.empty, Data.Map.empty, Data.Map.empty)
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
type TEMonad = FMonad' (Maybe ExprVal)
type EMonad = FMonad' RType
type RType = (Type, Bool)  -- (Type, isAssignable)
data ExprVal = VInt Integer | VBool Bool
  deriving (Eq, Ord, Show, Read)

type Pos = BNFC'Position
data Error =
  ErrUnknownVariable IIdent
  | ErrUnknownFunction IIdent
  | ErrUnknownClass IIdent
  | ErrUnknownClassAttribute IIdent
  | ErrUnknownClassMethod IIdent
  | ErrDuplicateVariable IIdent
  | ErrDuplicateFunction IIdent
  | ErrDuplicateClass IIdent
  | ErrDuplicateClassAttribute IIdent
  | ErrDuplicateClassMethod IIdent
  | ErrDuplicateFunctionArgumentName IIdent
  | ErrWrongType Type Type -- (expected, got)
  | ErrWrongNumberOfArguments IIdent Int Int -- (function name, expected, got)
  | ErrWrongTypeOfArgument IIdent Int Type Type -- (function name, arg number, expected, got)
  | ErrVoidReturnValue Pos
  | ErrNoMain
  | ErrMultipleMain Pos
  | ErrWrongMainType Type
  | ErrMainNotAFunction Pos
  | ErrNotAssignable Type
  | ErrArithmeticInt Type
  | ErrBooleanOperation Type
  | ErrInequalityOperation Type
  | ErrIfElseBranchesNotReturningInEveryCase Pos
  | ErrWhileLoopNotReturningInEveryCase Pos
  | ErrIntegerOutOfRange Pos Integer
  | ErrDivisionByZero Pos
  | ErrCannotCastTo Type
  | ErrNotAnArray Type
  | ErrNotAClass Type
  | ErrNotAFunction Type
  | ErrVoidValue Pos
  | ErrFunctionValue Pos

-- TODO not a class i not a function chyba nie mają sensu, podobnie chyba errfunctionvalue

instance Show Error where
  show (ErrUnknownVariable ident) = "Unknown variable " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrUnknownFunction ident) = "Unknown function " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrUnknownClass ident) = "Unknown class " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrUnknownClassAttribute ident) = "Unknown class attribute " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrUnknownClassMethod ident) = "Unknown class method " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrDuplicateVariable ident) = "Duplicate variable " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrDuplicateFunction ident) = "Duplicate function " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrDuplicateClass ident) = "Duplicate class " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrDuplicateClassAttribute ident) = "Duplicate class attribute " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrDuplicateClassMethod ident) = "Duplicate class method " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrDuplicateFunctionArgumentName ident) = "Duplicate function argument name " ++ show ident ++ " at " ++ show (hasPosition ident)
  show (ErrWrongType t t') = "Wrong type " ++ show t' ++ " at " ++ show (hasPosition t) ++ ", expected " ++ show t
  show (ErrWrongNumberOfArguments ident n n') = "Wrong number of arguments " ++ show n' ++ " at " ++ show (hasPosition ident) ++ " of function " ++ show ident ++ ", expected " ++ show n
  show (ErrWrongTypeOfArgument ident n t t') = "Wrong type " ++ show t' ++ " at " ++ show (hasPosition ident) ++ " of argument " ++ show n ++ " of function " ++ show ident ++ ", expected " ++ show t
  show (ErrVoidReturnValue pos) = "Void return value at " ++ show pos
  show ErrNoMain = "No main function"
  show (ErrMultipleMain pos) = "Multiple main functions at " ++ show pos
  show (ErrWrongMainType t) = "Wrong main function type " ++ show t ++ " at " ++ show (hasPosition t) ++ ", expected int"
  show (ErrMainNotAFunction pos) = "Main is not a function at " ++ show pos
  show (ErrNotAssignable t) = "Not assignable type " ++ show t ++ " at " ++ show (hasPosition t)
  show (ErrArithmeticInt t) = "Arithmetic operation on type " ++ show t ++ " at " ++ show (hasPosition t) ++ ", expected int"
  show (ErrBooleanOperation t) = "Boolean operation on type " ++ show t ++ " at " ++ show (hasPosition t) ++ ", expected bool"
  show (ErrInequalityOperation t) = "Inequality operation on type " ++ show t ++ " at " ++ show (hasPosition t) ++ ", expected int"
  show (ErrIfElseBranchesNotReturningInEveryCase pos) = "If else branches don't return in every case with no return after at " ++ show pos
  show (ErrWhileLoopNotReturningInEveryCase pos) = "While loop doesn't return in every case with no return after at " ++ show pos
  show (ErrIntegerOutOfRange pos n) = "Integer out of range at " ++ show pos ++ ": " ++ show n
  show (ErrDivisionByZero pos) = "Division by zero at " ++ show pos
  show (ErrCannotCastTo t) = "Cannot cast to type " ++ show t ++ " at " ++ show (hasPosition t)
  show (ErrNotAnArray t) = "Not an array type " ++ show t ++ " at " ++ show (hasPosition t)
  show (ErrNotAClass t) = "Not a class type " ++ show t ++ " at " ++ show (hasPosition t)
  show (ErrNotAFunction t) = "Not a function type " ++ show t ++ " at " ++ show (hasPosition t)
  show (ErrVoidValue pos) = "Void value at " ++ show pos
  show (ErrFunctionValue pos) = "Function value at " ++ show pos

type VEnv = Map IIdent Type
type FEnv = Map IIdent Type
type CEnv = Map IIdent (ClassType, VEnv, FEnv)
type ClassType = ([ClassElem], Maybe IIdent)
type Env = (VEnv, FEnv, CEnv)
emptyEnv = (Data.Map.empty, Data.Map.empty)

checkProgram :: Program -> FMonad
checkProgram (PProgram _ topDefs) = do
  let idents = map getTopDefIdent topDefs
  checkNoDuplicateIdents idents ErrDuplicateFunction
  checkMain topDefs
  let fenv = functionDeclarationsToVEnv topDefs
  let cenv = classDeclarationsToCEnv topDefs
  let env = (Data.Map.empty, fenv, cenv)
  local (const env) (mapM_ checkTopDef topDefs)
  return Nothing

getTopDefIdent :: TopDef -> IIdent
getTopDefIdent (PFunDef _ _ ident _ _) = ident
getTopDefIdent (PClassDef _ ident _) = ident
getTopDefIdent (PClassDefExt _ ident _ _) = ident

sameIdent :: IIdent -> IIdent -> Bool
sameIdent (IIdent _ ident) (IIdent _ ident') = ident == ident'

checkNoDuplicateIdents :: [IIdent] -> (IIdent -> Error) -> FMonad
checkNoDuplicateIdents idents err = do
  if length idents == length (Data.List.nub idents) then return Nothing else
    let dup = head $ idents Data.List.\\ Data.List.nub idents in
    throwError $ err dup

checkMain :: [TopDef] -> FMonad
checkMain topDefs = do
  let mains = filter (\def -> sameIdent (getTopDefIdent def) (IIdent BNFC'NoPosition (Ident "main"))) topDefs
  when (null mains) $ throwError ErrNoMain
  when (length mains > 1) $ throwError $ ErrMultipleMain (hasPosition $ head mains)
  let main = head mains
  case main of
    PFunDef _ t ident args _ ->
      when (case t of {TInt _ -> False; _ -> True} || args /= []) $ throwError $ ErrWrongMainType mainType
      where
        mainType = TFun (hasPosition main) (TInt $ hasPosition main) []
    _ -> throwError $ ErrMainNotAFunction (hasPosition main)
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
      let funElems = filter (\elem -> case elem of {ClassMethodDef {} -> True; _ -> False}) elems
          varElems = filter (\elem -> case elem of {ClassAttrDef {} -> True; _ -> False}) elems
          venv = Data.Map.fromList $ zip (classElemsToIdents varElems) (classElemsToTypes varElems)
          fenv = Data.Map.fromList $ zip (classElemsToIdents funElems) (classElemsToTypes funElems)
      in
      (ident, ((elems, Nothing), venv, fenv))
    f (PClassDefExt _ ident ident' (ClassDef _ elems)) =
      f (PClassDef BNFC'NoPosition ident (ClassDef BNFC'NoPosition elems))
      -- TODO
    f _ = error "classDeclarationsToCEnv: impossible"

    isClassDef PClassDef {} = True
    isClassDef PClassDefExt {} = True
    isClassDef _ = False


checkTopDef :: TopDef -> FMonad
checkTopDef (PFunDef _ t ident args block) = do
  checkFunRetType t
  let argTypes = map (\(PArg _ t _) -> t) args
  mapM_ checkValType argTypes
  let argIdents = map (\(PArg _ _ ident) -> ident) args
  checkNoDuplicateIdents argIdents ErrDuplicateFunctionArgumentName
  let envFun = \(venv, fenv, cenv) -> (Data.Map.union venv $ Data.Map.fromList $ zip argIdents argTypes, fenv, cenv)
  mt' <- local envFun (checkBlock block t)
  case mt' of
    Just t' -> if sameType t t' then return Nothing else throwError $ ErrWrongType t t'
    Nothing -> if sameType t (TVoid $ hasPosition t) then return Nothing else throwError $ ErrWrongType t (TVoid BNFC'NoPosition)
checkTopDef (PClassDef _ ident (ClassDef _ elems)) = do
  let elemIdents = classElemsToIdents elems
  let elemTypes = classElemsToTypes elems
  mapM_ (uncurry tryInsertToEnv) (elemIdents `zip` elemTypes)
  let funElems = filter (\elem -> case elem of {ClassMethodDef {} -> True; _ -> False}) elems
  checkNoDuplicateIdents (classElemsToIdents funElems) ErrDuplicateClassMethod
  mapM_ checkFunRetType $ classElemsToTypes funElems
  let varElems = filter (\elem -> case elem of {ClassAttrDef {} -> True; _ -> False}) elems
  mapM_ checkValType $ classElemsToTypes varElems
  checkNoDuplicateIdents (classElemsToIdents varElems) ErrDuplicateClassAttribute
  let envClass = \(venv, fenv, cenv) -> (aux venv varElems, aux fenv funElems, cenv)
      aux env' elems' = Data.Map.union env' $ Data.Map.fromList $ zip (classElemsToIdents elems') (classElemsToTypes elems')
  local envClass (mapM_ checkClassElem elems)
  return Nothing
checkTopDef (PClassDefExt pos ident ident' (ClassDef pos' elems)) = do
  -- TODO
  checkTopDef (PClassDef pos ident (ClassDef pos' elems))

checkClassElem :: ClassElem -> FMonad
checkClassElem (ClassAttrDef _ t ident) = return Nothing
checkClassElem (ClassMethodDef pos t ident args block) = do
  checkTopDef (PFunDef pos t ident args block)

classElemsToIdents :: [ClassElem] -> [IIdent]
classElemsToIdents [] = []
classElemsToIdents ((ClassAttrDef _ t items):elems) = map (\(ClassItem _ ident) -> ident) items ++ classElemsToIdents elems
classElemsToIdents ((ClassMethodDef _ t ident args _):elems) = ident:classElemsToIdents elems

classElemsToTypes :: [ClassElem] -> [Type]
classElemsToTypes [] = []
classElemsToTypes ((ClassAttrDef _ t items):elems) = [t | _ <- items] ++ classElemsToTypes elems
classElemsToTypes ((ClassMethodDef pos t _ args _):elems) = TFun pos t (map (\(PArg _ t _) -> t) args):classElemsToTypes elems


checkBlock :: Block -> Type -> FMonad
checkBlock (SBlock _ stmts) = checkStmts stmts

checkStmts :: [Stmt] -> Type -> FMonad
checkStmts [] _ = return Nothing
checkStmts (SEmpty _:stmts) t = checkStmts stmts t
checkStmts (SBStmt _ block:stmts) t = do
  mt' <- checkBlock block t
  mt'' <- checkStmts stmts t
  case (mt', mt'') of
    (Just t', _) -> return $ Just t
    (_, Just t'') -> return $ Just t
    (Nothing, Nothing) -> return Nothing
checkStmts (SDecl _ t (item:items):stmts) t' = do
  tryInsertToEnv ident t
  checkValType t
  local (insertToEnv ident t) (checkStmts stmts' t')
  where
    stmts' = case item of
      SNoInit {} -> SDecl (hasPosition item) t items:stmts
      SInit pos _ expr -> SAss pos (EVar pos ident) expr:SDecl pos t items:stmts
    ident = getIdent item
    getIdent (SNoInit _ ident) = ident
    getIdent (SInit _ ident _) = ident
checkStmts (SDecl _ _ []:stmts) t = checkStmts stmts t
checkStmts (SAss _ expr expr':stmts) t'' = do
  (t, ass) <- checkExpr expr
  unless ass $ throwError $ ErrNotAssignable t
  (t', _) <- checkExpr expr'
  unless (sameType t t') $ throwError $ ErrWrongType t t'
  tryEvalExpr expr'
  checkStmts stmts t''
checkStmts (SIncr pos expr:stmts) t' = do
  (t, ass) <- checkExpr expr
  unless ass $ throwError $ ErrNotAssignable t
  unless (sameType t (TInt pos)) $ throwError $ ErrArithmeticInt t
  checkStmts stmts t'
checkStmts (SDecr pos expr:stmts) t' = do
  (t, ass) <- checkExpr expr
  unless ass $ throwError $ ErrNotAssignable t
  unless (sameType t (TInt pos)) $ throwError $ ErrArithmeticInt t
  checkStmts stmts t'
checkStmts (SRet pos expr:stmts) t = do
  when (sameType t (TVoid pos)) $ throwError $ ErrVoidReturnValue pos
  (t', _) <- checkExpr expr
  unless (sameType t t') $ throwError $ ErrWrongType t t'
  tryEvalExpr expr
  mt'' <- checkStmts stmts t
  case mt'' of
    Just t'' -> if sameType t t'' then return $ Just t else throwError $ ErrWrongType t t''
    Nothing -> return $ Just t
checkStmts (SVRet pos:stmts) t = do
  unless (sameType t (TVoid pos)) $ throwError $ ErrWrongType (TVoid $ hasPosition t) t
  mt'' <- checkStmts stmts t
  case mt'' of
    Just t'' -> if sameType t t'' then return $ Just t else throwError $ ErrWrongType (TVoid $ hasPosition t'') t''
    Nothing -> return $ Just t
checkStmts (SCond pos expr stmt:stmts) t = do
  (t', _) <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError $ ErrWrongType (TBool $ hasPosition t') t'
  mt1 <- checkStmts [stmt] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt'') of
        (_, Just _) -> return $ Just t
        _ -> throwError $ ErrIfElseBranchesNotReturningInEveryCase pos
    Just (VBool True) -> return mt1
    Just (VBool False) -> return Nothing
    _ -> error "checkStmts: impossible"
checkStmts (SCondElse pos expr stmt1 stmt2:stmts) t = do
  (t', _) <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError $ ErrWrongType (TBool $ hasPosition t') t'
  mt1 <- checkStmts [stmt1] t
  mt2 <- checkStmts [stmt2] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt2, mt'') of
        (Just _, Just _, _) -> return $ Just t
        (_, _, Just _) -> return $ Just t
        _ -> throwError $ ErrIfElseBranchesNotReturningInEveryCase pos
    Just (VBool True) -> return mt1
    Just (VBool False) -> return mt2
    _ -> error "checkStmts: impossible"
checkStmts (SWhile pos expr stmt:stmts) t = do
  (t', _) <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError $ ErrWrongType (TBool $ hasPosition t') t'
  mt1 <- checkStmts [stmt] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt'') of
        (_, Just _) -> return $ Just t
        _ -> throwError $ ErrWhileLoopNotReturningInEveryCase pos
    Just (VBool True) -> return mt1
    Just (VBool False) -> return mt''
    _ -> error "checkStmts: impossible"
checkStmts (SFor pos t' ident expr stmt:stmts) t = do
  (t'', _) <- checkExpr expr
  unless (sameType t'' (TArray pos t')) $ throwError $ ErrWrongType (TArray (hasPosition expr) t') t''
  tryInsertToEnv ident t'
  local (insertToEnv ident t') (checkStmts [stmt] t)
  checkStmts stmts t
checkStmts (SExp _ expr:stmts) t = do
  checkExpr expr
  tryEvalExpr expr
  checkStmts stmts t


maxInt = 2^31 - 1
minInt = -2^31
checkInt :: Pos -> Integer -> TEMonad
checkInt pos n = if minInt <= n && n <= maxInt then return $ Just $ VInt n else throwError $ ErrIntegerOutOfRange pos n

tryEvalExpr :: Expr -> TEMonad
tryEvalExpr (ELitInt pos n) = checkInt pos n
tryEvalExpr (ELitTrue pos) = return $ Just $ VBool True
tryEvalExpr (ELitFalse pos) = return $ Just $ VBool False
tryEvalExpr (ENeg pos expr) = do
  mn <- tryEvalExpr expr
  case mn of
    Just (VInt n) -> checkInt pos (-n)
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
    (Just (VInt n1), Just (VInt n2)) -> case op of
      OTimes _ -> checkInt pos $ n1 * n2
      ODiv _ -> if n2 /= 0 then checkInt pos $ n1 `div` n2 else throwError $ ErrDivisionByZero pos
      OMod _ -> if n2 /= 0 then checkInt pos $ n1 `mod` n2 else throwError $ ErrDivisionByZero pos
    _ -> return Nothing
tryEvalExpr (EAdd pos expr1 op expr2) = do
  mn1 <- tryEvalExpr expr1
  mn2 <- tryEvalExpr expr2
  case (mn1, mn2) of
    (Just (VInt n1), Just (VInt n2)) -> case op of
      OPlus _ -> checkInt pos $ n1 + n2
      OMinus _ -> checkInt pos $ n1 - n2
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


checkExpr :: Expr -> EMonad
checkExpr (EVar _ ident) = do
  (venv, _, _) <- ask
  case Data.Map.lookup ident venv of
    Just t -> return (t, True)
    Nothing -> throwError $ ErrUnknownVariable ident
checkExpr (ELitInt pos _) = return (TInt pos, False)
checkExpr (ELitTrue pos) = return (TBool pos, False)
checkExpr (ELitFalse pos) = return (TBool pos, False)
checkExpr (EString pos _) = return (TStr pos, False)
checkExpr (ECastNull pos t) = do
  case t of
    TClass _ ident -> do
      (venv, _, cenv) <- ask
      case Data.Map.lookup ident cenv of
        Just _ -> return (t, False)
        Nothing -> throwError $ ErrUnknownClass ident
    TArray {} -> return (t, False)
    _ -> throwError $ ErrCannotCastTo t
checkExpr (EArrayNew pos t expr) = do
  checkValType t
  (t', _) <- checkExpr expr
  unless (sameType t' (TInt pos)) $ throwError $ ErrWrongType (TInt $ hasPosition t') t'
  return (TArray pos t, False)
checkExpr (EArrayElem pos expr val) = do
  (t, _) <- checkExpr expr
  checkValType t
  (t', _) <- checkExpr val
  unless (sameType t' (TInt pos)) $ throwError $ ErrWrongType (TInt $ hasPosition t') t'
  case t of
    TArray _ t'' -> return (t'', True)
    _ -> throwError $ ErrNotAnArray t
checkExpr (EClassAttr pos expr ident) = do
  (t, _) <- checkExpr expr
  case t of
    TClass _ ident' -> do
      (_, _, cenv) <- ask
      case Data.Map.lookup ident' cenv of
        Just (_, cvenv, _) -> do
          case Data.Map.lookup ident cvenv of
            Just t -> return (t, True)
            Nothing -> throwError $ ErrUnknownClassAttribute ident
        Nothing -> throwError $ ErrUnknownClass ident'
    _ -> throwError $ ErrNotAClass t
checkExpr (EClassNew pos ident) = do
  (_, _, cenv) <- ask
  case Data.Map.lookup ident cenv of
    Just _ -> return (TClass pos ident, False)
    Nothing -> throwError $ ErrUnknownClass ident
checkExpr (EMethodCall pos expr ident exprs) = do
  (t, _) <- checkExpr expr
  case t of
    TClass _ ident' -> do
      (_, _, cenv) <- ask
      case Data.Map.lookup ident' cenv of
        Just (_, _, cfenv) -> do
          case Data.Map.lookup ident cfenv of
            Just (TFun pos' t' ts) -> do
              let methodEnv = (\(venv, fenv, cenv) -> (venv, cfenv, cenv))
              local methodEnv $ checkExpr (EFuntionCall pos ident exprs)
            Just _ -> throwError $ ErrNotAFunction t
            Nothing -> throwError $ ErrUnknownClassMethod ident
        Nothing -> throwError $ ErrUnknownClass ident'
    _ -> throwError $ ErrNotAClass t
checkExpr (EFuntionCall pos ident exprs) = do
  (_, fenv, _) <- ask
  case Data.Map.lookup ident fenv of
    Just (TFun _ t ts) -> do
      when (length ts /= length exprs) $ throwError $ ErrWrongNumberOfArguments ident (length ts) (length exprs)
      argTypes' <- mapM checkExpr exprs
      let argTypes = map fst argTypes'
      if all (== True) $ zipWith sameType argTypes ts then return (t, False) else
        throwError $ ErrWrongTypeOfArgument ident (length ts) (head ts) (head argTypes)
    _ -> throwError $ ErrUnknownFunction ident
checkExpr (ENeg pos expr) = do
  (t, _) <- checkExpr expr
  unless (sameType t (TInt pos)) $ throwError $ ErrArithmeticInt t
  return (t, False)
checkExpr (ENot pos expr) = do
  (t, _) <- checkExpr expr
  unless (sameType t (TBool pos)) $ throwError $ ErrBooleanOperation t
  return (t, False)
checkExpr (EMul pos expr1 op expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TInt pos)) $ throwError $ ErrArithmeticInt t1
  unless (sameType t2 (TInt pos)) $ throwError $ ErrArithmeticInt t2
  return (TInt pos, False)
checkExpr (EAdd pos expr1 op expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TInt pos)) $ throwError $ ErrArithmeticInt t1
  unless (sameType t2 (TInt pos)) $ throwError $ ErrArithmeticInt t2
  return (TInt pos, False)
checkExpr (ERel pos expr1 op expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 t2) $ throwError $ ErrWrongType t1 t2
  when (sameType t1 (TVoid pos)) $ throwError $ ErrVoidValue pos
  case op of
    OEQU {} -> return (TBool pos, False)
    ONE {} -> return (TBool pos, False)
    _ -> if sameType t1 (TInt pos) then return (TBool pos, False) else throwError $ ErrInequalityOperation t1
checkExpr (EAnd pos expr1 expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TBool pos)) $ throwError $ ErrBooleanOperation t1
  unless (sameType t2 (TBool pos)) $ throwError $ ErrBooleanOperation t2
  return (TBool pos, False)
checkExpr (EOr pos expr1 expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TBool pos)) $ throwError $ ErrBooleanOperation t1
  unless (sameType t2 (TBool pos)) $ throwError $ ErrBooleanOperation t2
  return (TBool pos, False)


checkValType :: Type -> FMonad' ()
checkValType (TArray _ t) = checkValType t
checkValType (TClass _ ident) = do
  (_, _, cenv) <- ask
  case Data.Map.lookup ident cenv of
    Just _ -> return ()
    Nothing -> throwError $ ErrUnknownClass ident
checkValType t@TFun {} = throwError $ ErrFunctionValue (hasPosition t)
checkValType (TVoid pos) = throwError $ ErrVoidValue pos
checkValType _ = return ()

checkFunRetType :: Type -> FMonad' ()
checkFunRetType arr@(TArray _ t) = checkValType arr
checkFunRetType (TClass _ ident) = do
  (_, _, cenv) <- ask
  case Data.Map.lookup ident cenv of
    Just _ -> return ()
    Nothing -> throwError $ ErrUnknownClass ident
checkFunRetType t@TFun {} = throwError $ ErrFunctionValue (hasPosition t)
checkFunRetType _ = return ()


tryInsertToEnv :: IIdent -> Type -> FMonad
tryInsertToEnv ident t = do
  (venv, fenv, _) <- ask
  case t of
    TFun {} -> when (Data.Map.member ident fenv) $ throwError $ ErrDuplicateFunction ident
    _ -> when (Data.Map.member ident venv) $ throwError $ ErrDuplicateVariable ident
  when (sameType t (TVoid $ hasPosition t)) $ throwError $ ErrVoidValue (hasPosition t)
  return Nothing

insertToEnv :: IIdent -> Type -> Env -> Env
insertToEnv ident t (venv, fenv, cenv) =
  case t of
    TFun {} -> (venv, Data.Map.insert ident t fenv, cenv)
    _ -> (Data.Map.insert ident t venv, fenv, cenv)


sameType :: Type -> Type -> Bool
sameType (TInt _) (TInt _) = True
sameType (TStr _) (TStr _) = True
sameType (TBool _) (TBool _) = True
sameType (TVoid _) (TVoid _) = True
sameType (TClass _ ident) (TClass _ ident') = ident == ident'
sameType (TArray _ t) (TArray _ t') = sameType t t'
sameType _ _ = False