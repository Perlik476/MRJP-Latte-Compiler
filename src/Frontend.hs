module Frontend where

import Prelude
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import System.IO          ( hPutStrLn, stderr, hPutStr )
import Control.Monad      ( when )

import Latte.Abs
import Latte.Lex   ( Token, mkPosToken )
import Latte.Par   ( pProgram, myLexer )
import Latte.Print ( Print, printTree )
import Latte.Skel  ()

import Data.Map (Map, empty, fromList, union, member, lookup, insert, toList, keys, difference, intersection, elems, (!), intersectionWith)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

getParseErrPosition :: String -> Pos
getParseErrPosition s = case words s of
  ("syntax":"error":"at":"line":l_comma:"column":c:"before":_) ->
    let l = takeWhile (/= ',') l_comma in
    BNFC'Position (read l) (read c)
  _ -> BNFC'NoPosition

showCode :: String -> Pos -> String
showCode s (Just (l,c)) =
  getLines (min 3 l) (max 0 (l - 3)) ++
  replicate (c + maxLineNumberLenght) ' ' ++ "^\n" ++
  getLines (min 3 (length s - l)) l
  where getLines k from = unlines (map (
          \(n, s) ->
            show n
            ++ replicate (maxLineNumberLenght - length (show n)) ' '
            ++ "|"
            ++ s
          ) (take k $ zip [(from+1)..] $ drop from $ lines $ convertTabTo8Spaces s))
        maxLineNumberLenght = length $ show $ l + 3
showCode _ Nothing = ""

convertTabTo8Spaces :: String -> String
convertTabTo8Spaces [] = []
convertTabTo8Spaces ('\t':s) = replicate 8 ' ' ++ convertTabTo8Spaces s
convertTabTo8Spaces (c:s) = c:convertTabTo8Spaces s

type FMonad' a = ExceptT Error (ReaderT Env IO) a
type FMonad = FMonad' (Maybe Type)
type TEMonad = FMonad' (Maybe ExprVal)
type EMonad = FMonad' RType
type RType = (Type, Bool)  -- (Type, isAssignable)
data ExprVal = VInt Integer | VBool Bool | VStr String
  deriving (Eq, Ord, Show, Read)

type VEnv = Map String (Type, Integer)
type FEnv = Map String Type
type CEnv = Map String ClassData
data ClassData = ClassData
  {
    getClassElems :: [ClassElem],
    getExtends :: Maybe String,
    getCVenv :: VEnv,
    getCFenv :: FEnv,
    getClassPos :: Pos,
    getClassName :: String
  }
data Env = Env
  {
    getVenv :: VEnv,
    getFenv :: FEnv,
    getCenv :: CEnv,
    getDepth :: Integer
  }
emptyEnv :: Env
emptyEnv = Env Data.Map.empty stdlib Data.Map.empty 0


type Pos = BNFC'Position
data Error =
  ErrUnknownVariable Pos String
  | ErrUnknownFunction Pos String
  | ErrUnknownClass Pos String
  | ErrUnknownInheritedClass Pos String String
  | ErrUnknownClassAttribute Pos String
  | ErrUnknownClassMethod Pos String
  | ErrDuplicateVariable Pos String
  | ErrDuplicateFunction Pos String
  | ErrDuplicateClass Pos String
  | ErrDuplicateClassAttribute Pos String
  | ErrDuplicateClassMethod Pos String
  | ErrDuplicateFunctionArgumentName Pos String
  | ErrWrongType Pos Type Type -- (expected, got)
  | ErrWrongNumberOfArguments Pos String Int Int -- (function name, expected, got)
  | ErrWrongTypeOfArgument Pos String Int Type Type -- (function name, arg number, expected, got)
  | ErrVoidReturnValue Pos
  | ErrNoMain
  | ErrMultipleMain Pos
  | ErrWrongMainType Pos Type
  | ErrNotAssignable Pos Type
  | ErrAddition Pos Type
  | ErrBooleanOperation Pos Type
  | ErrInequalityOperation Pos Type
  | ErrIntegerOutOfRange Pos Integer
  | ErrDivisionByZero Pos
  | ErrCannotCastTo Pos Type Type
  | ErrCannotCastNullTo Pos Type
  | ErrNotAnArray Pos Type
  | ErrNotAClass Pos Type
  | ErrVoidValue Pos
  | ErrFunctionValue Pos
  | ErrRedefinitionOfBuiltinFunction Pos String
  | ErrFieldShadowing Pos String
  | ErrOverridingMethodWrongType Pos String String Type Type -- (method name, class name, expected, got)
  | ErrCyclicInheritance String String
  | ErrSelfDeclaration Pos
  | ErrExpectedSameType Pos Type Type
  | ErrFunctionNotAlwaysReturning Pos String
  | ErrNegativeArrayIndex Pos Integer
  | ErrArrayMultipleDimensions Pos Type
  | ErrClassesNotImplemented Pos

instance Show Error where
  show (ErrUnknownVariable pos ident) = "Unknown variable " ++ ident ++ " at " ++ showPos pos
  show (ErrUnknownFunction pos ident) = "Unknown function " ++ ident ++ " at " ++ showPos pos
  show (ErrUnknownClass pos ident) = "Unknown class " ++ ident ++ " at " ++ showPos pos
  show (ErrUnknownInheritedClass pos ident ident') = "Unknown inherited class " ++ ident ++ " in declaration of class " ++ ident' ++ " at " ++ showPos pos
  show (ErrUnknownClassAttribute pos ident) = "Unknown class attribute " ++ ident ++ " at " ++ showPos pos
  show (ErrUnknownClassMethod pos ident) = "Unknown class method " ++ ident ++ " at " ++ showPos pos
  show (ErrDuplicateVariable pos ident) = "Duplicate variable " ++ ident ++ " at " ++ showPos pos
  show (ErrDuplicateFunction pos ident) = "Duplicate function " ++ ident ++ " at " ++ showPos pos
  show (ErrDuplicateClass pos ident) = "Duplicate class " ++ ident ++ " at " ++ showPos pos
  show (ErrDuplicateClassAttribute pos ident) = "Duplicate class attribute " ++ ident ++ " at " ++ showPos pos
  show (ErrDuplicateClassMethod pos ident) = "Duplicate class method " ++ ident ++ " at " ++ showPos pos
  show (ErrDuplicateFunctionArgumentName pos ident) = "Duplicate function argument name " ++ ident ++ " at " ++ showPos pos
  show (ErrWrongType pos t t') = "Wrong type at " ++ showPos pos ++ ": got " ++ showType t' ++ " from " ++ showPos (hasPosition t') ++ ", expected " ++ showType t ++ " from " ++ showPos (hasPosition t)
  show (ErrWrongNumberOfArguments pos name n n') = "Wrong number of arguments " ++ show n' ++ " at " ++ showPos pos ++ " of function " ++ name ++ ", expected " ++ show n
  show (ErrWrongTypeOfArgument pos name n t t') = "Wrong type of argument number " ++ show n ++ " of function " ++ name ++ " at " ++ showPos pos ++ ": got " ++ showType t' ++ " from " ++ showPos (hasPosition t') ++ ", expected " ++ showType t ++ " from " ++ showPos (hasPosition t)
  show (ErrVoidReturnValue pos) = "Void return value at " ++ showPos pos
  show ErrNoMain = "No main function"
  show (ErrMultipleMain pos) = "Multiple main functions at " ++ showPos pos
  show (ErrWrongMainType pos t) = "Wrong main function type " ++ showType t ++ " at " ++ showPos pos ++ ", expected int()"
  show (ErrNotAssignable pos t) = "Not assignable type " ++ showType t ++ " at " ++ showPos pos
  show (ErrAddition pos t) = "Addition on type " ++ showType t ++ " at " ++ showPos pos ++ ", expected int/string"
  show (ErrBooleanOperation pos t) = "Boolean operation on type " ++ showType t ++ " at " ++ showPos pos ++ ", expected bool"
  show (ErrInequalityOperation pos t) = "Inequality operation on type " ++ showType t ++ " at " ++ showPos pos ++ ", expected int"
  show (ErrIntegerOutOfRange pos n) = "Integer out of range at " ++ showPos pos ++ ": " ++ show n
  show (ErrDivisionByZero pos) = "Division by zero at " ++ showPos pos
  show (ErrCannotCastTo pos t t') = "Cannot cast to type " ++ showType t ++ " from " ++ showType t' ++ " at " ++ showPos pos
  show (ErrCannotCastNullTo pos t) = "Cannot cast null to type " ++ showType t ++ " at " ++ showPos pos
  show (ErrNotAnArray pos t) = "Not an array type " ++ showType t ++ " at " ++ showPos pos
  show (ErrNotAClass pos t) = "Accessing class members on non-class type " ++ showType t ++ " at " ++ showPos pos
  show (ErrVoidValue pos) = "Void value at " ++ showPos pos
  show (ErrFunctionValue pos) = "Function value at " ++ showPos pos
  show (ErrRedefinitionOfBuiltinFunction pos name) = "Redefinition of builtin function " ++ name ++ " at " ++ showPos pos
  show (ErrFieldShadowing pos ident) = "Shadowing the field " ++ ident ++ " at " ++ showPos pos
  show (ErrOverridingMethodWrongType pos methodIdent classIdent t t') = "Overriding method " ++ methodIdent ++ " in class " ++ classIdent ++ " at " ++ showPos pos ++ " has wrong type " ++ showType t' ++ ", expected " ++ showType t
  show (ErrCyclicInheritance ident ident') = "Cyclic inheritance between " ++ ident ++ " and " ++ ident'
  show (ErrSelfDeclaration pos) = "Self declaration at " ++ showPos pos
  show (ErrExpectedSameType pos t t') = "Expected same types at " ++ showPos pos ++ ", got " ++ showType t ++ " at " ++ showPos (hasPosition t) ++ " and " ++ showType t' ++ " at " ++ showPos (hasPosition t')
  show (ErrFunctionNotAlwaysReturning pos ident) = "Function " ++ ident ++ " defined at " ++ showPos pos ++ " doesn't return a value in all possible execution paths"
  show (ErrNegativeArrayIndex pos n) = "Negative array index at " ++ showPos pos ++ ": " ++ show n
  show (ErrArrayMultipleDimensions pos t) = "Array of multiple dimensions at " ++ showPos pos ++ ": " ++ showType t
  show (ErrClassesNotImplemented pos) = "Classes not implemented at " ++ showPos pos  -- TODO

getErrPosition :: Error -> Pos
getErrPosition (ErrUnknownVariable pos _) = pos
getErrPosition (ErrUnknownFunction pos _) = pos
getErrPosition (ErrUnknownClass pos _) = pos
getErrPosition (ErrUnknownInheritedClass pos _ _) = pos
getErrPosition (ErrUnknownClassAttribute pos _) = pos
getErrPosition (ErrUnknownClassMethod pos _) = pos
getErrPosition (ErrDuplicateVariable pos _) = pos
getErrPosition (ErrDuplicateFunction pos _) = pos
getErrPosition (ErrDuplicateClass pos _) = pos
getErrPosition (ErrDuplicateClassAttribute pos _) = pos
getErrPosition (ErrDuplicateClassMethod pos _) = pos
getErrPosition (ErrDuplicateFunctionArgumentName pos _) = pos
getErrPosition (ErrWrongType pos _ _) = pos
getErrPosition (ErrWrongNumberOfArguments pos _ _ _) = pos
getErrPosition (ErrWrongTypeOfArgument pos _ _ _ _) = pos
getErrPosition (ErrVoidReturnValue pos) = pos
getErrPosition ErrNoMain = BNFC'NoPosition
getErrPosition (ErrMultipleMain pos) = pos
getErrPosition (ErrWrongMainType pos _) = pos
getErrPosition (ErrNotAssignable pos _) = pos
getErrPosition (ErrAddition pos _) = pos
getErrPosition (ErrBooleanOperation pos _) = pos
getErrPosition (ErrInequalityOperation pos _) = pos
getErrPosition (ErrIntegerOutOfRange pos _) = pos
getErrPosition (ErrDivisionByZero pos) = pos
getErrPosition (ErrCannotCastTo pos _ _) = pos
getErrPosition (ErrCannotCastNullTo pos _) = pos
getErrPosition (ErrNotAnArray pos _) = pos
getErrPosition (ErrNotAClass pos _) = pos
getErrPosition (ErrVoidValue pos) = pos
getErrPosition (ErrFunctionValue pos) = pos
getErrPosition (ErrRedefinitionOfBuiltinFunction pos _) = pos
getErrPosition (ErrFieldShadowing pos _) = pos
getErrPosition (ErrOverridingMethodWrongType pos _ _  _ _) = pos
getErrPosition (ErrCyclicInheritance _ _) = BNFC'NoPosition
getErrPosition (ErrSelfDeclaration pos) = pos
getErrPosition (ErrExpectedSameType pos _ _) = pos
getErrPosition (ErrFunctionNotAlwaysReturning pos _) = pos
getErrPosition (ErrNegativeArrayIndex pos _) = pos
getErrPosition (ErrArrayMultipleDimensions pos _) = pos
getErrPosition (ErrClassesNotImplemented pos) = pos


fromIdent :: IIdent -> String
fromIdent (IIdent _ (Ident name)) = name

showType :: Type -> String
showType (TInt _) = "int"
showType (TStr _) = "string"
showType (TBool _) = "bool"
showType (TVoid _) = "void"
showType (TArray _ t) = showType t ++ "[]"
showType (TClass _ ident) = fromIdent ident
showType (TFun _ t ts) = showType t ++ "(" ++ Data.List.intercalate ", " (map showType ts) ++ ")"

showPos :: Pos -> String
showPos (BNFC'Position l c) = show l ++ ":" ++ show c
showPos BNFC'NoPosition = "unknown position"
showPos _ = error "showPos: impossible"


stdlib :: FEnv
stdlib = Data.Map.fromList [
  ("printInt", TFun BNFC'NoPosition (TVoid BNFC'NoPosition) [TInt BNFC'NoPosition]),
  ("printString", TFun BNFC'NoPosition (TVoid BNFC'NoPosition) [TStr BNFC'NoPosition]),
  ("error", TFun BNFC'NoPosition (TVoid BNFC'NoPosition) []),
  ("readInt", TFun BNFC'NoPosition (TInt BNFC'NoPosition) []),
  ("readString", TFun BNFC'NoPosition (TStr BNFC'NoPosition) [])
  ]

frontendCheck :: Program -> String -> IO Bool
frontendCheck tree s = do
  val <- runReaderT (runExceptT (checkProgram tree)) emptyEnv
  case val of
    Right _ -> do
      return True
    Left err -> do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr $ "Error: " ++ show err
      hPutStr stderr $ showCode s (getErrPosition err)
      return False

checkProgram :: Program -> FMonad
checkProgram (PProgram _ topDefs) = do
  let funIdents = getTopDefFunIdents topDefs
  let classIdents = getTopDefClassIdents topDefs
  checkNoDuplicateIdents funIdents ErrDuplicateFunction
  checkNoDuplicateIdents classIdents ErrDuplicateClass
  checkMain topDefs
  let fenv = functionDeclarationsToFEnv topDefs
  let cenv = classDeclarationsToCEnv topDefs
  let newEnv = (\env' -> env' {getFenv = Data.Map.union (getFenv env') fenv, getCenv = Data.Map.union (getCenv env') cenv})
  local newEnv checkClassNoCircularInheritance
  local newEnv (mapM_ checkTopDef topDefs)
  return Nothing

getTopDefIdent :: TopDef -> IIdent
getTopDefIdent (PFunDef _ _ ident _ _) = ident
getTopDefIdent (PClassDef _ ident _) = ident
getTopDefIdent (PClassDefExt _ ident _ _) = ident

getTopDefFunIdents :: [TopDef] -> [IIdent]
getTopDefFunIdents = map getTopDefIdent . filter isFunDef
  where
    isFunDef PFunDef {} = True
    isFunDef _ = False

getTopDefClassIdents :: [TopDef] -> [IIdent]
getTopDefClassIdents = map getTopDefIdent . filter isClassDef
  where
    isClassDef PClassDef {} = True
    isClassDef PClassDefExt {} = True
    isClassDef _ = False


checkNoDuplicateIdents :: [IIdent] -> (Pos -> String -> Error) -> FMonad
checkNoDuplicateIdents idents err = do
  let names = map (\(IIdent _ (Ident name)) -> name) idents
  if length names == length (Data.List.nub names) then return Nothing else
    let dup = head $ names Data.List.\\ Data.List.nub names
        dupPos = hasPosition $ filter (\(IIdent _ (Ident name)) -> name == dup) idents !! 1 in
    throwError $ err dupPos dup

checkMain :: [TopDef] -> FMonad
checkMain topDefs = do
  let mains = filter (\def -> fromIdent (getTopDefIdent def) == "main") $ filter isFunDef topDefs
      isFunDef PFunDef {} = True
      isFunDef _ = False
  when (null mains) $ throwError ErrNoMain
  when (length mains > 1) $ throwError $ ErrMultipleMain (hasPosition $ mains !! 2)
  let main = head mains
  case main of
    PFunDef pos t ident args _ ->
      when (case t of {TInt _ -> False; _ -> True} || args /= []) $ throwError $ ErrWrongMainType pos (TFun pos t (map (\(PArg _ t _) -> t) args))
    _ -> error "checkMain: impossible"
  return Nothing

functionDeclarationsToFEnv :: [TopDef] -> FEnv
functionDeclarationsToFEnv topDefs =
  Data.Map.fromList $ map (
    \(PFunDef pos t ident args _) -> (fromIdent ident, TFun pos t $ map argToType args)
  ) (filter isFunDef topDefs)
  where
    isFunDef PFunDef {} = True
    isFunDef _ = False
    argToType (PArg _ t _) = t

classDeclarationsToCEnv :: [TopDef] -> CEnv
classDeclarationsToCEnv topDefs =
  Data.Map.fromList $ map f (filter isClassDef topDefs)
  where
    f (PClassDef _ ident (ClassDef _ elems)) =
      let (venv, fenv) = createEnvs elems in
      (fromIdent ident, ClassData elems Nothing venv fenv (hasPosition ident) (fromIdent ident))
    f (PClassDefExt _ ident ident' (ClassDef _ elems)) =
      let (venv, fenv) = createEnvs elems in
      (fromIdent ident, ClassData elems (Just $ fromIdent ident') venv fenv (hasPosition ident) (fromIdent ident))
    f _ = error "classDeclarationsToCEnv: impossible"

    createEnvs :: [ClassElem] -> (VEnv, FEnv)
    createEnvs elems = (venv, fenv)
      where
        funElems = filter isMethod elems
        varElems = filter isAttr elems
        venv = Data.Map.fromList $ zip (map fromIdent $ classElemsToIdents varElems) (classElemsToTypes varElems `zip` repeat 0)
        fenv = Data.Map.fromList $ zip (map fromIdent $ classElemsToIdents funElems) (classElemsToTypes funElems)

    isClassDef PClassDef {} = True
    isClassDef PClassDefExt {} = True
    isClassDef _ = False


checkClassNoCircularInheritance :: FMonad
checkClassNoCircularInheritance = do
  mapM_ (checkClassNoCircularInheritance' []) =<< asks getCenv
  return Nothing

checkClassNoCircularInheritance' :: [String] -> ClassData -> FMonad
checkClassNoCircularInheritance' visited cls = do
  let extends = getExtends cls
  case extends of
    Nothing -> return Nothing
    Just extendsIdent -> do
      when (extendsIdent `elem` visited) $ throwError $ ErrCyclicInheritance (head visited) extendsIdent
      cenv <- asks getCenv
      case Data.Map.lookup extendsIdent cenv of
        Nothing -> throwError $ ErrUnknownInheritedClass (getClassPos cls) extendsIdent (getClassName cls)
        Just cls' -> do
          checkClassNoCircularInheritance' (extendsIdent:visited) cls'
          return Nothing


checkTopDef :: TopDef -> FMonad
checkTopDef (PFunDef pos t ident args block) = do
  when (fromIdent ident == "self") $ throwError $ ErrSelfDeclaration (hasPosition ident)
  mapM_ checkCorrectType $ t:map (\(PArg _ t' _) -> t') args
  when (fromIdent ident `elem` Data.Map.keys stdlib) $ throwError $ ErrRedefinitionOfBuiltinFunction (hasPosition ident) (fromIdent ident)
  checkFunRetType t
  let argTypes = map (\(PArg _ t _) -> t) args
  mapM_ checkValType argTypes
  let argIdents = map (\(PArg _ _ ident) -> ident) args
  when ("self" `elem` map fromIdent argIdents) $ throwError $ ErrSelfDeclaration $ hasPosition $ head (filter (\i -> "self" == fromIdent i) argIdents)
  checkNoDuplicateIdents argIdents ErrDuplicateFunctionArgumentName
  let envFun = \env -> env {
    getVenv = Data.Map.union (getVenv env) $ Data.Map.fromList $ zip (map fromIdent argIdents) $ zip argTypes $ repeat (getDepth env + 1)
  }
  mt' <- local envFun (checkBlock block t)
  cenv <- asks getCenv
  case mt' of
    Just t' -> if castsTo cenv t t' then return Nothing else throwError $ ErrCannotCastTo pos t t'
    Nothing -> if sameType t (TVoid $ hasPosition t) then return Nothing else throwError $ ErrFunctionNotAlwaysReturning (hasPosition ident) (fromIdent ident)
checkTopDef (PClassDef _ ident (ClassDef _ elems)) = do
  when (fromIdent ident == "self") $ throwError $ ErrSelfDeclaration (hasPosition ident)
  let elemIdents = classElemsToIdents elems
  when ("self" `elem` map fromIdent elemIdents) $ throwError $ ErrSelfDeclaration $ hasPosition $ head (filter (\i -> "self" == fromIdent i) elemIdents)
  let elemTypes = classElemsToTypes elems
  mapM_ checkCorrectType elemTypes
  let funElems = filter isMethod elems
  unless (null funElems) $ throwError $ ErrClassesNotImplemented (hasPosition ident)  -- TODO
  checkNoDuplicateIdents (classElemsToIdents funElems) ErrDuplicateClassMethod
  mapM_ checkFunRetType $ classElemsToTypes funElems
  let varElems = filter isAttr elems
  mapM_ checkValType $ classElemsToTypes varElems
  checkNoDuplicateIdents (classElemsToIdents varElems) ErrDuplicateClassAttribute
  let envClass = \env -> env {
    getVenv = Data.Map.fromList $ zip (map fromIdent $ classElemsToIdents varElems) (classElemsToTypes varElems `zip` repeat (getDepth env)),
    getFenv = Data.Map.union (getFenv env) $ Data.Map.fromList $ zip (map fromIdent $ classElemsToIdents funElems) (classElemsToTypes funElems)
  }
  local envClass (mapM_ (checkClassElem ident) elems)
  return Nothing
checkTopDef (PClassDefExt _ ident extendsIdent (ClassDef pos' elems)) = do
  -- TODO
  throwError $ ErrClassesNotImplemented (hasPosition ident)
  -- when (fromIdent ident == "self") $ throwError $ ErrSelfDeclaration (hasPosition ident)
  -- cenv <- asks getCenv
  -- case Data.Map.lookup (fromIdent extendsIdent) cenv of
  --   Nothing -> throwError $ ErrUnknownClass (hasPosition extendsIdent) (fromIdent extendsIdent)
  --   Just cls -> do
  --     let elemIdents = classElemsToIdents elems
  --     when ("self" `elem` map fromIdent elemIdents) $ throwError $ ErrSelfDeclaration $ hasPosition $ head (filter (\i -> "self" == fromIdent i) elemIdents)
  --     let elemTypes = classElemsToTypes elems
  --     mapM_ checkCorrectType elemTypes
  --     let funElems = filter isMethod elems
  --     checkNoDuplicateIdents (classElemsToIdents funElems) ErrDuplicateClassMethod
  --     mapM_ checkFunRetType $ classElemsToTypes funElems
  --     let varElems = filter isAttr elems
  --     mapM_ checkValType $ classElemsToTypes varElems
  --     checkNoDuplicateIdents (classElemsToIdents varElems) ErrDuplicateClassAttribute
  --     cvenv <- createCVenv ident
  --     cfenv <- createCFenv ident
  --     let envClass = \env -> env {
  --       getVenv = cvenv,
  --       getFenv = Data.Map.union (getFenv env) cfenv
  --     }
  --     local envClass (mapM_ (checkClassElem ident) elems)
  --     return Nothing

checkClassElem :: IIdent -> ClassElem -> FMonad
checkClassElem _ (ClassAttrDef _ t ident) = return Nothing
checkClassElem classIdent (ClassMethodDef pos t ident args block) = do
  cenv <- asks getCenv
  let Just cls = Data.Map.lookup (fromIdent classIdent) cenv
  checkFunRetType t
  let argTypes = map (\(PArg _ t _) -> t) args
  mapM_ checkValType argTypes
  let argIdents = map (\(PArg _ _ ident) -> ident) args
  checkNoDuplicateIdents argIdents ErrDuplicateFunctionArgumentName
  cvenv <- createCVenv classIdent
  cfenv <- createCFenv classIdent
  let envFun = \env -> env {
    getVenv = Data.Map.union (Data.Map.fromList $ zip (map fromIdent argIdents) $ zip argTypes $ repeat (getDepth env + 1)) cvenv,
    getFenv = Data.Map.union cfenv (getFenv env),
    getDepth = getDepth env + 1
  }
  mt' <- local envFun (checkBlock block t)
  case mt' of
    Just t' -> if sameType t t' then return Nothing else throwError $ ErrWrongType pos t t'
    Nothing -> if sameType t (TVoid $ hasPosition t) then return Nothing else throwError $ ErrWrongType pos t (TVoid BNFC'NoPosition)


createCVenv :: IIdent -> FMonad' VEnv
createCVenv ident = do
  cenv <- asks getCenv
  let Just cls = Data.Map.lookup (fromIdent ident) cenv
  cvenv <- createCVenv' cls
  return $ Data.Map.insert "self" (TClass (hasPosition ident) ident, 0) cvenv
createCVenv' :: ClassData -> FMonad' VEnv
createCVenv' cls = do
  case getExtends cls of
    Nothing -> return $ getCVenv cls
    Just extendsIdent -> do
      cenv <- asks getCenv
      let cvenv = getCVenv cls
      let Just cls' = Data.Map.lookup extendsIdent cenv
      cvenv' <- createCVenv' cls'
      let dups = Data.Map.intersection cvenv cvenv'
      unless (null $ Data.Map.toList dups) (do
          let elems = classElemsToIdents $ filter isAttr $ getClassElems cls
          let pos = hasPosition $ head $ filter (\ident -> fromIdent ident `elem` Data.Map.keys dups) elems
          throwError $ ErrFieldShadowing pos (head $ Data.Map.keys dups)
        )
      return $ Data.Map.union cvenv' $ Data.Map.union (getCVenv cls) cvenv

createCFenv :: IIdent -> FMonad' FEnv
createCFenv ident = do
  cenv <- asks getCenv
  let Just cls = Data.Map.lookup (fromIdent ident) cenv
  cfenv <- createCFenv' (fromIdent ident) cls
  return $ Data.Map.union (getCFenv cls) cfenv
createCFenv' :: String -> ClassData -> FMonad' FEnv
createCFenv' name cls = do
  case getExtends cls of
    Nothing -> return $ getCFenv cls
    Just extendsName -> do
      cenv <- asks getCenv
      let cfenv = getCFenv cls
      let Just cls' = Data.Map.lookup extendsName cenv
      cfenv' <- createCFenv' extendsName cls'
      let dupsIdents = Data.Map.keys $ Data.Map.intersection cfenv cfenv'
      let sameTypes = map (\ident -> sameType (cfenv Data.Map.! ident) (cfenv' Data.Map.! ident)) dupsIdents
      let pos = hasPosition $ head $ filter (\ident -> fromIdent ident `elem` dupsIdents) $ classElemsToIdents $ filter isMethod $ getClassElems cls
      let t = getCFenv cls Data.Map.! head dupsIdents
      let t' = cfenv' Data.Map.! head dupsIdents
      unless (and sameTypes) $ throwError $ ErrOverridingMethodWrongType pos (head dupsIdents) name t' t
      return $ Data.Map.union cfenv' $ Data.Map.union (getCFenv cls) cfenv

classElemsToIdents :: [ClassElem] -> [IIdent]
classElemsToIdents [] = []
classElemsToIdents ((ClassAttrDef _ t items):elems) = map (\(ClassItem _ ident) -> ident) items ++ classElemsToIdents elems
classElemsToIdents ((ClassMethodDef _ t ident args _):elems) = ident:classElemsToIdents elems

classElemsToTypes :: [ClassElem] -> [Type]
classElemsToTypes [] = []
classElemsToTypes ((ClassAttrDef _ t items):elems) = [t | _ <- items] ++ classElemsToTypes elems
classElemsToTypes ((ClassMethodDef pos t _ args _):elems) = TFun pos t (map (\(PArg _ t _) -> t) args):classElemsToTypes elems

isMethod :: ClassElem -> Bool
isMethod ClassMethodDef {} = True
isMethod _ = False

isAttr :: ClassElem -> Bool
isAttr = not . isMethod


checkBlock :: Block -> Type -> FMonad
checkBlock (SBlock _ stmts) t = local increaseDepth (checkStmts stmts t)

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
checkStmts (SDecl pos t (item:items):stmts) t' = do
  checkCorrectType t
  when (fromIdent ident == "self") $ throwError $ ErrSelfDeclaration pos
  tryInsertToEnv ident t
  checkValType t
  depth <- asks getDepth
  case item of
    SNoInit {} -> return ()
    SInit _ _ expr -> do
      (t'', _) <- checkExpr expr
      cenv <- asks getCenv
      unless (castsTo cenv t'' t) $ throwError $ ErrWrongType (hasPosition expr) t t''
      tryEvalExpr expr
      return ()
  local (insertToEnv depth ident t) (checkStmts stmts' t')
  where
    stmts' = case item of
      SNoInit {} -> SDecl (hasPosition item) t items:stmts
      SInit _ _ expr -> SDecl (hasPosition item) t items:stmts
    ident = getIdent item
    getIdent (SNoInit _ ident) = ident
    getIdent (SInit _ ident _) = ident
checkStmts (SDecl _ _ []:stmts) t = checkStmts stmts t
checkStmts (SAss pos expr expr':stmts) t'' = do
  (t, ass) <- checkExpr expr
  unless ass $ throwError $ ErrNotAssignable pos t
  (t', _) <- checkExpr expr'
  cenv <- asks getCenv
  unless (castsTo cenv t' t) $ throwError $ ErrWrongType pos t t'
  tryEvalExpr expr'
  checkStmts stmts t''
checkStmts (SIncr pos expr:stmts) t' = do
  (t, ass) <- checkExpr expr
  unless ass $ throwError $ ErrNotAssignable pos t
  unless (sameType t (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t
  checkStmts stmts t'
checkStmts (SDecr pos expr:stmts) t' = do
  (t, ass) <- checkExpr expr
  unless ass $ throwError $ ErrNotAssignable pos t
  unless (sameType t (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t
  checkStmts stmts t'
checkStmts (SRet pos expr:stmts) t = do
  when (sameType t (TVoid pos)) $ throwError $ ErrVoidReturnValue (hasPosition expr)
  (t', _) <- checkExpr expr
  cenv <- asks getCenv
  unless (castsTo cenv t' t) $ throwError $ ErrWrongType (hasPosition expr) t t'
  tryEvalExpr expr
  mt'' <- checkStmts stmts t
  case mt'' of
    Just t'' -> if sameType t t'' then return $ Just t else throwError $ ErrWrongType (hasPosition expr) t t''
    Nothing -> return $ Just t
checkStmts (SVRet pos:stmts) t = do
  unless (sameType t (TVoid pos)) $ throwError $ ErrWrongType pos t (TVoid pos)
  mt'' <- checkStmts stmts t
  case mt'' of
    Just t'' -> if sameType t t'' then return $ Just t else throwError $ ErrWrongType pos t t''
    Nothing -> return $ Just t
checkStmts (SCond pos expr stmt:stmts) t = do
  (t', _) <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError $ ErrWrongType (hasPosition expr) (TBool pos) t'
  mt1 <- checkStmts [addBlockIfNecessary stmt] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt'') of
        (_, Just _) -> return $ Just t
        _ -> return Nothing
    Just (VBool True) ->
      case mt1 of
        Just _ -> return $ Just t
        _ -> return mt''
    Just (VBool False) -> return mt''
    _ -> error "checkStmts: impossible"
checkStmts (SCondElse pos expr stmt1 stmt2:stmts) t = do
  (t', _) <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError $ ErrWrongType (hasPosition expr) (TBool pos) t'
  mt1 <- checkStmts [addBlockIfNecessary stmt1] t
  mt2 <- checkStmts [addBlockIfNecessary stmt2] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt2, mt'') of
        (Just _, Just _, _) -> return $ Just t
        (_, _, Just _) -> return $ Just t
        _ -> return Nothing
    Just (VBool True) ->
      case mt1 of
        Just _ -> return $ Just t
        _ -> return mt''
    Just (VBool False) ->
      case mt2 of
        Just _ -> return $ Just t
        _ -> return mt''
    _ -> error "checkStmts: impossible"
checkStmts (SWhile pos expr stmt:stmts) t = do
  (t', _) <- checkExpr expr
  unless (sameType t' (TBool pos)) $ throwError $ ErrWrongType (hasPosition expr) (TBool pos) t'
  mt1 <- checkStmts [addBlockIfNecessary stmt] t
  mt'' <- checkStmts stmts t
  mb <- tryEvalExpr expr
  case mb of
    Nothing ->
      case (mt1, mt'') of
        (_, Just _) -> return $ Just t
        _ -> return Nothing
    Just (VBool True) ->
      case mt1 of
        Just _ -> return $ Just t
        _ -> return mt''
    Just (VBool False) -> return mt''
    _ -> error "checkStmts: impossible"
checkStmts (SFor pos t' ident expr stmt:stmts) t = do
  checkCorrectType t'
  when (fromIdent ident == "self") $ throwError $ ErrSelfDeclaration pos
  (t'', _) <- checkExpr expr
  cenv <- asks getCenv
  case t'' of
    TArray pos t''' -> do
      unless (castsTo cenv t''' t') $ throwError $ ErrCannotCastTo pos t''' t'
    _ -> throwError $ ErrWrongType pos (TArray (hasPosition expr) t') t''
  local increaseDepth $ tryInsertToEnv ident t'
  d <- asks getDepth
  local (insertToEnv (d + 1) ident t') (checkStmts [addBlockIfNecessary stmt] t)
  checkStmts stmts t
checkStmts (SExp _ expr:stmts) t = do
  checkExpr expr
  tryEvalExpr expr
  checkStmts stmts t


addBlockIfNecessary :: Stmt -> Stmt
addBlockIfNecessary stmt = case stmt of
  SBStmt {} -> stmt
  _ -> SBStmt (hasPosition stmt) (SBlock (hasPosition stmt) [stmt])


maxInt :: Integer
maxInt = 2^31 - 1
minInt :: Integer
minInt = -2^31
checkInt :: Pos -> Integer -> TEMonad
checkInt pos n = if minInt <= n && n <= maxInt then return $ Just $ VInt n else throwError $ ErrIntegerOutOfRange pos n

tryEvalExpr :: Expr -> TEMonad
tryEvalExpr (ELitInt pos n) = checkInt pos n
tryEvalExpr (ELitTrue pos) = return $ Just $ VBool True
tryEvalExpr (ELitFalse pos) = return $ Just $ VBool False
tryEvalExpr (EString pos s) = return $ Just $ VStr s
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
      OMod _ -> if n2 /= 0 then checkInt pos $ n1 `rem` n2 else throwError $ ErrDivisionByZero pos
    _ -> return Nothing
tryEvalExpr (EAdd pos expr1 op expr2) = do
  mn1 <- tryEvalExpr expr1
  mn2 <- tryEvalExpr expr2
  case (mn1, mn2) of
    (Just (VInt n1), Just (VInt n2)) -> case op of
      OPlus _ -> checkInt pos $ n1 + n2
      OMinus _ -> checkInt pos $ n1 - n2
    (Just (VStr s1), Just (VStr s2)) -> case op of
      OPlus _ -> return $ Just $ VStr $ s1 ++ s2
      _ -> return Nothing
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
    (Just (VStr s1), Just (VStr s2)) -> return $ Just $ VBool $ case op of
      OEQU _ -> s1 == s2
      ONE _ -> s1 /= s2
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
  venv <- asks getVenv
  case Data.Map.lookup (fromIdent ident) venv of
    Just (t, _) -> return (t, fromIdent ident /= "self")
    Nothing -> throwError $ ErrUnknownVariable (hasPosition ident) (fromIdent ident)
checkExpr (ELitInt pos _) = return (TInt pos, False)
checkExpr (ELitTrue pos) = return (TBool pos, False)
checkExpr (ELitFalse pos) = return (TBool pos, False)
checkExpr (EString pos _) = return (TStr pos, False)
checkExpr (ECastNull pos t) = do
  case t of
    TClass _ ident -> do
      venv <- asks getVenv
      cenv <- asks getCenv
      case Data.Map.lookup (fromIdent ident) cenv of
        Just _ -> return (t, False)
        Nothing -> throwError $ ErrUnknownClass (hasPosition ident) (fromIdent ident)
    TArray {} -> return (t, False)
    _ -> throwError $ ErrCannotCastNullTo pos t
checkExpr (EArrayNew pos t expr) = do
  checkCorrectType (TArray pos t)
  checkValType t
  (t', _) <- checkExpr expr
  unless (sameType t' (TInt pos)) $ throwError $ ErrWrongType pos (TInt $ hasPosition t') t'
  return (TArray pos t, False)
checkExpr (EArrayElem pos expr val) = do
  (t, _) <- checkExpr expr
  checkValType t
  (t', _) <- checkExpr val
  unless (sameType t' (TInt pos)) $ throwError $ ErrWrongType pos (TInt $ hasPosition t') t'
  mval <- tryEvalExpr val
  case mval of
    Just (VInt n) -> unless (n >= 0) $ throwError $ ErrNegativeArrayIndex (hasPosition val) n
    _ -> return ()
  case t of
    TArray _ t'' -> return (t'', True)
    _ -> throwError $ ErrNotAnArray pos t
checkExpr (EClassAttr pos expr ident) = do
  (t, _) <- checkExpr expr
  case t of
    TClass _ ident' -> do
      checkClassAttr (fromIdent ident') ident
    TArray _ t' -> do
      unless (fromIdent ident == "length") $ throwError $ ErrNotAClass pos t
      return (TInt pos, False)
    _ -> throwError $ ErrNotAClass pos t
checkExpr (EClassNew pos ident) = do
  cenv <- asks getCenv
  case Data.Map.lookup (fromIdent ident) cenv of
    Just _ -> return (TClass pos ident, False)
    Nothing -> throwError $ ErrUnknownClass (hasPosition ident) (fromIdent ident)
checkExpr (EMethodCall pos expr ident exprs) = do
  (t, _) <- checkExpr expr
  case t of
    TClass _ ident' -> do
      cenv <- asks getCenv
      let Just cls = Data.Map.lookup (fromIdent ident') cenv
      cfenv <- createCFenv ident'
      case Data.Map.lookup (fromIdent ident) cfenv of
        Just (TFun pos' t' ts) -> do
          when (length ts /= length exprs) $ throwError $ ErrWrongNumberOfArguments (hasPosition ident) (fromIdent ident) (length ts) (length exprs)
          argTypes' <- mapM checkExpr exprs
          let argTypes = map fst argTypes'
          if and $ zipWith (castsTo cenv) argTypes ts then return (t', False) else do
            let firstBadArg = head $ filter (\(t, t') -> not $ castsTo cenv t t') $ zip argTypes ts
                firstBadArgPos = hasPosition $ exprs !! length (takeWhile (/= firstBadArg) $ zip argTypes ts)
                firstBadArgNum = (+1) $ length $ takeWhile (/= firstBadArg) $ zip argTypes ts
            throwError $ ErrWrongTypeOfArgument firstBadArgPos (fromIdent ident) firstBadArgNum (snd firstBadArg) (fst firstBadArg)
        _ -> throwError $ ErrUnknownClassMethod (hasPosition ident) (fromIdent ident)
    _ -> throwError $ ErrNotAClass pos t
checkExpr (EFunctionCall pos ident exprs) = do
  fenv <- asks getFenv
  case Data.Map.lookup (fromIdent ident) fenv of
    Just (TFun _ t ts) -> do
      when (length ts /= length exprs) $ throwError $ ErrWrongNumberOfArguments pos (fromIdent ident) (length ts) (length exprs)
      argTypes' <- mapM checkExpr exprs
      let argTypes = map fst argTypes'
      cenv <- asks getCenv
      if and $ zipWith (castsTo cenv) argTypes ts then return (t, False) else do
        let firstBadArg = head $ filter (\(t, t') -> not $ castsTo cenv t t') $ zip argTypes ts
            firstBadArgPos = hasPosition $ exprs !! length (takeWhile (/= firstBadArg) $ zip argTypes ts)
            firstBadArgNum = (+1) $ length $ takeWhile (/= firstBadArg) $ zip argTypes ts
        throwError $ ErrWrongTypeOfArgument firstBadArgPos (fromIdent ident) firstBadArgNum (snd firstBadArg) (fst firstBadArg)
    _ -> throwError $ ErrUnknownFunction (hasPosition ident) (fromIdent ident)
checkExpr (ENeg pos expr) = do
  (t, _) <- checkExpr expr
  unless (sameType t (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t
  return (t, False)
checkExpr (ENot pos expr) = do
  (t, _) <- checkExpr expr
  unless (sameType t (TBool pos)) $ throwError $ ErrBooleanOperation pos t
  return (t, False)
checkExpr (EMul pos expr1 op expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t1
  unless (sameType t2 (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t2
  return (TInt pos, False)
checkExpr (EAdd pos expr1 op expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  case op of
    OPlus _ -> do
      unless (sameType t1 t2) $ throwError $ ErrExpectedSameType pos t1 t2
      unless (sameType t1 (TInt pos) || sameType t1 (TStr pos)) $ throwError $ ErrAddition pos t1
    OMinus _ -> do
      unless (sameType t1 (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t1
      unless (sameType t2 (TInt pos)) $ throwError $ ErrWrongType pos (TInt pos) t2
  return (t1, False)
checkExpr (ERel pos expr1 op expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  cenv <- asks getCenv
  unless (castsTo cenv t1 t2 || castsTo cenv t2 t1) $ throwError $ ErrExpectedSameType pos t1 t2
  when (sameType t1 (TVoid pos)) $ throwError $ ErrVoidValue pos
  case op of
    OEQU {} -> return (TBool pos, False)
    ONE {} -> return (TBool pos, False)
    _ -> if sameType t1 (TInt pos) then return (TBool pos, False) else throwError $ ErrInequalityOperation pos t1
checkExpr (EAnd pos expr1 expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TBool pos)) $ throwError $ ErrBooleanOperation pos t1
  unless (sameType t2 (TBool pos)) $ throwError $ ErrBooleanOperation pos t2
  return (TBool pos, False)
checkExpr (EOr pos expr1 expr2) = do
  (t1, _) <- checkExpr expr1
  (t2, _) <- checkExpr expr2
  unless (sameType t1 (TBool pos)) $ throwError $ ErrBooleanOperation pos t1
  unless (sameType t2 (TBool pos)) $ throwError $ ErrBooleanOperation pos t2
  return (TBool pos, False)


checkClassAttr :: String -> IIdent -> EMonad
checkClassAttr ident field = do
  cenv <- asks getCenv
  case Data.Map.lookup ident cenv of
    Just cls -> do
      case Data.Map.lookup (fromIdent field) (getCVenv cls) of
        Just (t, _) -> return (t, True)
        Nothing ->
          case getExtends cls of
            Just nameExtends ->
              checkClassAttr nameExtends field
            Nothing -> throwError $ ErrUnknownClassAttribute (hasPosition field) (fromIdent field)
    Nothing -> error "checkClassAttr: impossible"


checkClassMethod :: String -> IIdent -> EMonad
checkClassMethod ident method = do
  cenv <- asks getCenv
  case Data.Map.lookup ident cenv of
    Just cls -> do
      case Data.Map.lookup (fromIdent method) (getCFenv cls) of
        Just t -> return (t, False)
        Nothing ->
          case getExtends cls of
            Just nameExtends ->
              checkClassMethod nameExtends method
            Nothing -> throwError $ ErrUnknownClassMethod (hasPosition method) (fromIdent method)
    Nothing -> error "checkClassMethod: impossible"


checkValType :: Type -> FMonad' ()
checkValType (TArray _ t) = checkValType t
checkValType (TClass _ ident) = do
  cenv <- asks getCenv
  case Data.Map.lookup (fromIdent ident) cenv of
    Just _ -> return ()
    Nothing -> throwError $ ErrUnknownClass (hasPosition ident) (fromIdent ident)
checkValType t@TFun {} = throwError $ ErrFunctionValue (hasPosition t)
checkValType (TVoid pos) = throwError $ ErrVoidValue pos
checkValType _ = return ()

checkFunRetType :: Type -> FMonad' ()
checkFunRetType arr@(TArray _ t) = checkValType arr
checkFunRetType (TClass _ ident) = do
  cenv <- asks getCenv
  case Data.Map.lookup (fromIdent ident) cenv of
    Just _ -> return ()
    Nothing -> throwError $ ErrUnknownClass (hasPosition ident) (fromIdent ident)
checkFunRetType (TFun _ t _) = checkFunRetType t
checkFunRetType _ = return ()


tryInsertToEnv :: IIdent -> Type -> FMonad
tryInsertToEnv ident t = do
  venv <- asks getVenv
  fenv <- asks getFenv
  depth <- asks getDepth
  case t of
    TFun {} -> when (Data.Map.member (fromIdent ident) fenv) $ throwError $ ErrDuplicateFunction (hasPosition ident) (fromIdent ident)
    _ -> do
      case Data.Map.lookup (fromIdent ident) venv of
        Just (_, depth') -> do
          when (depth' == depth) $ throwError $ ErrDuplicateVariable (hasPosition ident) (fromIdent ident)
        Nothing -> return ()
  when (sameType t (TVoid $ hasPosition t)) $ throwError $ ErrVoidValue (hasPosition t)
  return Nothing

insertToEnv :: Integer -> IIdent -> Type -> Env -> Env
insertToEnv depth ident t env =
  case t of
    TFun {} -> env { getFenv = Data.Map.insert (fromIdent ident) t $ getFenv env}
    _ -> env { getVenv = Data.Map.insert (fromIdent ident) (t, depth) $ getVenv env}

increaseDepth :: Env -> Env
increaseDepth env = env {getDepth = getDepth env + 1}


sameType :: Type -> Type -> Bool
sameType (TInt _) (TInt _) = True
sameType (TStr _) (TStr _) = True
sameType (TBool _) (TBool _) = True
sameType (TVoid _) (TVoid _) = True
sameType (TClass _ ident) (TClass _ ident') = fromIdent ident == fromIdent ident'
sameType (TArray _ t) (TArray _ t') = sameType t t'
sameType (TFun _ t ts) (TFun _ t' ts') = sameType t t' && length ts == length ts' && all (uncurry sameType) (ts `zip` ts')
sameType _ _ = False

castsTo :: CEnv -> Type -> Type -> Bool
castsTo _ (TInt _) (TInt _) = True
castsTo _ (TStr _) (TStr _) = True
castsTo _ (TBool _) (TBool _) = True
castsTo _ (TVoid _) (TVoid _) = True
castsTo cenv (TClass _ ident) (TClass _ ident') = fromIdent ident' `elem` getAncestors cenv (fromIdent ident)
  where
    getAncestors :: CEnv -> String -> [String]
    getAncestors cenv ident = case Data.Map.lookup ident cenv of
      Just cls -> case getExtends cls of
        Nothing -> [ident]
        Just ident' -> ident:getAncestors cenv ident'
      Nothing -> error "castsTo: impossible"
castsTo cenv (TArray _ t) (TArray _ t') = sameType t t'
castsTo cenv (TFun _ t ts) (TFun _ t' ts') = sameType t t' && all (uncurry sameType) (ts `zip` ts')
castsTo _ _ _ = False

checkCorrectType :: Type -> FMonad' ()
checkCorrectType (TFun _ t ts) = do
  checkCorrectType t
  mapM_ checkCorrectType ts
checkCorrectType t@(TArray pos (TArray _ _)) = do
  throwError $ ErrArrayMultipleDimensions pos t
checkCorrectType _ = return ()
