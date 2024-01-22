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
  getEnv :: Map String String,
  getClassMethods :: Map String [String],
  getMethods :: [String]
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
transformTree prog = evalState (transformProgram prog) (TMState Map.empty Map.empty Map.empty [])

transformProgram :: Abs.Program -> TM AST.Program
transformProgram (Abs.PProgram _ topDefs) = do
  let classes = filter isClassDef topDefs
  let classToTopDef = Map.fromList $ map (\topDef -> (getClassIdent topDef, topDef)) classes
  mapM_ (gatherClassMethods classToTopDef) (Map.keys classToTopDef)
  AST.PProgram <$> mapM transformTopDef topDefs

isClassDef :: Abs.TopDef -> Bool
isClassDef (Abs.PClassDef {}) = True
isClassDef (Abs.PClassDefExt {}) = True
isClassDef _ = False

getClassIdent :: Abs.TopDef -> String
getClassIdent (Abs.PClassDef _ ident _) = fromIIdent ident
getClassIdent (Abs.PClassDefExt _ ident _ _) = fromIIdent ident

gatherClassMethods :: Map String Abs.TopDef -> String -> TM ()
gatherClassMethods classToTopDef ident = do
  let classDef = classToTopDef Map.! ident
  case classDef of
    Abs.PClassDef _ _ (Abs.ClassDef _ classElems) -> do
      let methods = filter isClassMethodDef classElems
      let methodsIdents = map getClassMethodIdent methods
      modify (\s -> s { getClassMethods = Map.insert ident methodsIdents (getClassMethods s) })
    Abs.PClassDefExt _ _ extendingIdent (Abs.ClassDef _ classElems) -> do
      let methods = filter isClassMethodDef classElems
      let methodsIdents = map getClassMethodIdent methods
      gatherClassMethods classToTopDef (fromIIdent extendingIdent)
      methodsExtendingIdents <- gets (\s -> getClassMethods s Map.! fromIIdent extendingIdent)
      let methodsIdents' = methodsIdents ++ methodsExtendingIdents
      modify (\s -> s { getClassMethods = Map.insert ident methodsIdents' (getClassMethods s) })
    _ -> return ()

isClassMethodDef :: Abs.ClassElem -> Bool
isClassMethodDef (Abs.ClassMethodDef {}) = True
isClassMethodDef _ = False

getClassMethodIdent :: Abs.ClassElem -> String
getClassMethodIdent (Abs.ClassMethodDef _ _ ident _ _) = fromIIdent ident


transformTopDef :: Abs.TopDef -> TM AST.TopDef
transformTopDef (Abs.PFunDef _ t ident args block) = do
  args' <- mapM (\(Abs.PArg _ t (Abs.IIdent _ (Abs.Ident ident))) -> do
    newIdent <- getNewName ident
    modify (\s -> s { getEnv = Map.insert ident newIdent (getEnv s) })
    t' <- transformType t
    return $ AST.PArg t' newIdent
    ) args
  tBlock <- transformBlock block
  let tBlock' = case t of
        Abs.TVoid _ -> addVRetToBlockIfNecessary tBlock
        _ -> tBlock
  AST.PFunDef <$> transformType t <*> transformIIdentFun ident <*> pure args' <*> pure tBlock'
transformTopDef (Abs.PClassDef _ ident classDef) = do
  methods <- gets (\s -> getClassMethods s Map.! fromIIdent ident)
  modify (\s -> s { getMethods = methods })
  res <- AST.PClassDef <$> transformIIdentClass ident <*> transformClassDef classDef
  modify (\s -> s { getMethods = [] })
  return res
transformTopDef (Abs.PClassDefExt _ ident ident' classDef) = do
  methods <- gets (\s -> getClassMethods s Map.! fromIIdent ident)
  modify (\s -> s { getMethods = methods })
  res <- AST.PClassDefExt <$> transformIIdentClass ident <*> transformIIdentClass ident' <*> transformClassDef classDef
  modify (\s -> s { getMethods = [] })
  return res

transformClassDef :: Abs.ClassDef -> TM AST.ClassDef
transformClassDef (Abs.ClassDef _ classElems) = do
  tClassElems <- mapM transformClassElem classElems
  let tClassElemsFlat = Data.List.concat tClassElems
  return $ AST.ClassDef tClassElemsFlat

transformClassElem :: Abs.ClassElem -> TM [AST.ClassElem]
transformClassElem (Abs.ClassAttrDef _ t classItems) = do
  idents <- mapM transformClassItem classItems
  t' <- transformType t
  return $ map (AST.ClassAttrDef t') idents
transformClassElem (Abs.ClassMethodDef _ t ident args block) = do
  args' <- mapM (\(Abs.PArg _ t (Abs.IIdent _ (Abs.Ident ident))) -> do
    newIdent <- getNewName ident
    modify (\s -> s { getEnv = Map.insert ident newIdent (getEnv s) })
    t' <- transformType t
    return $ AST.PArg t' newIdent
    ) args
  tBlock <- transformBlock block
  let tBlock' = case t of
        Abs.TVoid _ -> addVRetToBlockIfNecessary tBlock
        _ -> tBlock
  ct <- transformType t
  identMethod <- transformIIdentClassMethod ident
  return [AST.ClassMethodDef ct identMethod args' tBlock']

transformClassItem :: Abs.ClassItem -> TM AST.Ident
transformClassItem (Abs.ClassItem _ ident) = transformIIdentClassAttr ident

transformBlock :: Abs.Block -> TM AST.Block
transformBlock (Abs.SBlock _ stmts) = do
  oldEnv <- gets getEnv
  tStmts <- transformStmts stmts
  modify (\s -> s { getEnv = oldEnv })
  return $ AST.SBlock tStmts

addVRetToBlockIfNecessary :: AST.Block -> AST.Block
addVRetToBlockIfNecessary (AST.SBlock stmts) =
  case stmts of
    [] -> AST.SBlock [AST.SVRet]
    _ -> case last stmts of
      AST.SVRet -> AST.SBlock stmts
      _ -> AST.SBlock (stmts ++ [AST.SVRet])

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
    AST.ELitTrue -> transformBlock (getStmtsBlock stmt) >>= return . AST.SBStmt
    AST.ELitFalse -> return AST.SEmpty
    _ -> do
      let block = getStmtsBlock stmt
      tBlock <- transformBlock block
      return $ AST.SCond tExpr (AST.SBStmt tBlock)

transformStmt' (Abs.SCondElse _ expr stmt1 stmt2) = do
  tExpr <- transformExpr expr
  case tExpr of
    AST.ELitTrue -> transformBlock (getStmtsBlock stmt1) >>= return . AST.SBStmt
    AST.ELitFalse -> transformBlock (getStmtsBlock stmt2) >>= return . AST.SBStmt
    _ -> do
      let block1 = getStmtsBlock stmt1
      let block2 = getStmtsBlock stmt2
      tBlock1 <- transformBlock block1
      tBlock2 <- transformBlock block2
      return $ AST.SCondElse tExpr (AST.SBStmt tBlock1) (AST.SBStmt tBlock2)
transformStmt' (Abs.SWhile _ expr stmt) = do
  tExpr <- transformExpr expr
  case tExpr of
    AST.ELitFalse -> return AST.SEmpty
    _ -> do
      let block = getStmtsBlock stmt
      tBlock <- transformBlock block
      return $ AST.SWhile tExpr (AST.SBStmt tBlock)
transformStmt' (Abs.SFor _ t ident expr stmt) = do
  let block = getStmtsBlock stmt
  newIdent <- getNewName (fromIIdent ident)
  env <- gets getEnv
  modify (\s -> s { getEnv = Map.insert (fromIIdent ident) newIdent (getEnv s) })
  tBlock <- transformBlock block
  modify (\s -> s { getEnv = env })
  AST.SFor <$> transformType t <*> pure newIdent <*> transformExpr expr <*> pure (AST.SBStmt tBlock)
transformStmt' (Abs.SExp _ expr) = AST.SExp <$> transformExpr expr

getStmtsBlock :: Abs.Stmt -> Abs.Block
getStmtsBlock (Abs.SBStmt _ block) = block
getStmtsBlock stmt = Abs.SBlock undefined [stmt]

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
defaultValue t@(AST.TArray _) = AST.ECastNull t
defaultValue t@(AST.TClass _) = AST.ECastNull t


transformType :: Abs.Type -> TM AST.Type
transformType (Abs.TInt _) = pure AST.TInt
transformType (Abs.TStr _) = pure AST.TStr
transformType (Abs.TBool _) = pure AST.TBool
transformType (Abs.TVoid _) = pure AST.TVoid
transformType (Abs.TArray _ t) = AST.TArray <$> transformType t
transformType (Abs.TClass _ ident) = AST.TClass <$> transformIIdentClass ident

transformExpr :: Abs.Expr -> TM AST.Expr
transformExpr (Abs.EVar _ ident) = transformIIdent ident
transformExpr (Abs.ELitInt _ integer) = pure $ AST.ELitInt integer
transformExpr (Abs.ELitTrue _) = pure AST.ELitTrue
transformExpr (Abs.ELitFalse _) = pure AST.ELitFalse
transformExpr (Abs.ECastNull _ t) = AST.ECastNull <$> transformType t
transformExpr (Abs.EString _ string) = pure $ AST.EString string
transformExpr (Abs.EFunctionCall _ ident exprs) = do
  tExprs <- mapM transformExpr exprs
  transformFun ident tExprs
transformExpr (Abs.EClassNew _ ident) = AST.EClassNew <$> transformIIdentClass ident
transformExpr (Abs.EClassAttr _ expr ident) = AST.EClassAttr <$> transformExpr expr <*> transformIIdentClassAttr ident  -- TODO
transformExpr (Abs.EMethodCall _ expr ident exprs) = AST.EMethodCall <$> transformExpr expr <*> transformIIdentClassMethod ident <*> mapM transformExpr exprs
transformExpr (Abs.EArrayNew _ t expr) = AST.EArrayNew <$> transformType t <*> transformExpr expr
transformExpr (Abs.EArrayElem _ expr expr') = AST.EArrayElem <$> transformExpr expr <*> transformExpr expr'
transformExpr (Abs.ENeg _ (Abs.ENeg _ expr)) = transformExpr expr
transformExpr (Abs.ENeg _ expr) = do
  tExpr <- transformExpr expr
  return $ case tExpr of
    AST.ELitInt integer -> AST.ELitInt (-integer)
    _ -> AST.ENeg tExpr
transformExpr (Abs.ENot _ (Abs.ENot _ expr)) = transformExpr expr
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
        Abs.OMod _ -> AST.ELitInt (integer1 `rem` integer2)
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
    (_, AST.ELitFalse) -> AST.EAnd tExpr1 tExpr2
    (AST.ELitFalse, _) -> AST.ELitFalse
    _ -> AST.EAnd tExpr1 tExpr2
transformExpr (Abs.EOr _ expr1 expr2) = do
  tExpr1 <- transformExpr expr1
  tExpr2 <- transformExpr expr2
  return $ case (tExpr1, tExpr2) of
    (AST.ELitTrue, _) -> AST.ELitTrue
    (_, AST.ELitTrue) -> AST.EOr tExpr1 tExpr2
    (AST.ELitFalse, _) -> tExpr2
    (_, AST.ELitFalse) -> tExpr1
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

transformIIdent :: Abs.IIdent -> TM AST.Expr
transformIIdent (Abs.IIdent _ (Abs.Ident ident)) = do
  env <- gets getEnv
  case Map.lookup ident env of
    Just newIdent -> return $ AST.EVar newIdent
    Nothing ->
      if ident == "self" then return $ AST.EVar "self"
      else do
        transformExpr (Abs.EClassAttr undefined
            (Abs.EVar undefined (Abs.IIdent undefined (Abs.Ident "self"))
          ) (Abs.IIdent undefined (Abs.Ident ident)))

transformIIdent' :: String -> Abs.IIdent -> TM AST.Ident
transformIIdent' s (Abs.IIdent _ (Abs.Ident ident)) = return $ s ++ "." ++ ident

transformIIdentClass :: Abs.IIdent -> TM AST.Ident
transformIIdentClass = transformIIdent' "class"

transformFun :: Abs.IIdent -> [AST.Expr] -> TM AST.Expr
transformFun ident tExprs = do
  methods <- gets getMethods
  if fromIIdent ident `elem` methods then (AST.EMethodCall (AST.EVar "self") <$> transformIIdentClassMethod ident) <*> pure tExprs
  else AST.EFunctionCall <$> transformIIdent' "fun" ident <*> pure tExprs

transformIIdentFun :: Abs.IIdent -> TM AST.Ident
transformIIdentFun = transformIIdent' "fun"

transformIIdentClassAttr :: Abs.IIdent -> TM AST.Ident
transformIIdentClassAttr = transformIIdent' "attr"

transformIIdentClassMethod :: Abs.IIdent -> TM AST.Ident
transformIIdentClassMethod = transformIIdent' "method"

fromIIdent :: Abs.IIdent -> String
fromIIdent (Abs.IIdent _ (Abs.Ident ident)) = ident