module Utils where

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
import qualified Data.Char
import qualified Numeric

import AST

type GenM = StateT GenState IO

data GenState = GenState {
  getCurrentLabel :: Label,
  getCurrentFunLabels :: [Label],
  getCurrentFunName :: String,
  getVEnv :: Map String (Map Label Address),
  getRegCount :: Integer,
  getLabelCount :: Integer,
  getBasicBlockEnv :: Map Label BasicBlock,
  getFunctions :: Map String FunBlock,
  getFEnv :: Map Ident FunType,
  getCEnv :: Map Ident CType,
  getSealedBlocks :: [String],
  getPhiCount :: Integer,
  getIncompletePhis :: Map Label (Map String PhiID),
  getPhiEnv :: Map PhiID Address,
  getVarType :: Map String CType,
  getStringPool :: Map String Integer,
  getStringPoolCount :: Integer,
  getInternalVarIdentCount :: Integer
  -- TODO
}


type PhiID = Integer

getCurrentBasicBlock :: GenM BasicBlock
getCurrentBasicBlock = do
  label <- gets getCurrentLabel
  gets $ (Map.! label) . getBasicBlockEnv

getLabel :: GenM Label
getLabel = gets getCurrentLabel

getInstrs :: GenM [Instr]
getInstrs = getCurrentBasicBlock >>= return . getBlockInstrs

addInstr :: Label -> Instr -> GenM ()
addInstr label instr = do
  blockEnv <- gets getBasicBlockEnv
  block <- gets $ (Map.! label) . getBasicBlockEnv
  let instrs = getBlockInstrs block
  modify $ \s -> s { getBasicBlockEnv = Map.insert label (block { getBlockInstrs = instrs ++ [instr] }) blockEnv }
  liftIO $ putStrLn $ "Added instruction " ++ show instr ++ " to block " ++ label

getTerminator :: GenM (Maybe Instr)
getTerminator = getCurrentBasicBlock >>= return . getBlockTerminator

addTerminator :: Instr -> GenM ()
addTerminator instr = do
  blockEnv <- gets getBasicBlockEnv
  block <- getCurrentBasicBlock
  modify $ \s -> s { getBasicBlockEnv = Map.insert (getCurrentLabel s) (block { getBlockTerminator = Just instr }) blockEnv }

getPreds :: GenM [Label]
getPreds = getCurrentBasicBlock >>= return . getBlockPredecessors

addPredToBlock :: Label -> Label -> GenM ()
addPredToBlock label pred = do
  block <- gets $ (Map.! label) . getBasicBlockEnv
  let preds = getBlockPredecessors block
  modify $ \s -> s { getBasicBlockEnv = Map.insert label (block { getBlockPredecessors = pred : preds }) (getBasicBlockEnv s) }

addPred :: Label -> GenM ()
addPred pred = do
  label <- getLabel
  addPredToBlock label pred

addPreds :: [Label] -> GenM ()
addPreds preds = do
  label <- getLabel
  mapM_ (addPredToBlock label) preds

idToLabel :: Integer -> String
idToLabel n = "L" ++ show n

data FunBlock = FunBlock {
  getFunName :: String,
  getFunRetType :: CType,
  getFunArgs :: [(Address, CType)], -- TODO ctype chyba niepotrzebny
  getFunBlocks :: [BasicBlock]
}
instance Show FunBlock where
  show (FunBlock name t args blocks) =
    "define " ++ show t ++ " @" ++ name ++ "(" ++
    Data.List.intercalate ", " (map (\(addr, t) -> show t ++ " " ++ show addr) args) ++
    ") {\n" ++ unlines (map show blocks) ++ "}\n"

data FunType = FunType {
  getFunTypeEntryLabel :: Label,
  getFunTypeRet :: CType,
  getFunTypeArgs :: [(Address, CType)]
} deriving (Show)

data BasicBlock = BasicBlock {
  getBlockLabel :: String,
  getBlockInstrs :: [Instr],
  getBlockTerminator :: Maybe Instr,
  getBlockPredecessors :: [Label],
  getPhis :: Map Integer Address
}
instance Show BasicBlock where
  show (BasicBlock label instrs (Just terminator) preds _) =
    label ++ ":  ; preds: " ++ Data.List.intercalate ", " preds ++ "\n" ++ unlines (map (("  " ++) . show) (instrs ++ [terminator]))
  show (BasicBlock label instrs Nothing preds _) =
    label ++ ":  ; ERROR preds: " ++ Data.List.intercalate ", " preds ++ "\n" ++ unlines (map (("  " ++) . show) instrs)
nothingBlock :: BasicBlock
nothingBlock = BasicBlock "" [] Nothing [] Map.empty

type Label = String

data Instr =
  IComment String |
  IBinOp Address Address ArithOp Address |
  IRelOp Address Address RelOp Address |
  ICall Address String [Address] |
  IVCall String [Address] |
  IRet Address |
  IVRet |
  IJmp Label |
  IBr Address Label Label |
  IPhi' Address PhiID |
  IPhi Address [(Label, Address)] |
  IString Address Integer Integer |
  IBitcast Address Address |
  IStore Address Address |
  ILoad Address Address |
  IGetElementPtr Address Address [Address]
instance Show Instr where
  show (IComment str) = "; " ++ str
  show (IBinOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (IRelOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (ICall addr name args) =
    show addr ++ " = call " ++ showAddrType addr ++ " @" ++ name ++ "(" ++ Data.List.intercalate ", " (
      map (\arg -> showAddrType arg ++ " " ++ show arg) args) ++ ")"
  show (IVCall name args) = "call void @" ++ name ++ "(" ++ Data.List.intercalate ", " (
      map (\arg -> showAddrType arg ++ " " ++ show arg) args) ++ ")"
  show (IRet addr) = "ret " ++ showAddrType addr ++ " " ++ show addr
  show IVRet = "ret void"
  show (IJmp label) = "br label %" ++ label
  show (IBr addr label1 label2) = "br i1 " ++ show addr ++ ", label %" ++ label1 ++ ", label %" ++ label2
  show (IPhi' addr phiId) = show addr ++ " = phi " ++ showAddrType addr ++ " " ++ show phiId
  show (IPhi addr vals) = show addr ++ " = phi " ++ showAddrType addr ++ " " ++ Data.List.intercalate ", " (map (\(label, addr) -> "[" ++ show addr ++ ", %" ++ label ++ "]") vals)
  show (IString addr ident len) = show addr ++ " = bitcast [" ++ show len ++ " x i8]* " ++ showStrName ident ++ " to i8*"
  show (IBitcast addr1 addr2) = show addr1 ++ " = bitcast " ++ showAddrType addr2 ++ " " ++ show addr2 ++ " to " ++ showAddrType addr1
  show (IStore addr1 addr2) = "store " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ showAddrType addr2 ++ " " ++ show addr2
  show (ILoad addr1 addr2) = show addr1 ++ " = load " ++ showAddrType addr1 ++ ", " ++ showAddrType addr1 ++ "* " ++ show addr2
  show (IGetElementPtr addr1 addr2 args) =
    show addr1 ++ " = getelementptr " ++ show (dereferencedType (getAddrType addr2)) ++ ", " ++ showAddrType addr2 ++ " " ++ show addr2 ++ ", " ++
    Data.List.intercalate ", " (map (\arg -> showAddrType arg ++ " " ++ show arg) args)
    where dereferencedType (CPtr t) = t
          dereferencedType t = error $ "Cannot dereference type " ++ show t

showStrName :: Integer -> String
showStrName n = "@str." ++ show n

showStrPool :: (String, Integer) -> String
showStrPool (str, n) = 
  showStrName n ++ " = private unnamed_addr constant [" ++ show (length str + 1) ++ " x i8] c\"" ++ concatMap encodeChar str ++ "\\00\""

encodeChar :: Char -> String
encodeChar c = "\\" ++ Numeric.showHex (Data.Char.ord c) ""



showClass :: (String, CType) -> String
showClass (name, t) = "%" ++ name ++ " = type " ++ show t


data Address =
  AImmediate EVal |
  ARegister Integer CType |
  APhi PhiID CType [(Label, Address)]
-- TODO
instance Show Address where
  show (AImmediate val) = show val
  show (ARegister n _) = "%r" ++ show n
  show (APhi _ t vals) = error "Phi should not be used in this context"
  -- TODO

getAddrType :: Address -> CType
getAddrType (AImmediate val) = getEvalType val
getAddrType (ARegister _ t) = t
getAddrType (APhi _ t _) = t


showAddrType :: Address -> String
showAddrType = show . getAddrType
-- TODO

data EVal =
  EVUndef CType |
  EVVoid |
  EVInt Integer |
  EVBool Bool |
  EVNull CType
instance Show EVal where
  show (EVUndef t) = "undef"
  show EVVoid = ""
  show (EVInt n) = show n
  show (EVBool b) = if b then "1" else "0"
  show (EVNull t) = "null"

getEvalType :: EVal -> CType
getEvalType (EVInt _) = CInt
getEvalType (EVBool _) = CBool
getEvalType EVVoid = CVoid
getEvalType (EVUndef t) = t
getEvalType (EVNull t) = t


data CType =
  CInt |
  CBool |
  CVoid |
  CChar |
  CString | -- TODO
  CPtr CType |
  CClass Ident |
  CStruct [String] (Map String CType)
-- TODO
  deriving (Eq, Ord, Read)
instance Show CType where
  show CInt = "i32"
  show CBool = "i1"
  show CVoid = "void"
  show CChar = "i8"
  show CString = "i8*"
  show (CPtr t) = show t ++ "*"
  show (CClass ident) = "%" ++ ident
  show (CStruct fieldNames fields) = "{" ++ Data.List.intercalate ", " (map (\name -> show (fields Map.! name)) fieldNames) ++ "}"

classToStruct :: CType -> GenM CType
classToStruct (CClass ident) = do
  cenv <- gets getCEnv
  case Map.lookup ident cenv of
    Just t -> return t
    Nothing -> error $ "Class " ++ show ident ++ " not found"

getTypeSize :: CType -> Integer
getTypeSize CInt = 4
getTypeSize CBool = 1
getTypeSize CVoid = 0
getTypeSize CString = 8
getTypeSize (CPtr _) = 8
getTypeSize (CStruct _ fields) = sum $ map getTypeSize $ Map.elems fields  -- TODO