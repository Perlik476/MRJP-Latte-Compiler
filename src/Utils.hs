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

import AST

type GenM = StateT GenState IO

data GenState = GenState {
  getCurrentBasicBlock :: BasicBlock,
  getVEnv :: Map String (Map Label Address),
  getRegCount :: Integer,
  getLabelCount :: Integer,
  getBasicBlockEnv :: Map Label BasicBlock,
  getFunctions :: Map String FunBlock,
  getFEnv :: Map String Address,
  getCVenv :: Map String Address,
  getSealedBlocks :: [String],
  getPhiCount :: Integer,
  getIncompletePhis :: Map Label (Map String PhiID),
  getPhiEnv :: Map PhiID Address,
  getVarType :: Map String CType
  -- TODO
}

type PhiID = Integer

getLabel :: GenM String
getLabel =  gets $ getBlockLabel . getCurrentBasicBlock

getInstrs :: GenM [Instr]
getInstrs = gets $ getBlockInstrs . getCurrentBasicBlock

addInstr :: Label -> Instr -> GenM ()
addInstr label instr = do
  blockEnv <- gets getBasicBlockEnv
  block <- gets $ (Map.! label) . getBasicBlockEnv
  currentLabel <- getLabel
  if currentLabel == label then do
    instrs <- gets $ getBlockInstrs . getCurrentBasicBlock
    modify $ \s -> s { getCurrentBasicBlock = block { getBlockInstrs = instrs ++ [instr] } }  -- TODO na odwrót
  else do
    let instrs = getBlockInstrs block
    modify $ \s -> s { getBasicBlockEnv = Map.insert label (block { getBlockInstrs = instrs ++ [instr] }) blockEnv }

getTerminator :: GenM (Maybe Instr)
getTerminator = gets $ getBlockTerminator . getCurrentBasicBlock

addTerminator :: Instr -> GenM ()
addTerminator instr = do
  modify $ \s -> s { getCurrentBasicBlock = (getCurrentBasicBlock s) { getBlockTerminator = Just instr } }

idToLabel :: Integer -> String
idToLabel n = "L" ++ show n

data FunBlock = FunBlock {
  getFunName :: String,
  getFunRetType :: CType,
  getFunArgs :: [(String, CType)],
  getFunBlocks :: [BasicBlock]
}
instance Show FunBlock where
  show (FunBlock name t args blocks) =
    "define " ++ show t ++ " @" ++ name ++ "(" ++
    Data.List.intercalate ", " (map (\(name, t) -> show t ++ " " ++ name) args) ++
    ") {\n" ++ unlines (map show blocks) ++ "}\n"

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
newBasicBlock :: String -> [String] -> GenM ()
newBasicBlock label preds = do
  modify $ \s -> s {
    getCurrentBasicBlock = BasicBlock label [] Nothing preds Map.empty
  }
  modify $ \s -> s {
    getBasicBlockEnv = Map.insert label (getCurrentBasicBlock s) (getBasicBlockEnv s)
  }
  return ()
nothingBlock :: BasicBlock
nothingBlock = BasicBlock "" [] Nothing [] Map.empty

type Label = String

data Instr =
  IBinOp Address Address ArithOp Address |
  IRelOp Address Address RelOp Address |
  IRet Address |
  IJmp Label |
  IBr Address Label Label |
  IPhi' Address PhiID |
  IPhi Address [(Label, Address)]
instance Show Instr where
  show (IBinOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (IRelOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (IRet addr) = "ret " ++ showAddrType addr ++ " " ++ show addr
  show (IJmp label) = "br label %" ++ label
  show (IBr addr label1 label2) = "br i1 " ++ show addr ++ ", label %" ++ label1 ++ ", label %" ++ label2
  show (IPhi' addr phiId) = show addr ++ " = phi " ++ showAddrType addr ++ " " ++ show phiId
  show (IPhi addr vals) = show addr ++ " = phi " ++ showAddrType addr ++ " " ++ Data.List.intercalate ", " (map (\(label, addr) -> "[" ++ show addr ++ ", %" ++ label ++ "]") vals)


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
  EVInt Integer |
  EVBool Bool |
  EVString String
instance Show EVal where
  show (EVInt n) = show n
  show (EVBool b) = show b
  show (EVString s) = show s

getEvalType :: EVal -> CType
getEvalType (EVInt _) = CInt
getEvalType (EVBool _) = CBool
getEvalType (EVString _) = CString

data CType =
  CInt |
  CBool |
  CVoid |
  CString
-- TODO
instance Show CType where
  show CInt = "i32"
  show CBool = "i1"
  show CVoid = "void"
  show CString = "i8*"
