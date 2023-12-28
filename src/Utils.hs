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

import Data.Map (Map, empty, fromList, union, member, lookup, insert, toList, keys, difference, intersection, elems, (!), intersectionWith)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.List

import AST

type GenM = StateT GenState IO

data GenState = GenState {
  getCurrentBasicBlock :: BasicBlock,
  getVEnv :: Map String Address,
  getRegCount :: Integer,
  getLabelCount :: Integer,
  getBasicBlockEnv :: Map String BasicBlock,
  getFunctions :: Map String FunBlock,
  getFEnv :: Map String Address,
  getCVenv :: Map String Address
  -- TODO
}

getLabel :: GenM String
getLabel =  gets $ getBlockLabel . getCurrentBasicBlock

getInstrs :: GenM [Instr]
getInstrs = gets $ getBlockInstrs . getCurrentBasicBlock

addInstr :: Instr -> GenM ()
addInstr instr = do
  instrs <- getInstrs
  modify $ \s -> s { getCurrentBasicBlock = (getCurrentBasicBlock s) { getBlockInstrs = instrs ++ [instr] } }

getTerminator :: GenM (Maybe Instr)
getTerminator = gets $ getBlockTerminator . getCurrentBasicBlock

addTerminator :: Instr -> GenM ()
addTerminator instr = do
  modify $ \s -> s { getCurrentBasicBlock = (getCurrentBasicBlock s) { getBlockTerminator = Just instr } }

idToLabel :: Integer -> String
idToLabel n = "L" ++ show n

freshLabel :: GenM String
freshLabel = do
  modify $ \s -> s { getLabelCount = getLabelCount s + 1 }
  gets $ idToLabel . getLabelCount

data FunBlock = FunBlock String CType [(String, CType)] [BasicBlock]
instance Show FunBlock where
  show (FunBlock name t args blocks) = 
    "define " ++ showType t ++ " @" ++ name ++ "(" ++ 
    Data.List.intercalate ", " (map (\(name, t) -> showType t ++ " " ++ name) args) ++ 
    ") {\n" ++ unlines (map show blocks) ++ "}\n"

data BasicBlock = BasicBlock {
  getBlockLabel :: String,
  getBlockInstrs :: [Instr],
  getBlockTerminator :: Maybe Instr,
  getBlockPredecessors :: [String]
}
instance Show BasicBlock where
  show (BasicBlock label instrs (Just terminator) preds) = 
    label ++ ":  ; preds: " ++ Data.List.intercalate ", " preds ++ "\n" ++ unlines (map (("  " ++) . show) (instrs ++ [terminator]))
newBasicBlock :: String -> [String] -> GenM ()
newBasicBlock label preds = do
  modify $ \s -> s { getCurrentBasicBlock = BasicBlock label [] Nothing preds }
  return ()
nothingBlock :: BasicBlock
nothingBlock = BasicBlock "" [] Nothing []

type Label = String

data Instr =
  IBinOp Address Address ArithOp Address |
  IRelOp Address Address RelOp Address |
  IRet Address |
  IJmp Label |
  IBr Address Label Label
instance Show Instr where
  show (IBinOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (IRelOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr1 ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (IRet addr) = "ret " ++ showAddrType addr ++ " " ++ show addr
  show (IJmp label) = "br label %" ++ label
  show (IBr addr label1 label2) = "br i1 " ++ show addr ++ ", label %" ++ label1 ++ ", label %" ++ label2


data Address =
  AImmediate Integer CType |
  ARegister Integer CType
-- TODO

instance Show Address where
  show (AImmediate n _) = show n
  show (ARegister n _) = "%r" ++ show n

showAddrType :: Address -> String
showAddrType (AImmediate _ t) = showType t
showAddrType (ARegister _ t) = showType t

showType :: CType -> String
showType CInt = "i32" -- TODO
showType CBool = "i1"
showType CVoid = "void"
showType CString = "i8*"
-- TODO

data CType =
  CInt |
  CBool |
  CVoid |
  CString
-- TODO
