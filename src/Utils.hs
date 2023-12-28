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
  getInstrs :: [Instr],
  getVEnv :: Map String Address,
  getRegCount :: Integer,
  getBasicBlockEnv :: Map String BasicBlock,
  getBasicBlockCount :: Integer,
  getFunctions :: Map String FunBlock,
  getFEnv :: Map String Address,
  getCVenv :: Map String Address
  -- TODO
}

data FunBlock = FunBlock String CType [(String, CType)] [BasicBlock]
instance Show FunBlock where
  show (FunBlock name t args blocks) = 
    "define " ++ showType t ++ " @" ++ name ++ "(" ++ 
    Data.List.intercalate ", " (map (\(name, t) -> showType t ++ " " ++ name) args) ++ 
    ") {\n" ++ unlines (map show blocks) ++ "}\n"
data BasicBlock = BasicBlock Label [Instr]
instance Show BasicBlock where
  show (BasicBlock label instrs) = label ++ ":\n" ++ unlines (map show instrs)
type Label = String
data Instr =
  IBinOp Address Address ArithOp Address |
  IRet Address |
  IBr Address Label Label
instance Show Instr where
  show (IBinOp addr addr1 op addr2) = show addr ++ " = " ++ show op ++ " " ++ showAddrType addr ++ " " ++ show addr1 ++ ", " ++ show addr2
  show (IRet addr) = "ret " ++ showAddrType addr ++ " " ++ show addr
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
