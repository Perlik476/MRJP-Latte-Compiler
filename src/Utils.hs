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

type GenM = StateT GenState IO

data GenState = GenState {
  getInstrs :: [String],
  getRegEnv :: Map String Address,
  getRegCount :: Integer,
  getFEnv :: Map String Address,
  getCVenv :: Map String Address
  -- TODO
}

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
showType CInt = "i32"
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
