module Emitter where

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
import Utils

emit :: String -> GenM ()
emit instr = do
  modify $ addInstr instr

addInstr :: String -> GenState -> GenState
addInstr instr state = state { getInstrs = getInstrs state ++ [instr] }

emitBinOp :: Address -> Address -> ArithOp -> Address -> GenM ()
emitBinOp addr addr1 op addr2 = do
  emit $ show addr ++ " = " ++ show op ++ " " ++ showAddrType addr ++ " " ++ show addr1 ++ ", " ++ show addr2

