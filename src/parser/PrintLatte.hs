-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintLatte.

module PrintLatte where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsLatte

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsLatte.Ident where
  prt _ (AbsLatte.Ident i) = doc $ showString i
instance Print (AbsLatte.Program' a) where
  prt i = \case
    AbsLatte.PProgram _ topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print (AbsLatte.TopDef' a) where
  prt i = \case
    AbsLatte.PFunDef _ type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print [AbsLatte.TopDef' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsLatte.Arg' a) where
  prt i = \case
    AbsLatte.PArg _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [AbsLatte.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsLatte.ArrayElem' a) where
  prt i = \case
    AbsLatte.ArrayElem _ lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print (AbsLatte.ClassAttr' a) where
  prt i = \case
    AbsLatte.ClassAttr _ lvalue id_ -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "."), prt 0 id_])

instance Print (AbsLatte.MethodCall' a) where
  prt i = \case
    AbsLatte.MethodCall _ lvalue id_ exprs -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "."), prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])

instance Print (AbsLatte.FunctionCall' a) where
  prt i = \case
    AbsLatte.FunctionCall _ id_ exprs -> prPrec i 0 (concatD [prt 0 id_, doc (showString "("), prt 0 exprs, doc (showString ")")])

instance Print (AbsLatte.Lvalue' a) where
  prt i = \case
    AbsLatte.LIdent _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsLatte.LArrayElem _ arrayelem -> prPrec i 0 (concatD [prt 0 arrayelem])
    AbsLatte.LClassAttr _ classattr -> prPrec i 0 (concatD [prt 0 classattr])
    AbsLatte.LMethodCall _ methodcall -> prPrec i 0 (concatD [prt 0 methodcall])
    AbsLatte.LFuntionCall _ functioncall -> prPrec i 0 (concatD [prt 0 functioncall])

instance Print (AbsLatte.Block' a) where
  prt i = \case
    AbsLatte.SBlock _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsLatte.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsLatte.Stmt' a) where
  prt i = \case
    AbsLatte.SEmpty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsLatte.SBStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    AbsLatte.SDecl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsLatte.SAss _ lvalue expr -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsLatte.SIncr _ lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "++"), doc (showString ";")])
    AbsLatte.SDecr _ lvalue -> prPrec i 0 (concatD [prt 0 lvalue, doc (showString "--"), doc (showString ";")])
    AbsLatte.SRet _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsLatte.SVRet _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsLatte.SCond _ expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsLatte.SCondElse _ expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    AbsLatte.SWhile _ expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsLatte.SExp _ expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])

instance Print (AbsLatte.Item' a) where
  prt i = \case
    AbsLatte.SNoInit _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsLatte.SInit _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [AbsLatte.Item' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsLatte.Type' a) where
  prt i = \case
    AbsLatte.TInt _ -> prPrec i 0 (concatD [doc (showString "int")])
    AbsLatte.TStr _ -> prPrec i 0 (concatD [doc (showString "string")])
    AbsLatte.TBool _ -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsLatte.TVoid _ -> prPrec i 0 (concatD [doc (showString "void")])
    AbsLatte.TArray _ type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])
    AbsLatte.TClass _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    AbsLatte.TFun _ type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])

instance Print [AbsLatte.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsLatte.Expr' a) where
  prt i = \case
    AbsLatte.EVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsLatte.ELitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsLatte.ELitTrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    AbsLatte.ELitFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    AbsLatte.EString _ str -> prPrec i 6 (concatD [printString str])
    AbsLatte.ECastNull _ type_ -> prPrec i 6 (concatD [doc (showString "("), prt 0 type_, doc (showString ")null")])
    AbsLatte.EArrayElem _ arrayelem -> prPrec i 6 (concatD [prt 0 arrayelem])
    AbsLatte.EClassNew _ id_ -> prPrec i 6 (concatD [doc (showString "new"), prt 0 id_])
    AbsLatte.EClassAttr _ classattr -> prPrec i 6 (concatD [prt 0 classattr])
    AbsLatte.EMethodCall _ methodcall -> prPrec i 6 (concatD [prt 0 methodcall])
    AbsLatte.EFuntionCall _ functioncall -> prPrec i 6 (concatD [prt 0 functioncall])
    AbsLatte.ENeg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsLatte.ENot _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsLatte.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsLatte.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsLatte.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsLatte.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsLatte.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [AbsLatte.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsLatte.AddOp' a) where
  prt i = \case
    AbsLatte.OPlus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsLatte.OMinus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsLatte.MulOp' a) where
  prt i = \case
    AbsLatte.OTimes _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsLatte.ODiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsLatte.OMod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsLatte.RelOp' a) where
  prt i = \case
    AbsLatte.OLTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsLatte.OLE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsLatte.OGTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsLatte.OGE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsLatte.OEQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsLatte.ONE _ -> prPrec i 0 (concatD [doc (showString "!=")])