module Parser where

import Text.ParserCombinators.Parsec --hiding (spaces)
import Monad


pProg :: Parser Prog
pProg = sepBy1 pSc (do spaces
      	       	       char ';'
		       spaces)

pExpr :: Parser Expr
pExpr = do pExpr
      	   pAexpr
	<|> pLet 
      	<|> pLetRec 
	<|> pCase
	<|> pLambda
	<|> pAexpr

pAExpr :: Parser Expr
pAExpr = pStr <|>
         pAtom <|>
	 pNum <|>
	 pList

pLambda :: Parser Expr
pLambda = do char '^'
	     args <- sepBy pAtom spaces
	     char '.'
	     body <- pExpr
	     return $ Lambda args body

{-
pLet :: Parser Expr
pLet = do string "let"
       	  defs <- sepBy1 pDefs  
          return 
-}

pStr :: Parser Expr
pStr = do char '"'
	  s <- many (noneOf "\"")
	  char '"'
	  return $ Str s

pAtom :: Parser Expr 
pAtom = do x <- letter 
	   xs <- many (letter <|> digit) 
	   return $ Atom (x:xs)

pNum :: Parser Expr
pNum = liftM (Num . read) $ many1 digit

pList :: Parser Expr
pList =  do char '('
      	    spaces
	    es <- sepBy pExpr spaces
	    spaces
	    char ')'
	    return $ List es

