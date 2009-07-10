-- intermediate pretty printer

module Pprint where

class ISEQ a where
      iNil :: a
      iStr :: String -> a
      iAppend :: a -> a -> a
      iNewline :: a
      iIndent :: a -> a
      iDisplay :: a -> String

-- figure this one out
pprExpr :: Expr -> Iseq
pprExpr (Atom v) = iStr v
pprExpr (List es) = foldl1 iAppend . (map pprExpr) es
	     	      	      
{-
pprAExpr :: Expr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
	   | otherwise = iConcat [iStr "(", pprExpr e, iStr ")"]
-}

--here
pprProgram :: Prog -> Iseq
pprProgram p = iInterleave (iStr " ") (map pprAExpr p)

pprDefns :: [(Name, Expr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
	 where sep = iConcat [ iStr ";", iNewline]

pprDefn :: (Name, Expr) -> Iseq
pprDefn (name, expr)
	= iConcat [IStr	 name, iStr " = ", iIndent (pprExpr expr)]

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave iseq xs = iConcat (foldl1 (++) (zipWith (\x y -> [x,y])
						    xs
						    (repeat iseq)))

--iConcat2 :: [Iseq] -> Iseq
iConcat2 = foldl1 (+)

--iInterleave2 :: Iseq -> [Iseq] -> Iseq
iInterleave2 iseq xs = iConcat2 (foldl1 (++) (zipWith (\x y -> [x,y])
	    	      	      	     	   	    (repeat iseq)
						    xs))
data Iseq = INil
     	  | IStr String
	  | IAppend Iseq Iseq

instance ISEQ Iseq where
  iNil = INil
  iAppend = IAppend
  iStr = IStr
  iIndent seq = seq
  iNewline = IStr "\n"
  iDisplay seq = flatten [seq]

flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ (flatten seqs)
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1:seq2:seqs)

