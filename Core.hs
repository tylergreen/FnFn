module Core where

import Text.ParserCombinators.Parsec --hiding (spaces)
import Monad
import Utils
import Heap

----------------
-- Top Level

runProg :: String -> String
runProg = showResults . eval . compile . parse

type GmState = (GhCode, GmStack, GmHeap, GmGlobals, GmStats)

type GmCode = [Instruction]

getCode :: GMState -> GmCode
getCode   (i,s,h,gs,stats) = i
putCode i (_,s,h,gs,stats) = (i,s,h,g,stats)

data Instruction  
    =   Unwind
    | PushG Name
    | PushI Int
    | Push Int
    | Mkap
    | Slide Int
      deriving (Eq)

instance Eq Instruction where
  Unwind  == Unwind  = True
  PushG a == PushG b = a == b
  PushI a == PushI b = a == b
  Push a  == Push b  = a == b
  Mkap    == Mkap    = True
  Slide a == Slide b = False
  _ == _             = False 

type GmStack = [Addr]

getStack :: GmStack -> GmStack
getStack (i, stack, heap, globals, stats) = stack
putStack stack (i, _, heap, globals, stats) 
  = (i, stack, heap, globals, stats) 

type GmHeap = Heap Node
getHeap (i, stack, heap, globals, stats) = heap
putHeap heap (i, stack, _, globals, stats)
 = (i, stack, heap, globals, stats)


data Node = NNum Int
          | NAp Addr Addr
	  | NGlobal Int GmCode

type GmGlobals = [(Name, Addr)]

getGlobals (i,s,h,gs,stats) = gs

---------------
-- Statistics

type GmStats = Int

statInit = 0
statIncSteps = (+1)
statGetSteps s = s

getStats (i,s,h,gs,stats) = stats
putStats s (i,_,h,gs,stats) = (i,s,h,gs,stats)

--------------
-- evaluator

eval :: GmState -> [GmState]
eval = unfold gmFinal id (doAdmin . step)

doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal s = case (getCode s) of
		[] -> True
		_ -> False

step :: GmState -> GmState
step state = let (i:is) = getCode state
        	 state1 = putCode is state
	      in case i of 
		 (PushG f) -> pushglobal f state
		 (PushI nf) -> pushint n state
		 Mkap -> mkap state
		 (Push n) -> push n state
		 (Slide n) -> slide n state
		 Unwind -> unwind state

pushglobal f state 
  = putStack (a:getStack state) state
    where a = aLookup (getGlobals state) f
    	      	      $ error $ "Undeclared global " ++ f

pushint n state
  = putHeap heap1 (putStack (a:getStack state) state)
    where (heap1, a) = hAlloc (getHeap state) (NNum n)


-- construct an application node in the heap
mkap state = putHeap heap1 (putStack (a:as1) state)
     	     where (heap1, a) = hAlloc (getHeap state) (NAp a1 a2)
	     	   (a1:a2:as1) = getStack state

push n state = putStack (a:as) state	  
       	     where as = getStack state
	     	   a = getArg $ hLookup (getHeap state) (as !! (n+1))

getArg :: Node -> Addr
getArg (NAp _ a2) = a2

slide n state  
 = putStack (a: drop n as) state
   where (a:as) = getStack state

unwind state
 = newState (hLookup heap a)
   where
   (a:as) = getStack state
   heap = getHeap state

type Name = String

data Expr 
      = Atom Name
      | Num Int
      | Str String
      | Constr Int Int -- Constructor tag
      | App Expr Expr -- Application
      | Lambda [Name] Expr
      | Let IsRec [(Name, Expr)] Expr
      deriving Show

data IsRec = Rec
     	   | NonRec
  deriving Show
 
type ScDefn = (Name, [Name], Expr)

lambdaSym = "^"
defnSym = "$"

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]
lhssOf defns = [name | (name, rhs) <- defns]

isAtomicExpr :: Expr -> Bool
isAtomicExpr (Atom _) = True
isAtomicExpr (Num _) = True
isAtomicExpr (Str _) = True
isAtomicExpr e = False

type Prog =  [ScDefn]

-- Std Prelude
preludeDefs :: Prog
preludeDefs
	= [("I", ["x"], (Atom "x")),
           ("K", ["x","y"], (Atom "x")),
           ("K1", ["x","y"], (Atom "y")),
           ("S", ["f","g","x"], (App
				   (App (Atom "f")(Atom "x"))
                                   (App (Atom "g")(Atom "x")))),
           ("B", ["f","g", "x"], (App (Atom "f")
	                            (App (Atom "g") (Atom "x")))),
           ("M", ["x"], (App (Atom "x")(Atom "x"))),
	   ("twice", ["f"], (App (App (Atom "B") (Atom "f")) (Atom "f")))]

primitives :: [(Name, Primitive)]
primitives = [("neg", Neg),
	      ("+", Add),
	      ("-", Sub),
	      ("*", Mul),
	      ("/", Div)]

--mkMultiAp n e1 e2 = foldl EAp e1 (take n (repeat e2))

-----------------------------
---- Pretty Printer

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
pprExpr (Str s) = iStr s	
pprExpr (App a b) = iConcat[iStr "(",
	         	    pprExpr a,
			    iStr " ",
			    pprExpr b,
			    iStr ")"]
pprExpr (Lambda args body) = iConcat[ iStr lambdaSym,
		     	     	      iStr "(",
		     	     	      iInterleave (iStr " ")
				                  (map iStr args),
				      iStr ".",
				      pprExpr body,
				      iStr ")"]
				      
--here
pprProg :: Prog -> Iseq
pprProg p = iInterleave iNewline (map pprDef p)

pprDef :: ScDefn -> Iseq
pprDef (name, args, expr)
	= iConcat [iStr defnSym,
	  	   iStr "(",
		   iStr name,
		   iInterleave (iStr " ") (map iStr args),
		   iStr " = ",
		   pprExpr expr]

iConcat :: [Iseq] -> Iseq
iConcat = foldl iAppend iNil

-- this works but its still prob. quadratic.  Also ugly as hell
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave iseq xs = iConcat (aux iseq xs)
	    where aux sep (y:ys) = if (null ys)
	        	           then [y]
				   else [y, sep] ++ (aux iseq ys)
data Iseq = INil
     	  | IStr String
	  | IAppend Iseq Iseq

instance ISEQ Iseq where
  iNil = INil
  iAppend = IAppend
  iStr = IStr
  iIndent seq = seq
  iNewline = IStr "\n "  -- can't seem to get a good newline
  iDisplay seq = flatten [seq]

flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ (flatten seqs)
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1:seq2:seqs)

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n
  = let n1 = show n in 
      iStr $ (spcs (width - length n1)) ++ n1


iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
        	where lay_item (n, seq) = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]
		 
-------------------------
--- Parser

-- need to encode the list shorthand, ie
-- (A x y z) = (((Ax) y ) z)

parseProg :: Parser Prog
parseProg = sepBy1 pSc spaces

pSc :: Parser ScDefn
pSc = do char '('
    	 name <- word
	 spaces
	 args <- sepBy word spaces
	 char ')'
	 spaces
	 char '='
	 spaces
	 body <- pExpr
	 return $ (name, args, body)

pExpr :: Parser Expr
pExpr = pStr <|>
        pAtom <|>
	pNum <|>
	pApp <|>
	pLet <|>
	pLam


-- ^(a 5) (b 3) $ (a b 3)

pBind :: Parser (String, Expr)
pBind = do char '('
      	   spaces
	   sym <- word
	   spaces
	   body <- pExpr
	   char ')'
	   return $ (sym, body)
	   
pLet :: Parser Expr
pLet = do char '^'
	  bindings <- many1 pBind
	  spaces
	  char '$'
	  spaces
	  body <- pExpr
	  return $ Let Rec bindings body

pStr :: Parser Expr
pStr = do char '"'
	  s <- many (noneOf "\"")
	  char '"'
	  return $ Str s

pAtom :: Parser Expr 
pAtom = do x <- letter <|> (oneOf "+-*/")
	   xs <- many (letter <|> digit) 
	   return $ Atom (x:xs)

pNum :: Parser Expr
pNum = liftM (Num . read) $ many1 digit

-- pApp hold the key to the left assoc. shorthand.
-- update your prolog to utilize this breakthrough
-- want to possibly end a list like: (a b c ) but can't *
pApp :: Parser Expr
pApp =  do char '('
           spaces
	   es <- sepBy1 pExpr spaces  -- * because of here
	   char ')'
	   return $ foldl1 App es

word :: Parser String
word = many1 letter

pLam :: Parser Expr
pLam = do string lambdaSym
       	  char '('
       	  spaces
       	  args <- sepBy word spaces     
	  spaces
	  char '.'
	  spaces
	  body <- pExpr 
	  spaces
	  char ')'
	  return $ Lambda args body

------------------
-- Compiler

-- template instantiation interpreter for my lispy version of the core language

--let StMach "State Machine" = TiState from text  

runProg :: String -> String

compile :: Prog -> TiState

readin :: String -> Prog
readin input = case parse parseProg "" input of
       Left err -> [("monk", [], (Num 5))]
       Right val -> val
 
-------------------
----- Top Level

runProg = showResults . eval . compile . readin
runProg2 = showVal . eval1 . compile . readin

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]

type TiDump = [TiStack]
initTiDump = []

type TiHeap = Heap Node

data Node = NAp Addr Addr
     	  | NSupercomb Name [Name] Expr
	  | NNum Int
	  | NInd Addr
	  | NPrim Name Primitive


data Primitive = Neg | Add | Sub | Mul | Div

type TiGlobals = [(Name,Addr)]

tiStatInit :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int

type TiStats = Int
tiStatInit = 0
tiStatIncSteps = (+1)
tiStatGetSteps s = s

------------
----- The actual compiler

compile program = (initStack, initTiDump, initHeap, globals, tiStatInit)
	where sc_defs = program ++ preludeDefs ++ extraDefs
	      (initHeap, globals) = buildInitHeap sc_defs
	      initStack = [address_of_main]
	      address_of_main = case lookup "main" globals of
	      		          Nothing -> (error "no main")
				  Just x -> x

extraDefs = []

-- can you redo this as an instance of a state monad?
buildInitHeap :: [ScDefn] -> (TiHeap, TiGlobals)
buildInitHeap sc_defs 
 = (heap2, sc_addrs ++ prim_addrs)
   where
   (heap1, sc_addrs) = mapAccuml allocateSc hInit sc_defs
   (heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives

allocateSc :: TiHeap -> ScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) 
  = (heap1, (name, addr))
    	 where (heap1, addr) = hAlloc heap (NSupercomb name args body)

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim)
 = (heap1, (name, addr))
   where
   (heap1, addr) = hAlloc heap (NPrim name prim)

------------------
-- the evaluator

eval :: TiState -> [TiState]
eval = unfold tiFinal id (doAdmin . step)

eval1 = unfold1 tiFinal id (doAdmin . step)

doAdmin :: TiState -> TiState
doAdmin = applyToStats tiStatIncSteps 

tiFinal :: TiState -> Bool
tiFinal ([x], [], heap, globals, stats) -- dump must be empty
	= isDataNode (hLookup heap x)

tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False -- Stack contains more than one item

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode _ = False

-- Rule 2.3: UNWIND
step :: TiState -> TiState
step state 
 = let (stack, dump, heap, globals, stats) = state in
  case hLookup heap (hd stack) of
   (NNum n) -> numStep state n
   (NAp a b) -> apStep state a b
   (NSupercomb sc args body) -> scStep state sc args body
   (NInd addr) -> inStep state addr
   (NPrim n f) -> primStep state f

-- Rule 2.7
numStep :: TiState -> Int -> TiState
numStep ([a], (s:d), heap, globals, stats) n = 
  case hLookup heap a of
       (NNum n) -> (s, d, heap, globals, stats)
       _ -> (error "number used as application 1")

numStep _ _ = (error "number used as application 2")

apStep :: TiState -> Addr -> Addr -> TiState
apStep (a:stack, dump, heap, globals, stats) a1 a2
       = case  hLookup heap a2 of
       	 (NInd a3) -> (a:stack, dump, hUpdate heap a (NAp a1 a3), globals, stats)
	 _ -> (a1:a:stack, dump, heap, globals, stats)
       	 

-- Rule 2.2: REDUCE
scStep :: TiState -> Name -> [Name] -> Expr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name args body
       = (new_stack, dump, heap2, globals, stats)
            where arg_len = length args
	    	  new_stack = result_addr : (drop (1 + arg_len) stack)
	          (heap1, result_addr) = instantiate body heap env
		  heap2 = hUpdate heap1 (stack !! arg_len) (NInd result_addr)
		  env = arg_bindings ++ globals
		  arg_bindings = zip args (getargs heap stack)

inStep (a:stack, dump, heap, globals, stats) a1 =
       (a1:stack, dump, heap, globals, stats)

primStep state Neg = primNeg state
{-
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state (div)
-}

{-
primNeg ([a,a1], dump, heap, globals, stats)
 = let (NAp a b) = hLookup heap a1 in
      case hLookup heap b of 
       	  (NNum n) ->                                 -- rule 2.5
	   ([a1], dump, hUpdate heap a1 (NNum (negate n)), globals, stats)
	  _ -> ([b], [a1]:dump, heap, globals, stats) -- rule 2.9
-}

primNeg ([a,a1], dump, heap, globals, stats)
 = case hLookup heap b of 
       	  (NNum n) ->                                 -- rule 2.5
	   ([a1], dump, hUpdate heap a1 (NNum (negate n)), globals, stats)
	  _ -> ([b], [a1]:dump, heap, globals, stats) -- rule 2.9
   where (NAp a b) = hLookup heap a1

{-
primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state prim
 = ([a2], dump, hUpdate heap a2 (NNum (prim b c)), globals, stats)
 where 
      ([a,a1,a2], dump, heap, globals, stats) = state
      (NAp _ b) = hLookup heap a1
      (NAp _ c) = hLookup heap a2
      (NNum n1) = hLookup heap b
      (NNum n2) = hLookup heap c
-}
      
getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack)
 = map get_arg stack
       where get_arg addr = arg
             (NAp fn arg) = hLookup heap addr

--workhorse function.  Does the interesting work
instantiate :: Expr -> TiHeap -> [(Name,Addr)] -> (TiHeap, Addr)
instantiate (Num n) heap env = hAlloc heap (NNum n)
instantiate (App e1 e2) heap env 
  = hAlloc heap2 (NAp a1 a2) 
    where (heap1, a1) = instantiate e1 heap env
    	  (heap2, a2) = instantiate e2 heap1 env

instantiate (Atom v) heap env = (heap, aLookup env v (error ("Undefined name" ++ show v)))

{- Tree machine vs. Graph Machine
 without let/letrec rules 2.1 and 2.2 (top) describe a
 tree-reduction machine, which does not update the root of the
redex
-}

{-
-- exercise 2.10
instantiate (Let NonRec defs body) heap env
 = instantiate body heap env1
   where insts =  map (\rhs -> instantiate rhs heap env)
            	      (rhssOf defs)
   	 env1 =  (zip (lhssOf defs)
	      	      (map snd insts))
		 ++ env
-}

instantiate (Let NonRec defs body) heap env
 = let insts = map (\rhs -> instantiate rhs heap env)
            	   (rhssOf defs)
       env1 =  env ++ (zip (lhssOf defs)
                 	   (map getAddr insts))
       getAddr = snd
     in
    instantiate body heap env1

-- Ex. 2.11
instantiate (Let Rec defs body) heap env
 = instantiate body heap env1
   where insts =  map (\rhs -> instantiate rhs heap env1)
            	      (rhssOf defs)
   	 env1 = env ++ (zip (lhssOf defs)
	      	            (map snd insts))

lrt :: String
lrt = "(main) = (f 3 4) (f x y) = ^(a (pair x b))(b (pair y a))$(fst (snd (snd (snd a)))) (fst p) = (p K) (snd p) = (p K1)"


{-
instantiate (EConstr tag arity) heap env
	    = error "Can't instantiate constructors yet"
-}

showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [iLayn (map showState states),
	    	     	      	       showStats (last states)])

showVal :: [TiState] -> String
showVal states =
 let final = (last states) in
   iDisplay  (iConcat [showState final, showStats final])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats)
  = iConcat [showStack heap stack, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack
  = iConcat [ iStr "Stack [",
   	      iIndent (iInterleave iNewline (map showItem stack)),
	      iStr "]" ]
      where showItem addr = iConcat [showFWAddr addr, iStr": ",
      	    	     	    	     showStkNode heap (hLookup heap addr)]
 
showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fn_addr arg_addr)
  = iConcat [ iStr "NAp ", showFWAddr fn_addr,
    	      iStr " ", showFWAddr arg_addr, iStr " (",
	      showNode (hLookup heap arg_addr), iStr ")"]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", showAddr a1,
	      	       	       	 iStr " ", showAddr a2 ]

showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iAppend (iStr "NNum ") (iNum n)
showNode (NInd a) = iAppend (iStr "NInd ") (iNum a)
showNode (NPrim n _) = iAppend (iStr "NPrim ") (iStr n)

showAddr :: Addr -> Iseq
showAddr addr = iConcat [iStr "#", iStr (show addr)]

showFWAddr :: Addr -> Iseq
showFWAddr = showAddr


{- these must be wierd bc of indentation and vertical layout.
   fuckit

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr ((spcs (4 - length str)) ++ str)
	   	  where
		  str = show addr
-}

spcs n = take n (repeat ' ')

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats)
  = iConcat [iNewline, iNewline, iStr "Total number of steps: ",
    	    iNum (tiStatGetSteps stats)]

-------
-- statistics

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (s, d, h, defs, stats) = (s, d, h, defs, f stats)

