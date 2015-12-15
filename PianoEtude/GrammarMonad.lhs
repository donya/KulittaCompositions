Harmony Grammar Monad Implementations 
Donya Quick and Paul Hudak
Last modified: 16-December-2014

Changes since last version:
- Dummy Applicative instance added to ensure that legacy code will
  still compile with ghc 7.10

> module GrammarMonad where
> import Euterpea
> import Data.List
> import Control.Monad.State
> import System.Random
> import Control.Applicative

The following constant determines the behavior of the choose function.
If set to True, then ID rules MUST be present in the rule set to succeed.
If set to False, then if no relevant rules are found for a symbol, the
symbol will be left unchanged (rather than throwing an error).

> forceIDs = False 

Construction of the grammar's symbols and structure

A CType is the Roman numeral.

> data CType = I | II | III | IV | V | VI | VII
>      deriving (Eq, Show, Ord, Enum, Read)

An MType is a degree of modulation (relative 2nd, 3rd, ..., 7th). 
There is no M1 since that would indicate remaining in the home key.

> data MType = M2 | M3 | M4 | M5 | M6 | M7
>      deriving (Eq, Show, Ord, Enum)

A Chord is a duration and CType.

> data Chord a = Chord Dur a
>      deriving (Eq, Show)

The Term data structure has five constructors: a chord, which acts as
a nonterminal but can also be a terminal; a "sentence," or sequence of
Terms; a modulation applied to a Term; a variable; and a Let statement
that uses variables that are represented as strings.

> data Term a = NT (Chord a) | S [Term a] | Mod MType (Term a) |  
>             Let String (Term a) (Term a) | Var String
>      deriving (Eq, Show)

> data Rule a = (a, Prob) :-> RuleFun a

> type RuleFun a = Ctxt -> Dur -> Term a
> type Prob = Double

These are used to simplify writing rules.

> i, ii, iii, iv, v, vi, vii :: Dur -> Term CType
> [i, ii, iii, iv, v, vi, vii] = map (\c t -> NT (Chord t c)) $ enumFrom I 
> c ct t = NT (Chord t ct)
> rf fs t = map ($t) fs

======== CONTEXT ========

The modal context can be stored using Euterpea's Mode data type.

> type Ctxt = Mode 

> type CtxtFun = Ctxt -> MType -> Ctxt 

> defCtxtFun :: CtxtFun
> defCtxtFun Major m = 
>     [Minor, Minor, Major, Major, Minor, Minor] !! fromEnum m 
> defCtxtFun Minor m = 
>     [Minor, Major, Minor, Minor, Major, Major] !! fromEnum m


=========== MONAD IMPLEMENTATION ============

> newtype Prog a = Prog ((StdGen, Ctxt) -> ((StdGen, Ctxt), a))

Define Prog as an instance of Monad:

> instance Monad Prog where
>     return a       = Prog (\s -> (s,a))
>     Prog p0 >>= f1 = Prog $ \s0 ->
>         let (s1, a1) = p0 s0
>             Prog p1 = f1 a1
>         in  p1 s1

One domain-specific primitive:

> getRand :: (Random a) => (a, a) -> Prog a
> getRand ran = Prog (\(g,c) -> let (r,g') = randomR ran g
>               in ((g',c),r))

> getCtxt :: Prog Ctxt
> getCtxt = Prog (\(g,c) -> ((g,c),c))

> setCtxt :: Ctxt -> Prog ()
> setCtxt c' = Prog (\(g,c) -> ((g,c'), ())) 

To "run" the Prog monad:

> runP :: Prog a -> StdGen -> Ctxt -> a
> runP (Prog f) g c = snd (f (g,c))

-------------------------------------------------------------------------

A function to rewrite one Term a to another:

> update :: (Eq a) => [Rule a] -> Term a -> Prog (Term a)
> update rules t = case t of
>     NT x      -> applyRule rules x
>     S s       -> do ss <- sequence (map (update rules) s)
>                     return (S ss)
>     Mod m s   -> do c <- getCtxt
>                     setCtxt (defCtxtFun c m) -- into mod
>                     s' <- update rules s
>                     setCtxt c -- out of mod
>                     return (Mod m s')
>     Var x     -> return (Var x)
>     Let x a t -> do a' <- update rules a
>                     t' <- update rules t
>                     return (Let x a' t')

> applyRule :: (Eq a) => [Rule a] -> Chord a -> Prog (Term a)
> applyRule rules t@(Chord d c) =  
>   let  rs = filter (\((c',p) :-> rf) -> c'==c) rules
>   in   do r <- getRand (0.0, 1.0)
>           x <- getCtxt -- NEED TO DO SOMETHING WITH THIS?
>           return ((choose' t rs r) x d)

> choose' :: Chord a -> [Rule a] -> Prob -> (RuleFun a)
> choose' t rs p = 
>     if null rs && not forceIDs then \c d -> NT $ t -- auto ID rule
>     else choose rs p

> choose :: [Rule a] -> Prob -> (RuleFun a)
> choose [] p  = error "Nothing to choose from!"
> choose (((c,p') :-> rf):rs) p  = 
>     if p<=p' || null rs then rf else choose rs (p-p') 

Iterate a monadic function a given number of times:

> iter :: Monad m => (a -> m a) -> a -> m [a]
> iter f a = do a' <- f a
>               as <- iter f a' 
>               return (a':as)

-----------------------------------------------------------------------

Putting it all together:

> gen :: (Eq a) => [Rule a] -> Int -> Seed -> Ctxt -> Term a -> Term a
> gen rules i s c t = runP (iter (update rules) t) (mkStdGen s) c !! i

> gen' :: (Eq a) => [Rule a] -> Int -> StdGen -> Ctxt -> Term a -> Term a
> gen' rules i s c t = runP (iter (update rules) t) s c !! i

> gen'' :: (Eq a) => [Rule a] -> StdGen -> Ctxt -> Term a -> [Term a] -- gives all results
> gen'' rules s c t = runP (iter (update rules) t) s c

> type Seed = Int

The expand function eliminates Lets and Vars from a generated Term a.
It allows for nested Let expressions for variables with the same name
with lexical scoping. For example:

expand [] (Let "x" t1 (Let "x" t2 (Var "x"))) ==> t2

> expand :: [(String, Term a)] -> Term a -> Term a 
> expand e t = case t of 
>     Let x a exp -> expand ((x, expand e a):e) exp
>     Var x       -> maybe (error (x ++ " is undefined")) id $ lookup x e
>     S s         -> S $ map (expand e) s
>     Mod m t'    -> Mod m $ expand e t'
>     x           -> x

========================

ADDITIONAL MANIPULATIONS

Map defined over Term

> tMap :: (a -> b) -> Term a -> Term b
> tMap f t = case t of
>     Let x a exp -> Let x (tMap f a) (tMap f exp)
>     Var x -> Var x
>     S s -> S $ map (tMap f) s
>     Mod m t' -> Mod m $ tMap f t'
>     NT (Chord d a) -> NT (Chord d $ f a)

Minimizing nested structure while staying as a Term value

> flatten :: (Eq a) => Term a -> Term a
> flatten xs = let xs' = flattenRec xs in 
>     if xs' == xs then xs else flatten xs' where
>     flattenRec t = case t of
>         Let x a exp -> Let x (flattenRec a) (flattenRec exp)
>         Var x -> Var x
>         Mod m t -> Mod m (flattenRec t)
>         S [] -> S []
>         S xs -> S $ concatMap stripS $ map flattenRec xs 
>         NT v -> NT v where
>     stripS :: Term a -> [Term a]
>     stripS (S xs) = concatMap stripS xs
>     stripS xs = [xs]

Flattening completely to a list

> toPairs :: Term a -> [(a, Dur)]
> toPairs exp@(Let x a e) = toPairs $ expand [] exp
> toPairs (Var x) = error ("Undefined variable: "++ x)
> toPairs (S xs) = concatMap toPairs xs
> toPairs (Mod m t) = error ("Modulations not supported! Found: "++ show m)
> toPairs (NT (Chord d ct)) = [(ct,d)]


========================

DISPLAY FUNCTIONS

> cShow = latexShow -- preferred format for showing results

> latexShow t ct = show ct ++ "^{" ++ showDur t ++ "}"
> printShow t ct = '(' : show ct ++ " " ++ showDur t ++ ")"

> showTerm :: Term CType -> String
> showTerm t = case t of 
>     Let x a exp -> "Let "++x++"="++show a++" in "++showTerm exp
>     Mod m t -> show m ++ "( "++ showTerm t ++" )"
>     S s -> (concat $ intersperse " " $ map showTerm s)
>     Var v -> v
>     NT (Chord t ct) -> cShow t ct

> showDur :: Dur -> String
> showDur d = if d == sn then "s" else
>             if d == en then "e" else
>             if d == qn then "q" else
>             if d == hn then "h" else
>             if d == wn then "w" else show d

============================

Applicative instance
This is only included because of the warning in ghc 7.8.3 that 
says instances of Monads that are not also instances of Applicative
will become an error in ghc 7.10 under the Applicative-Monad Proposal.
However, the implementation of the Prog monad predates this by several
years, so the code below only exists to ensure that legacy Kulitta 
code will still compile (which uses neither fmap, pure, nor <*>).

> instance Functor Prog where
>  fmap f (Prog p) = error "(GrammarMonad) No definition for fmap."

> instance Applicative Prog where
>  pure = error "(GrammarMonad) No definition for pure."
>  a <*> b = error "(GrammarMonad) No definition for <*>."
