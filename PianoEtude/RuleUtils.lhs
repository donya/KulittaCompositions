Rule utilities for use with GrammarRules.lhs (and other rule modules)
Donya Quick and Paul Hudak
Last modified: 12-June-2013
For paper: Grammar-Based Automated Music Composition in Haskell

This module adds some utility functions for use in defining rule
sets. This includes "shorthand" functions for writing single chord
Term values.

> module RuleUtils where
> import Euterpea
> import GrammarMonad


Rule probabilities need to sum to 1.0 for rules with the same lefthand 
side. Since there is an option to include or exclude the let-in rules, 
the probabilities should be normalized (forced to sum to 1.0) before 
the rules are used.

> lhs ((c,p) :-> rf) = c
> prob ((c,p) :-> rf) = p

> normalize :: (Eq a) => [Rule a] -> [Rule a]
> normalize [] = []
> normalize (r@((l,p) :-> rf):rs) = 
>     let rset = r : filter ((l==).lhs) rs
>         rset' = filter ((l/=).lhs) rs
>         psum = sum $ map prob rset
>     in  map (\((l',p') :-> c') -> ((l',p'/psum) :-> c')) rset ++ normalize rset'

Rules can be "wrapped" to produce rules that only operate on chords
with at least a certain duration. 

> toRelDur :: Dur -> Rule a -> Rule a
> toRelDur d ((c,p) :-> f) = 
>     let dmin ctxt t = minDur $ expand [] $ f ctxt t
>     in  ((c, p) :-> \ctxt t -> if dmin ctxt t < d then NT $ Chord t c else f ctxt t)

> minDur :: Term a -> Dur
> minDur (S s) = minimum $ map minDur s
> minDur (Mod m t) = minDur t
> minDur (NT (Chord d x)) = d
> minDur _ = error "(minDur) String is not fully interpreted."


> relRuleSet d rs = map (toRelDur d) $ rs

==== PRINTING FUNCTIONS ====

> showRule :: (Show a) => Rule a -> String
> showRule ((l,p) :-> rf) = show p ++ "\t" ++ show l ++ " -> " ++ show (rf Major 1.0) ++ "\n"


> writeRules :: (Show a) => [Rule a] -> IO ()
> writeRules ruleSet = 
>     let rstr = concatMap showRule ruleSet 
>     in  writeFile "rules.txt" rstr