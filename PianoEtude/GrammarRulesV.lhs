Rules for use with GrammarMonad.lhs
Donya Quick and Paul Hudak
Last modified: 25-Jan-2014
Based on grammar from paper: Grammar-Based Automated Music Composition in Haskell

THESE RULES ARE INTENDED TO END ON A FIVE CHORD (V)

This module defines a collection of rules for generating
sequences of Roman numerals. It is based on examples found in
"Analyzing Fugue: A Schenkerian Approach" by William RenWick
as well as the authors' judgement for assigning probabilities 
and rules involving let-in expressions and modulations.

RULE SET MODIFIED FOR USE IN PSYCH EXPERIMENT

> module GrammarRulesV where
> import Euterpea hiding (c)
> import GrammarMonad
> import RuleUtils


=== RULES WITH LET ===



> ruleL1, ruleL2 :: CType -> RuleFun CType
> ruleL1 ct ctxt t = Let "x" (c ct (t/2)) (S [Var "x", Var "x"])
> ruleL2 ct ctxt t = Let "x" (c ct (t/4)) (S [Var "x", v (t/2), Var "x"]) 

> rulesL :: [Rule CType]
> rulesL = concatMap (\ct -> [(ct, 0.1) :-> \t -> ruleL1 ct t, 
>                             (ct, 0.1) :-> \t -> ruleL2 ct t]) (enumFrom I)


=== RULES FOR I ===

> ruleI1 = (I, 0.15) :-> \ctxt t -> 
>     if ctxt == Major then S [i (t/2), v (t/2)] -- major
>     else S [i (t/2), v (t/2)] -- minor
> ruleI2 = (I, 0.15) :-> \ctxt t -> S [i (t/2), iv (t/4), v (t/4)]
> ruleI3 = (I, 0.15) :-> \ctxt t -> S [i (t/2), v (t/2)]
> ruleI4 = (I, 0.15) :-> \ctxt t -> 
>     if ctxt == Major then S [i (t/2), ii (t/4), v (t/4)] -- major
>     else S [i (t/2), iv (t/4), v (t/4)] -- minor
> ruleI5 = (I, 0.2) :-> \ctxt t -> if t <= hn then i t else S [i (t/2), v (t/2)] 

> rulesI = [ruleI1, ruleI2, ruleI3, ruleI4, ruleI5]


=== RULES FOR II ===

> ruleII1 = (II, 0.4) :-> \ctxt -> 
>     if ctxt == Major then ii -- major 
>     else iv -- minor
> ruleII1b = (II, 0.4) :-> \ctxt t -> 
>     if ctxt == Major then if t > qn then ii t else Mod M2 $ i t -- major
>     else Mod M2 $ i t -- minor
> ruleII2 = (II, 0.2) :-> \ctxt t -> 
>     if ctxt == Major then S[vi (t/2), ii (t/2)] -- major
>     else S[vi (t/2), iv (t/2)] -- minor

> rulesII = [ruleII1, ruleII1b, ruleII2] 

=== RULES FOR III ===

> ruleIII1 = (III, 0.9) :-> \ctxt -> iii
> ruleIII2 = (III, 0.1) :-> \ctxt -> (Mod M3 . i)

> rulesIII = [ruleIII1, ruleIII2]


=== RULES FOR IV ===

> ruleIV1 = (IV, 0.9) :-> \ctxt -> iv
> ruleIV2 = (IV, 0.1) :-> \ctxt -> (Mod M4 . i)

> rulesIV = [ruleIV1, ruleIV2]


=== RULES FOR V ===

> ruleV1 = (V, 0.15) :-> \ctxt t -> S [iv (t/2), v (t/2)]
> ruleV2 = (V, 0.10) :-> \ctxt t -> S [vi (t/2), v(t/2)]
> ruleV3 = (V, 0.10) :-> \ctxt t -> S [iii (t/4), vi (t/4), v (t/2)]
> ruleV4 = (V, 0.10) :-> \ctxt t -> S [vi (t/4), vii (t/4), v (t/2)]
> ruleV5 = (V, 0.10) :-> \ctxt t -> S [iii (t/2), v (t/2)]

> ruleV6 = (V, 0.10) :-> \ctxt -> iii
> ruleV7 = (V, 0.10) :-> \ctxt t -> S [v (t/2), v (t/2)]  
> ruleV8 = (V, 0.05) :-> \ctxt t -> S [vii (t/2), v (t/2)] 
> ruleV9 = (V, 0.10) :-> \ctxt -> v
> ruleV10 = (V, 0.10) :-> \ctxt -> (Mod M5 . i) 

> rulesV = [ruleV1, ruleV2, ruleV3, ruleV4, ruleV5, 
>          ruleV6, ruleV7, ruleV8, ruleV9, ruleV10]


=== RULES FOR VI ===

> ruleVI1 = (VI, 0.7) :-> \ctxt -> vi
> ruleVI2 = (VI, 0.3) :-> \ctxt -> (Mod M6 . i)

> rulesVI = [ruleVI1, ruleVI2]


=== RULES FOR VII ===

> ruleVII1 = (VII, 0.5) :-> \ctxt t -> if t > qn then vii t else Mod M7 $ i t
> ruleVII2 = (VII, 0.5) :-> \ctxt t -> S [i (t/2), iii (t/2)]

> rulesVII = [ruleVII1, ruleVII2]


=== RULE SET ===

The rule set defined below is broken up into two steps to allow easier testing of the probability sums.

> ruleSet' useLets = normalize $ concat 
>    [rulesI, rulesII, rulesIII, rulesIV, rulesV, rulesVI, rulesVII, 
>     if useLets then rulesL else []]

> ruleSetV d useLets = map (toRelDur d) $ ruleSet' useLets 

