THIS COMPOSITION WILL ONLY SOUND AS INTENDED WITH HASKELL PLATFORM 2014. 
The random threading behaves differently in later versions.

========================================
Piano Composition using Kulitta
Donya Quick

Last modified: 26-January-2014

> module PianoComposition where
> import Euterpea
> import ChordSpaces
> import JazzFG
> import ModeSpace
> import ClassicalFG
> import GrammarMonad
> import GrammarRules
> import PostProc
> import Search
> import Data.List
> import System.Random
> import Play2

============

Overall composition:

> piece = partA :+: partB :+: partA


============

Chord Spaces

This piece needed to be peformable. The right and lefthand spaces will be 
defined separately, but they will share some features in common. For 
example, a constraint that there are no chords spanning more than 12 
halfsteps in the right hand. This is a single-chord constriant.

> okChord :: Predicate AbsChord
> okChord x = maximum x - minimum x <= 12

We also want chords that are sets, which is to say that they are sorted 
vectors containing no duplicate pitches.

> setChord, pianoPred :: Predicate AbsChord
> setChord x = x == sort (nub x)
> pianoPred x = setChord x && okChord x

However, we 
don't want either to move more than an octave for transitions. We'll 
measure that by the center of the chord.

> okMotion :: Predicate (AbsChord, AbsChord)
> okMotion (a,b) = let [a',b'] = sort [avg a, avg b] in b'-a' <= 12.0 where
>     avg xs = fromIntegral (sum xs) / fromIntegral (length xs) 

Now to define all of the chords.

> rhRange = (60,80) :: (AbsPitch, AbsPitch)
> lhRange = (40,59) :: (AbsPitch, AbsPitch)

These are the OPC spaces that will be used for each hand.

> qRH, qLH :: QSpace AbsChord
> qRH = ((filter pianoPred $ makeRange $ takeRep 4 rhRange) // opcEq) ++ 
>       ((filter pianoPred $ makeRange $ takeRep 3 rhRange) // opcEq) ++ 
>       ((filter pianoPred $ makeRange $ takeRep 1 rhRange) // opcEq)
> qLH = ((filter pianoPred $ makeRange $ takeRep 2 lhRange) ++ 
>        (filter pianoPred $ makeRange $ takeRep 1 lhRange)) // opcEq

> takeRep :: Int -> a -> [a]
> takeRep x y = take x $ repeat y

We also need to define the jazz spaces that will be used.

> rhTemps :: [[Int]]
> rhTemps = [[0,2,4], -- simple triad
>            [0,4,6], -- 3-voice seventh (root, fifth, seventh)
>            [1,4,6], -- second fifth, seventh
>            [0,2,4,6], -- basic seventh
>            [0,3,4,6], -- seventh, no third
>            [0,1,2,4], -- root, 2nd third, fifth
>            [1,2,4,6], -- dropped root, second, seventh chord
>            [0], -- just the root
>            [2], -- just the third
>            [4]] -- just the fifth

> lhTemps :: [[Int]]
> lhTemps = [[0,0], -- doubled root
>            [0,4], -- root and fifth
>            [0,6], -- root and seventh
>            [0]] -- just root

> qRHJ, qLHJ :: QSpace JChord
> qRHJ = modeSpace' rhTemps
> qLHJ = modeSpace' lhTemps



============

Part A

We start with user-specified constants. These include the starting Term
to use for generation (AA form in this case), the number of generative 
iterations to use, and random seeds for the grammar, chord spaces, and 
foreground.

> tSeedA = Let "x" (i 4) $ S [Var "x", Var "x"] :: Term CType
> minDurA = hn -- minimum duration for chords
> itersA = 6 -- number of generative iterations
> letsA = False -- whether to use internal let statements
> rSeedA = 832593 -- for abstract structure
> jSeedA1 = 9328 -- for RH jazz chords
> jSeedA2 = 93238 -- for LH jazz chords
> cSeedA1 = 2539 -- for RH classical cs
> cSeedA2 = 2509 -- for LH classical cs
> fgSeedA1 = 1942 -- for RH foreground
> fgSeedA2 = 732 -- for LH foreground
> tKeyA = 0 -- C
> tCtxtA = Minor

1. Generate the abstract structure

> tGenA :: Term CType -- this is the abstract structure
> tGenA = gen (ruleSet minDurA letsA) itersA rSeedA tCtxtA tSeedA

> tChordsA = ctTrans tKeyA $
>     toChords (expand [] tGenA) tCtxtA
> constsA = findInds [] tGenA

2. Use a jazz chord space

> jtsA = map toJTriple tChordsA
> msA = map (\(a,b,c) -> ([],c)) jtsA -- get just modes as JChords

> rhJA, lhJA :: [JChord]
> rhJA = greedyLet (const True) nearFallJ constsA
>        (map (eqClass qRHJ modeEq) msA) (mkStdGen jSeedA1)
> lhJA = greedyLet (const True) nearFallJ constsA
>        (map (eqClass qLHJ modeEq) msA) (mkStdGen jSeedA2)


3. Use a classical chord space

> rhCA, lhCA :: [AbsChord]
> rhCA = fixVs $ greedyLet (okMotion) nearFall constsA 
>        (map (eqClass qRH opcEq) $ map fst rhJA) (mkStdGen cSeedA1)
> lhCA = greedyLet (okMotion) nearFall constsA 
>        (map (eqClass qLH opcEq) $ map fst lhJA) (mkStdGen cSeedA2)

4. Add a foreground

The piano right hand will only have ONE voice worth of foreground.
The left hand will have a custom foreground.

> rhV1A = toVoices $ zipWith newP tChordsA $ map (\x -> [last x]) rhCA
> rhRestA = toVoices $ zipWith newP tChordsA $ map f rhCA where
>     f x = take (length x - 1) x
> rhV1FGA = snd $ addFG defConsts (mkStdGen fgSeedA1) rhV1A

> rhFGA = vsToMusic rhV1FGA :=: vsToMusic rhRestA
> lhFGA = lhFGAlg (mkStdGen fgSeedA2) $
>     zipWith newP tChordsA lhCA

> partA = instrument BrightAcousticPiano lhFGA :=:
>         instrument AcousticGrandPiano rhFGA

============

Part B

Part B needs the same sorts of constants that part A used, but they 
need to be different to ensure that the sections are contrasting. 
After that, much of the code is essentially the same as for part A.

> tSeedB :: Term CType
> tSeedB = S [i 4, i 4, Mod M5 (i 4), Mod M6 (i 4)]
> minDurB = hn
> itersB = 10
> letsB = False
> rSeedB = 18930 -- for abstract structure
> jSeedB1 = 19328 -- for RH jazz chords
> jSeedB2 = 193238 -- for LH jazz chords
> cSeedB1 = 12539 -- for RH classical cs
> cSeedB2 = 12509 -- for LH classical cs
> fgSeedB1 = 11942 -- for RH foreground
> fgSeedB2 = 1732 -- for LH foreground
> tKeyB = 8 -- A-flat
> tCtxtB = Major

1. Generate the abstract structure

> tGenB :: Term CType -- this is the abstract structure
> tGenB = gen (ruleSet minDurB letsB) itersB rSeedB tCtxtB tSeedB

> tChordsB = ctTrans tKeyB $
>     toChords (expand [] tGenB) tCtxtB
> constsB = findInds [] tGenB

2. Use a jazz chord space

> jtsB = map toJTriple tChordsB
> msB = map (\(a,b,c) -> ([],c)) jtsB -- get just modes as JChords

> rhJB, lhJB :: [JChord]
> rhJB = greedyLet (const True) nearFallJ constsB
>        (map (eqClass qRHJ modeEq) msB) (mkStdGen jSeedB1)
> lhJB = greedyLet (const True) nearFallJ constsB
>        (map (eqClass qLHJ modeEq) msB) (mkStdGen jSeedB2)


3. Use a classical chord space

> rhCB, lhCB :: [AbsChord]
> rhCB = fixVs $ greedyLet (okMotion) nearFall constsB 
>        (map (eqClass qRH opcEq) $ map fst rhJB) (mkStdGen cSeedB1)
> lhCB = greedyLet (okMotion) nearFall constsB 
>        (map (eqClass qLH opcEq) $ map fst lhJB) (mkStdGen cSeedB2)

4. Add a foreground

The piano right hand will only have ONE voice worth of foreground.
The left hand will have a custom foreground.

> rhV1B = toVoices $ zipWith newP tChordsB $ map (\x -> [last x]) rhCB
> rhRestB = toVoices $ zipWith newP tChordsB $ map f rhCB where
>     f x = take (length x - 1) x
> rhV1FGB = snd $ addFG defConsts (mkStdGen fgSeedB1) rhV1B

> rhFGB = vsToMusic rhV1FGB :=: vsToMusic rhRestB
> lhFGB = lhFGAlg (mkStdGen fgSeedB2) $
>     zipWith newP tChordsB lhCB

> partB = instrument BrightAcousticPiano lhFGB :=:
>         instrument AcousticGrandPiano rhFGB


================

Both parts make use of a custom-defined foreground algorithm for the
left hand of the piano and a function for handling rests in voices. 
Rests are represented as negative pitches.

> lhFGAlg :: StdGen -> [TChord] -> Music Pitch
> lhFGAlg g [] = rest 0
> lhFGAlg g ((k,d,[x]):xs) = note d (pitch x) :+: lhFGAlg g xs
> lhFGAlg g ((k,d,x@[x1,x2]):xs) = 
>     let (p,g') = randomR (0,1::Double) g
>         xs' = lhFGAlg g' xs
>         f1 (x1:x2:_) = note d (pitch x1) :=: note d (pitch x2)
>         f2 (x1:x2:_) = note (d/2) (pitch x1) :+: note (d/2) (pitch x2)
>     in  if p<0.5 || x2-x1 > 12 || x2-x1 < 7 then f2 x :+: xs' else f1 x :+: xs'
> lhFGAlg g _ = error "lhFGAlg: bad input format"


> fixVs xs = 
>     let n = maximum $ map length xs
>         f i [] = if i==0 then [] else (-1) : f (i-1) []
>         f i (a:b) = a : f (i-1) b
>     in  map (reverse . f n) $ map reverse xs 

