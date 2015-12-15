> module Main where
> import PTGG
> import MusicGrammars
> import System.Random
> import EuterpeaSpecial
> import ChordSpaces
> import Search
> import Constraints
> import ModeSpace
> import JazzFG
> import PostProc
> import Euterpea.ExperimentalPlay
> import PlayDev

A rule set specifically intended for Dorian.
It avoides VI, which is diminished. 

> r1a = (I, 0.2) :-> \p -> [NT (V, h p), NT (I, h p)]
> r1b = (I, 0.2) :-> \p -> [NT (IV, h p), NT (I, h p)]
> r1c = (I, 0.2) :-> \p -> [NT (VII, h p), NT (I, h p)]
> r1d = (I, 0.2) :-> \p -> [NT (IV, q p), NT (V, q p), NT (I, h p)]
> r1e = (I, 0.2) :-> \p -> if dur p > hn then [NT (III, h p), NT (I, h p)] 
>                          else [NT (I, p)]

> r2a = (II, 0.1) :-> \p -> [NT (II, h p), NT (VII, h p)]
> r2b = (II, 0.2) :-> \p -> [NT (II, h p), NT (V, h p)]
> r2c = (II, 0.1) :-> \p -> [NT (II, h p), NT (III, h p)]
> r2d = (II, 0.5) :-> \p -> [NT (II, h p), NT (I, h p)]
> r2e = (II, 0.1) :-> \p -> [NT (II, p)]

> r3a = (III, 0.2) :-> \p -> [NT (III, h p), NT (II, h p)]
> r3b = (III, 0.2) :-> \p -> [NT (III, h p), NT (V, h p)]
> r3c = (III, 0.4) :-> \p -> [NT (III, h p), NT (I, h p)]
> r3d = (III, 0.2) :-> \p -> [NT (III, p)]

> r4a = (IV, 0.2) :-> \p -> [NT (IV, h p), NT (V, h p)]
> r4b = (IV, 0.2) :-> \p -> [NT (II, h p), NT (IV, h p)]
> r4c = (IV, 0.4) :-> \p -> [NT (IV, h p), NT (I, h p)]
> r4d = (IV, 0.2) :-> \p -> [NT (IV, p)]

> r5a = (V, 0.2) :-> \p -> [NT (V, h p), NT (VII, h p)]
> r5b = (V, 0.2) :-> \p -> [NT (V, h p), NT (IV, h p)]
> r5c = (V, 0.4) :-> \p -> [NT (V, h p), NT (I, h p)]
> r5d = (V, 0.2) :-> \p -> [NT (V, p)]

> r7a = (VII, 0.4) :-> \p -> [NT (VII, h p), NT (I, h p)]
> r7b = (VII, 0.4) :-> \p -> [NT (VII, h p), NT (V, h p)]
> r7c = (VII, 0.2) :-> \p -> [NT (VII, p)]

> rulesRaw = [
>     r1a, r1b, r1c, r1d, r1e,
>     r2a, r2b, r2c, r2d, r2e,
>     r3a, r3b, r3c, r3d,
>     r4a, r4b, r4c, r4d,
>     r5a, r5b, r5c, r5d,
>     r7a, r7b, r7c]

> rules = map (toRelDur2 (<qn)) rulesRaw


> simplifyRules = [
>     (I,   1.0) :-> \p -> [NT (I, p)],
>     (II,  1.0) :-> \p -> [NT (IV, p)],
>     (III, 1.0) :-> \p -> [NT (I, p)],
>     (IV,  1.0) :-> \p -> [NT (IV, p)],
>     (V,   1.0) :-> \p -> [NT (V, p)],
>     (VI,  1.0) :-> \p -> [NT (I, p)],
>     (VII, 1.0) :-> \p -> [NT (V, p)]]


============================

> changeMode mNew = tMap (\(x,p) -> (x, p{mode=mNew}))

> interpMode :: Sentence CType MP -> Sentence (Int, CType) MP
> interpMode = tMap f where
>     f (rNum, p) = 
>         let scale = getScale $ mode p
>         in  ((scale !! fromEnum rNum, rNum), p)

> interpMode2 :: Sentence CType MP -> Sentence ([Int], CType) MP
> interpMode2 = tMap f where
>     f (rNum, p) = 
>         let scale0 = getScale $ mode p
>             scale = scale0 ++ map (+12) scale0
>             root = scale !! fromEnum rNum
>             third = scale !! ((fromEnum rNum)+2)
>         in  (([root, third], rNum), p)

> interpMode3 :: Sentence CType MP -> Sentence ([Int], CType) MP
> interpMode3 = tMap f where
>     f (rNum, p) = 
>         let scale0 = getScale $ mode p
>             scale = scale0 ++ map (+12) scale0
>             root = scale !! fromEnum rNum
>             third = scale !! ((fromEnum rNum)+2)
>             fifth = scale !! ((fromEnum rNum)+4)
>         in  (([root, third, fifth], rNum), p)

> justPDs :: Sentence (a, CType) MP -> [(a, Dur)]
> justPDs = map (\x -> (fst $ fst x, dur $ snd x)) . toPairs

> toMusic :: [(Int, Dur)] -> Music Pitch
> toMusic = line . map (\(p,d) -> note d $ pitch p)

> toMusicN :: [([Int], Dur)] -> Music Pitch
> toMusicN = line . concat . map f where
>    f (ps, d) = 
>        let d' = d / fromIntegral (length ps)
>            g p = note d' $ pitch p
>        in  map g ps

> toMusicV :: [(Int, Dur, Volume)] -> Music (Pitch, Volume)
> toMusicV = line . map (\(p,d,v) -> note d (pitch p, v))

> toMusicVN :: [([Int], Dur, Volume)] -> Music (Pitch, Volume)
> toMusicVN = line . concat . map f where
>    f (ps, d, v) = 
>        let d' = d / fromIntegral (length ps)
>            g p = note d' (pitch p, v)
>        in  map g ps

=============================

Chord spaces for melodies

> mySpace1 = (map (\x -> [x]) [60..84]) // oEq

> mySpace2 = 
>     let cs = [[a,b] | a<-[60..84], b<-[60..84]]
>     in  cs // opEq

> mySpace3 = 
>     let cs = [[a,b,c] | a<-[60..84], b<-[60..84], c<-[60..84]]
>     in  cs // opEq

> splitDurs (notes, d) = 
>     let d' = d / fromIntegral (length notes)
>     in  map (\n -> (n, d')) notes

> myClass :: Predicate ([Int],[Int])
> myClass ([x], [y]) = abs(x-y) <= 5
> myClass (xs, ys) = maxDist xs ys <= 5.0

> makeMelPath1 pds g = 
>     let ps = map (\x -> [fst x]) pds
>         es = map (eqClass mySpace1 oEq) ps
>         ps' = greedyLet myClass nearFall [] es g
>         ps'' = map (\[x] -> x) ps'
>     in  zip ps'' $ map snd pds

> makeMelPathN space pds g = 
>     let ps = map fst pds
>         es = map (eqClass space opEq) ps
>         ps' = greedyLet myClass nearFall [] es g
>     in  zip ps' $ map snd pds

=============================

Creating an initial melody

> startVal = [NT (I, MP 4 Dorian 0)]

> g0 = mkStdGen 124

> (_, mel0) = gen rules (g0, startVal) !! 3

Now some variations

> g0a = mkStdGen 5
> g0b = mkStdGen 10
> g0c = mkStdGen 234
> g0d = mkStdGen 26
> (_, mel0a) = gen rules (g0a, mel0) !! 2
> (_, mel0b) = gen rules (g0b, mel0) !! 2
> (_, mel0c) = gen rules (g0c, mel0) !! 3
> (_, mel0d) = gen rules (g0d, mel0) !! 3

Simplified versions for use in a bassline (basically reduces to TSD)

> (_, mel0aSimp) = gen simplifyRules ((mkStdGen 0), mel0a) !! 2
> (_, mel0bSimp) = gen simplifyRules ((mkStdGen 0), mel0b) !! 2
> (_, mel0cSimp) = gen simplifyRules ((mkStdGen 0), mel0c) !! 2
> (_, mel0dSimp) = gen simplifyRules ((mkStdGen 0), mel0d) !! 2

First, melodies without chord space modification

> mel0Ps = justPDs $ interpMode mel0
> mel0aPs = justPDs $ interpMode mel0a
> mel0bPs = justPDs $ interpMode mel0b
> mel0cPs = justPDs $ interpMode mel0c
> mel0dPs = justPDs $ interpMode mel0d

> mel0M = transpose 60 $ toMusic mel0Ps
> mel0aM = transpose 60 $ toMusic mel0aPs
> mel0bM = transpose 60 $ toMusic mel0bPs
> mel0cM = transpose 60 $ toMusic mel0cPs
> mel0dM = transpose 60 $ toMusic mel0dPs

Now, melodies with single note chord space modification

> toMusicX1 x i doTies = 
>     let f = if doTies then ties else id
>         h = tMap (\((a,b),c) -> (([a],b),c))
>     in  toMusic $ f $ makeMelPath1 (concat $ map splitDurs $ justPDs $ h $ interpMode x) 
>         (mkStdGen i)

> toMusicXV1 x i doTies vs vr = 
>     let f = if doTies then ties else id
>         h = tMap (\((a,b),c) -> (([a],b),c))
>     in  toMusicV $ randomDyn (mkStdGen vs) vr $ f $ 
>         makeMelPath1 (concat $ map splitDurs $ justPDs $ h $ interpMode x) 
>         (mkStdGen i)


> mel0MP1 = toMusicXV1 mel0 3 True 120 (60,100)
> mel0aMP1 = toMusicXV1 mel0a 5 True 1210 (60,100)
> mel0bMP1 = toMusicXV1 mel0b 6 True 122 (60,100)
> mel0cMP1 = toMusicXV1 mel0c 7 True 123 (60,100)
> mel0dMP1 = toMusicXV1 mel0d 7 True 124 (60,100) -- <-- THIS IS A NICE ONE


> mel0aSimpMP1 = toMusicXV1 mel0aSimp 520 False 12101 (60,100)
> mel0bSimpMP1 = toMusicXV1 mel0bSimp 521 False 12102 (60,100)
> mel0cSimpMP1 = toMusicXV1 mel0cSimp 522 False 12103 (60,100)
> mel0dSimpMP1 = toMusicXV1 mel0dSimp 523 False 12104 (60,100)


Two-note versions

> toMusicX2 x i doTies = 
>     let f = if doTies then ties else id
>     in  toMusic $ f $ makeMelPath1 (concat $ map splitDurs $ justPDs $ interpMode2 x) (mkStdGen i)

> toMusicXV2 x i doTies vs vr = 
>     let f = if doTies then ties else id
>     in  toMusicV $ randomDyn (mkStdGen vs) vr $ f $ makeMelPath1 (concat $ map splitDurs $ justPDs $ interpMode2 x) (mkStdGen i)

> mel0MP2 = toMusicXV2 mel0 60 True 100 (60,100)
> mel0aMP2 = toMusicXV2 mel0a 61 True 101 (60,100)
> mel0bMP2 = toMusicXV2 mel0b 62 True 102 (60,100)
> mel0cMP2 = toMusicXV2 mel0c 63 True 103 (60,100)
> mel0dMP2 = toMusicXV2 mel0d 65 True 104 (60,100)

Three-note versions

> toMusicX3 x i doTies = 
>     let f = if doTies then ties else id
>     in  toMusic $ f $ makeMelPath1 (concat $ map splitDurs $ justPDs $ interpMode3 x) (mkStdGen i)

> toMusicXV3 x i doTies vs vr = 
>     let f = if doTies then ties else id
>     in  toMusicV $ randomDyn (mkStdGen vs) vr $ f $ makeMelPath1 (concat $ map splitDurs $ justPDs $ interpMode3 x) (mkStdGen i)

> mel0MP3 = toMusicXV3 mel0 70 True 80 (60,100)
> mel0aMP3 = toMusicXV3 mel0a 71 True 81 (60,100)
> mel0bMP3 = toMusicXV3 mel0b 72 True 82 (60,100)
> mel0cMP3 = toMusicXV3 mel0c 73 True 83 (60,100)
> mel0dMP3 = toMusicXV3 mel0d 74 True 84 (60,100)

> ties :: (Eq a) => [(a, Dur)] -> [(a, Dur)]
> ties ((a,b):(c,d):xs) = if a==c then ties ((a,b+d):xs) else (a,b) : ties ((c,d):xs)
> ties x = x


=======================================================

Dynamics algorithm


> randomDyn :: StdGen -> (Volume, Volume) ->[(a, Dur)] -> [(a, Dur, Volume)]
> randomDyn g range xs = 
>     let vs = randomRs range g
>     in  zipWith (\(a,b) c -> (a,b,c)) xs vs

> scaleVolume :: Rational -> Music (Pitch,Volume) -> Music (Pitch,Volume)
> scaleVolume r = mMap (\(p,v) -> (p, round(r * fromIntegral v)))


========================================================

Making some chords

> toTriples :: Sentence CType MP -> [(Key, Dur, CType)]
> toTriples t = 
>     let ps = toPairs t
>         f (c,mp) = ((key mp, mode mp), dur mp, c)
>     in  map f ps


This will make some jazzy chords in modes. This is similar to
the jazz chords foreground algorithm in JazzFG.lhs.

> --         r t f 7    r t f    r=root, r=third, f=fifth
> cTemps = [[0,2,4,6], [0,2,4]] 

> --          bass       chords
> cRans = (34,50) : take 4 (repeat (50,72))

> upperRange = makeRange' (take 4 (repeat (50,72)))
> upperSpace = upperRange // opcEq
> bassSpace = map (\x -> [x]) [34..50] // opcEq

> myModeSpace = modeSpace' cTemps -- subset of ModeSpace desired
> myModeRange = makeRange' cRans // opcEq  -- subset of OPC-space desired

> ties2 ((k,d,a):(k2,d2,a2):xs) = 
>     if a==a2 then ties2 ((k,d+d2,a):xs) else (k,d,a) : ties2 ((k2,d2,a2):xs)
> ties2 x = x

> spaceClass1 n x = okDists $ normP x where
>     okDists (x1:x2:xs) = if x2-x1 < n then False else okDists (x2:xs)
>     okDists _ = True

> spaceClass2 n (x,y) = minDist 5 x y

> minDist n x y = (sum $ map abs $ zipWith subtract x y) > n

> --makeJazzy :: StdGen -> [(Key, Dur, CType)] -> Music Pitch
> makeJazzy amt doTies g chords = 
>     let [gJ, gR, gOPC, gOPC2] = take 4 $ splitN g
>         jts = map toJTriple chords
>         ms = map (\(a,b,c) -> ([],c)) jts -- get just modes as JChords
>         qJ = myModeSpace
>         chordsJ = greedyLet (const True) nearFallJ [] (map (eqClass qJ modeEq) ms) gJ
>         bNotes = map (\(x,y) -> ([head x],y)) chordsJ -- just the bassline
>         uNotes = map (\(x,y) -> (tail x, y)) chordsJ -- upper chord portion
>         bEs = map (eqClass bassSpace opcEq) $ map fst bNotes -- OPC equivalence classes for chords
>         uEs = map (eqClass upperSpace opcEq) $ map fst uNotes
>         bChordsOPC = greedyProg' (const True) nearFall gOPC bEs -- random walk through OPC-space
>         uChordsOPC = greedyProg' (spaceClass2 4) nearFall gOPC2 uEs -- random walk through OPC-space
>         chordsOPC = zipWith (++) bChordsOPC uChordsOPC
>         chordsOPC' = zipWith newP jts chordsOPC -- tag with dur & mode
>         ties2' = if doTies then map ties2 else id
>         voices = stagger amt $ ties2' $ toVoices chordsOPC' -- place in voice format
>         voicesB = ties2' $ toVoices $ zipWith newP jts bChordsOPC
>         voicesU = stagger amt $ ties2' $ toVoices $ zipWith newP jts  uChordsOPC
>     in  (vsToMusic voicesB, vsToMusic voicesU, vsToMusic voices)

> stagger amt vs = 
>     let amts = map (*amt) $ map fromIntegral [0..length vs-1]
>         fsts = zipWith ((\(k,d,p) a -> (k,a ,-1)).head) vs amts
>     in  zipWith (:) fsts vs


> rA1a = (I, 0.3) :-> \p -> [i (h p), i (h p)]
> rA1b = (I, 0.3) :-> \p -> [i (q p), iv (q p), v (q p), i (q p)]
> rA1c = (I, 0.4) :-> \p -> [i (q p), v (q p), v (q p), i (q p)]

> rA4a = (IV, 0.3) :-> \p -> [iv (h p), iv (h p)]
> rA4b = (IV, 0.3) :-> \p -> [v (h p), iv (h p)]
> rA4c = (IV, 0.4) :-> \p -> [i (h p), iv (h p)]

> rA5a = (V, 0.3) :-> \p -> [v (h p), v (h p)]
> rA5b = (V, 0.3) :-> \p -> [iv (h p), v (h p)]
> rA5c = (V, 0.4) :-> \p -> [i (h p), v (h p)]

> rulesA = map (toRelDur2 (<qn)) [
>     rA1a, rA1b, rA1c, 
>     rA4a, rA4b, rA4c, 
>     rA5a, rA5b, rA5c] 

> rB1a = (I, 0.9) :-> \p -> [i p]
> rB1b = (I, 0.1) :-> \p -> [iii p]

> rB4a = (IV, 0.7) :-> \p -> [iv p]
> rB4b = (IV, 0.3) :-> \p -> [vi p]

> rB5a = (V, 0.7) :-> \p -> [v p]
> rB5b = (V, 0.3) :-> \p -> if mode p == Minor then [v p]
>                           else [vii p]

> rulesB = [rB1a, rB1b, rB4a, rB4b, rB5a, rB5b]

> startVal2 = [NT (I, MP 4 Minor 0)]
> (_, prog1A) = gen rulesA (mkStdGen 2351, startVal2) !! 4
> (_, prog1B) = gen rulesB (mkStdGen 2341, prog1A) !! 1
> prog1BTrips = toTriples prog1B
> (prog1MB, prog1MU, prog1M) = makeJazzy 0 True (mkStdGen 835) prog1BTrips 
> (prog1MBA, prog1MUA, prog1MA) = makeJazzy tn False (mkStdGen 83) prog1BTrips 


> startVal3 = [NT (I, MP 2 Minor 0)]
> (_, prog2A) = gen rulesA (mkStdGen 23510, startVal3) !! 3
> (_, prog2B) = gen rulesB (mkStdGen 23412, prog2A) !! 1
> prog2BTrips = toTriples prog2B
> (prog2MB, prog2MU, prog2M) = makeJazzy 0 True (mkStdGen 396) prog2BTrips 
> (prog2MBA, prog2MUA, prog2MA) = makeJazzy tn False (mkStdGen 528) prog2BTrips 


> startVal4= [NT (I, MP 4 Minor 0)]
> (_, prog4) = gen rulesA (mkStdGen 2351, startVal4) !! 4
> prog4Trips = toTriples prog4
> (prog4MB, prog4MU, prog4M) = makeJazzy 0 True (mkStdGen 2360) prog4Trips 
> (prog4MBA, prog4MUA, prog4MA) = makeJazzy tn False (mkStdGen 2360) prog4Trips 

=======================================

Final parts

> part1 = addVolume 70 $ tempo (1/8) prog1M
> part2 = addVolume 70 $ tempo (1/8) prog1MA -- for absynth5 pads
> part2Bass = addVolume 70 $ tempo (1/8) prog1MBA
> part3 = addVolume 70 $ tempo (1/16) prog2MA 
> part4 = addVolume 70 $ tempo (1/8) prog4MA 

> melFragA1 = tempo (1/8) $ scaleVolume 0.7 mel0dMP1
> melFragA2 = tempo (1/8) $ scaleVolume 0.7 mel0dMP2

> melody1 = tempo (1/8) $ scaleVolume 0.7 ( -- for absynth5 DoughnutUniverse
>     mel0dMP2 :=: (rest den :+: mel0aMP2))

> melody2 = tempo (1/8) $ scaleVolume 0.7 ( -- for absynth5 DoughnutUniverse or I See Stars
>     mel0bMP2 :=: (rest den :+: mel0cMP2))

> melody3 = tempo (1/16) $ scaleVolume 0.7 melody1 -- for MetalStretchHorror
> melody3a = takeM 64 melody3 -- for MetalStretchHorror
> melody4 = tempo (1/16) $ scaleVolume 0.7 melody2 -- for MetalStretchHorror
> melody4b = tempo 4 melody4

> melody5 = transpose (-36) $ tempo (1/8) $ scaleVolume 0.9 (mel0dSimpMP1) -- for crystallization
> melody5seg = takeM 8 melody5 -- for crystallization outro

> preFolder = "mid\\"

> main = do
>     writeMidi (preFolder++"part1.mid") part1
>     writeMidi (preFolder++"part2.mid") part2
>     writeMidi (preFolder++"part2Bass.mid") part2Bass
>     writeMidi (preFolder++"part3.mid") part3
>     writeMidi (preFolder++"part4.mid") part4
>     writeMidi (preFolder++"melFragA1.mid") melFragA1
>     writeMidi (preFolder++"melFragA2.mid") melFragA2
>     writeMidi (preFolder++"melody1.mid") melody1
>     writeMidi (preFolder++"melody2.mid") melody2
>     writeMidi (preFolder++"melody3.mid") melody3
>     writeMidi (preFolder++"melody3a.mid") melody3a
>     writeMidi (preFolder++"melody4.mid") melody4
>     writeMidi (preFolder++"melody4b.mid") melody4b
>     writeMidi (preFolder++"melody5.mid") melody5

