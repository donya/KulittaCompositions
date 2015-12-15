Post Processing Module to Link Grammar with OPTIC Functions
Donya Quick and Paul Hudak
Last modified: 18-June-2014
For paper: Grammar-Based Automated Music Composition in Haskell

Post processing module to turn Terms into music using Euterpea.

> module PostProc where
> import Euterpea hiding (transpose)
> import GrammarMonad
> import ChordSpaces
> import Data.List
> import System.Random

Intermediate types:

> type Key = (AbsPitch, Mode)
> type RChord = (Key, Dur, CType)
> type TChord = (Key, Dur, AbsChord)
> type TNote = (Key, Dur, AbsPitch)
> type Voice = [TNote]

Accessing the members of a TNote:

> tnK (k,d,p) = k
> tnD (k,d,p) = d
> tnP (k,d,p) = p
> newP (k,d,p) p' = (k,d,p')


The goal using these intermediate types is the following:

INPUT	     STEP            OUTPUT         FUNCTION
Seeds -----(grammar)-------> Sentence       gen
Sentence --(mode info)-----> [TChord]       toAbsChords
[TChord] ------------------> [Voice] 		toVoices
[Voice] -------------------> Music Pitch    vsToMusic or vsToMusicI


> toChords :: Term CType -> Mode -> [RChord]
> toChords (NT (Chord d c)) m = [((0, m), d, c)]
> toChords (S ts) m = concatMap (\t -> toChords t m) ts
> toChords (Mod mt x) m = 
>     let (amt, mt') = getMods mt m
>         f = map (\((k,m),d,c) -> ((k+amt,mt'),d,c)) 
>     in  f (toChords x mt') 
> toChords x m = 
>     error ("(toChords) Unable to handle expression: "++showTerm x)

Function to convert into intermediate type [TChord]:

> toAbsChord :: RChord -> TChord
> toAbsChord ((k,m),d,c) = ((k,m), d, t k $ toAs c m)

> toAbsChords :: Term CType -> Mode -> [TChord]
> toAbsChords ts m = map toAbsChord $ toChords ts m

We also provide an alternate version that doesn't use diminished chords.

> toAbsChordNoDim :: RChord -> TChord
> toAbsChordNoDim ((k,m),d,c) = ((k,m), d, t k $ toAsNoDim c m)

> toAbsChordsNoDim :: Term CType -> Mode -> [TChord]
> toAbsChordsNoDim ts m = map toAbsChordNoDim$ toChords ts m

Conversion of a single chord to a mode rooted at zero:

> toAs :: CType -> Mode -> [AbsPitch]
> toAs ct m = 
>     let s = getScale m ++ map (+12) s -- fininite scale
>         i = head $ findIndices (==ct) [I, II, III, IV, V, VI, VII] -- can be updated w/enum
>     in  map (s !!) $ map (+i) [0,2,4]

> toAsNoDim :: CType -> Mode -> [AbsPitch]
> toAsNoDim ct m = 
>     let s = getScale m ++ map (+12) s -- fininite scale
>         i = head $ findIndices (==ct) [I, II, III, IV, V, VI, VII] -- can be updated w/enum
>     in  map (s !!) $ fixDim $ map (+i) [0,2,4] where
>         fixDim x = if optEq x [0,3,6] then t (head x) [0,3,7] else x

Fundtions to get information on modes:

> getScale :: Mode -> [AbsPitch]
> getScale Major = [0,2,4,5,7,9,11]
> getScale Minor = [0,2,3,5,7,8,10]

> getMods mt m = 
>     let mts = [M2, M3, M4, M5, M6, M7]
>         i = (head $ findIndices (==mt) mts)+1
>     in  (getScale m !! i, relPat m !! i) where
>    relPat :: Mode -> [Mode]
>    relPat Major = [Major, Minor, Minor, Major, Major, Minor, Minor]
>    relPat Minor = [Minor, Minor, Major, Minor, Minor, Major, Major]

Transposition using a key (to avoid C-major assignment only):

> atTrans :: AbsPitch -> [(Key, Dur, AbsChord)] -> [(Key, Dur, AbsChord)]
> atTrans a = map (\((k,m),d,c) -> ((fixK k a m,m),d, t (a `mod` 12) c)) 

The toCords functon does a similar thing, but returns a CType and 
its key/mode context without performing the conversion to AbsChord.

> ctTrans :: AbsPitch -> [(Key, Dur, CType)] -> [(Key, Dur, CType)]
> ctTrans a = map (\((k,m),d,c) -> ((fixK k a m,m),d,c)) 

> fixK k a Major = (k + a) `mod` 12
> fixK k a Minor = ((k + a) `mod` 12) + 12


Conversion of intermediate type to Music Pitch:

> tChordsToMusic :: [TChord] -> Music Pitch
> tChordsToMusic = line . map f  where
>     f ((k,m),d, as) = chord $ map (\a -> note d (pitch a)) as


============ SPLITTING VOICES APART ===========

The code here places TChords into a form more suitable
for additional musical processing. A Voice is a list of
pitches with duration and key/mode context. 

> toVoices :: [TChord] -> [Voice]
> toVoices ts = 
>     let (ks,ds,ps) = unzip3 ts
>     in  map (\v -> zip3 ks ds v) $ transpose ps

> toNotes :: Voice -> Music Pitch
> toNotes = line . map (\(k,d,p) -> note' d p) where
>     note' d p = if p<0 then rest d else note d (pitch p)

> vsToMusic :: [Voice] -> Music Pitch
> vsToMusic = 
>     chord . map toNotes 

> vsToMusicI :: [InstrumentName] -> [Voice] -> Music Pitch
> vsToMusicI is = 
>     chord . zipWith (\i m -> instrument i m) is . map toNotes







