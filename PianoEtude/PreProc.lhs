> module PreProc where
> import Euterpea hiding (key)
> import Data.List
> import Data.Char
> import System.IO.Unsafe

Donya Quick
Parser for csv data files
Last modified: 19-Nov-2013

Current parser: procFile8

> type DChord = (AbsPitch, Mode, [AbsPitch]) -- chord info
> type DPoint = (String, Double, Maybe DChord) -- data point info
> data FPoint = FPoint {example :: Int,
>                       fname :: String, 
>                       beats :: Double, 
>                       key :: AbsPitch, 
>                       mode :: Mode, 
>                       pcs :: [AbsPitch], -- original
>                       pcguess :: [AbsPitch], -- guessed trichord
>                       ind :: Int, -- Roman numeral index from 0 to 6
>                       tsd :: String} -- String as "T", "S", or "D"
>     deriving (Show)

==============

> procBach = 
>     procFile8' "BachChoraleXmlListOfChords_tab.csv"
>     8.0 "out_data.csv" "out_chords.txt" "BachChords" "BachTSD"

==============

FINAL FORMATTING

> procFile8' fp dMax fpOut fpChords mName mName2 = do
>     putStrLn "Working..."
>     ps0 <- procFile3 fp -- raw DPoints grouped by file
>     let ps1 = filter (not.null) $ groupPs2 dMax $ map fixDPoints ps0 -- group by example sizes
>         fs = zipWith toFPoints [0..length ps1-1] ps1
>         fs' = groupByKey $ filter (and . map (\x -> ind x >= 0)) fs
>         comments = mkComments2 fp dMax
>     writeFile fpOut $ labels ++ toFStr fs
>     writeFile fpChords $ justChords fs
>     writeFile (mName++".lhs") $ buildLHS1 mName comments fs' 
>     writeFile (mName2++".lhs") $ buildLHS2 mName2 comments fs' 
>     putStrLn "Done."

> mkComments2 :: String -> Double -> String
> mkComments2 fn dMax = "Auto-generated code and derived numerals.\n"++
>             "File: "++fn++"\n"++
>             "Maximum phrase duration (beats): "++show dMax++"\n"++
>             "Data format: (Key, Mode, Chords)\n\n"

> buildLHS1 :: String -> String -> [[FPoint]] -> String
> buildLHS1 mName comments fps = 
>     let header = comments++
>                  "> module "++mName++" where\n"++
>                  "> import Euterpea\n"++
>                  "> import Rohrmeier\n\n"
>         body1 = "> chordData = [\n" ++
>                 (concat $ intersperse ",\n" $ map mkLhsLine fps) ++ 
>                 "\n>     ]\n\n"
>     in  header ++ "\n\n" ++ body1 ++ "\n\n"

> buildLHS2 :: String -> String -> [[FPoint]] -> String
> buildLHS2 mName comments fps = 
>     let header = comments++
>                  "> module "++mName++" where\n"++
>                  "> import Euterpea hiding (D)\n"++
>                  "> import Rohrmeier\n\n"
>         body2 = "> tsdData = [\n" ++
>                 (concat $ intersperse ",\n" $ map mkLhsLine2 fps) ++ 
>                 "\n>     ]\n\n"
>     in  header ++ "\n\n" ++ body2 ++ "\n\n"

> mkLhsLine :: [FPoint] -> String
> mkLhsLine [] = error "(mkLhsLine) Empty list not accepted."
> mkLhsLine fps = 
>     let m = mode (head fps)
>         k = key (head fps) 
>         cs = concat $ intersperse ", " $ 
>              map (showR alph2 m . ind) fps
>     in  ">     ("++show k++", "++show m++", ["++cs++"])"

> mkLhsLine2 :: [FPoint] -> String
> mkLhsLine2 [] = error "(mkLhsLine2) Empty list not accepted."
> mkLhsLine2 fps = 
>     let m = mode (head fps)
>         k = key (head fps) 
>         tcs = concat $ intersperse ", " $ map tsd fps
>     in  ">     ("++show k++", "++show m++", ["++tcs++"])"

==============

GUESS TSD LEVEL

Major assignment:
T S  T   S  D T  D
I II III IV V VI VII

Minor assignment:
T S  T   S  D T/S D  <-- will ignore /S option for now
I II III IV V VI  VII

> toTSD :: Mode -> Int -> String
> toTSD m i = ["T", "S", "T", "S", "D", "T", "D"] !! i


==============

GROUPING BY KEY

> groupByKey :: [[FPoint]] -> [[FPoint]]
> groupByKey = concatMap splitKey

> splitKey :: [FPoint] -> [[FPoint]]
> splitKey [] = []
> splitKey (h:t) = 
>     let hGroup = h : takeWhile ((==key h).key) t
>         rest = drop (length hGroup) (h:t)
>     in  hGroup : splitKey rest


==============

GUESSING ROMAN NUMERALS

Produce a string of just Roman numerals I-VII from FPoints.

> justChords :: [[FPoint]] -> String
> justChords fps = 
>     let f fp = showR alph2 (mode fp) (ind fp) ++ " "
>         km fs = let x = head fs in show (key x) ++ " " ++
>                         show (mode x) ++ "\t" 
>     in  concatMap (\fs -> km fs ++ concatMap f fs ++ "\n") fps

> toFPoints :: Int -> [DPoint] -> [FPoint]
> toFPoints i ps = zipWith guessFPoint (repeat i) ps

writeFile fpOut $ labels ++ (toFStr $ map (map guessFPoint) ps')

> labels = "Example\tFile\tBeats\tKey\tMode\tPC In\tPC Guess\tSame?\tR Guess\tR Simp\tTSD\n"

> toFStr1 :: FPoint -> String
> toFStr1 (FPoint e s b k m c c' i tsd) = 
>     show e ++ "\t" ++ s ++ "\t" ++ show b ++ "\t" ++ 
>     show k ++ "\t" ++ show m ++ "\t" ++ show c ++ "\t" ++ 
>     show c' ++ "\t" ++ show (c==c') ++ "\t" ++
>     showR alph1 m i ++ "\t" ++ showR alph2 m i ++ "\t" ++
>     tsd ++ "\n"

> showR alph m i = if i<0 then "Ambiguous" else
>     case m of
>         Major -> fst alph !! i
>         Minor -> snd alph !! i

> alph1 = (["I", "ii", "iii", "IV", "V", "vi", "viio"], 
>          ["i", "iio", "III", "iv", "v", "VI", "VII"])

> alph2 = (["I", "II", "III", "IV", "V", "VI", "VII"],
>          ["I", "II", "III", "IV", "V", "VI", "VII"])

> toFStr fparts = concatMap (concatMap toFStr1) fparts


> mkLine :: DPoint -> String
> mkLine (n,b,Nothing) = n ++ "\t" ++ show b ++ "-\t-\t-\n"
> mkLine (n,b,Just (k,m,c)) = n++"\t"++show b++"\t"++show k++"\t"++show m++"\t["++showx c++"]\n"

> showx :: Show a => [a] -> String
> showx [] = []
> showx [x] = show x
> showx (x:xs) = show x ++ ", " ++ showx xs


> --          example#  dataIn    dataOut
> guessFPoint :: Int -> DPoint -> FPoint
> guessFPoint e (s,b,Nothing) = FPoint e s b (-1) Major [] [] (-1) "_"
> guessFPoint e (s,b,Just (k,m,c)) = 
>     let (c', i) = guessChord2 (k,m,c) -- already neighbor adjusted, now guess new PCs and index
>     in  FPoint e s b k m c c' i $ toTSD m i

> templateGuess (k, Minor, c) = c -- nothing to do for Minor yet
> templateGuess (k,Major,c) = 
>     if c==[0,2,7] then [11,2,7] -- should be a V
>     else if elem 6 c then replace 6 5 c
>     else c

> replace x y [] = []
> replace x y (x1:xs) = if x==x1 then y : replace x y xs else x1 : replace x y xs

> guessChord2 :: DChord -> ([AbsPitch], Int)
> guessChord2 (k,m,c) = 
>     let cs = case m of Major -> toPref majorCs -- major chords in preference order
>                        Minor -> toPref minorCs -- minor chords in preference order
>         c0 = templateGuess (k,m,c) -- perform brute force conversions
>         c' = if elem (normOPC c) cs then c0 else -- if the chord is an exact match, keep it
>              bestShared c0 cs -- otherwise, find the best match
>     in  (c', preferenceOrder !! (head $ findIndices (==c') cs))

> numShared :: (Eq a) => [a] -> [a] -> Int
> numShared xs ys = length $ [x | x<-xs, elem x ys]

> bestShared :: [AbsPitch] -> [[AbsPitch]] -> [AbsPitch]
> bestShared c [] = error "Nothing to compare."
> bestShared c cs = 
>     let scores = map (numShared c) cs
>         m = maximum scores
>         i = head $ findIndices (==m) scores
>     in  cs !! i

> t c = map (+c)
> normO = map (`mod` 12)
> normOPC = nub . sort . normO
> opcEq a b = normOPC a == normOPC b

> (majC, minC, dimC) = ([0,4,7], [0,3,7], [0,3,6])
> majS = [0,2,4,5,7,9,11]
> minS = [0,2,3,5,7,8,10]

> majorCs = map normO $ zipWith t majS 
>     [majC, minC, minC, majC, majC, minC, dimC]
> minorCs = map normO $ zipWith t minS 
>     [minC, dimC, majC, minC, minC, majC, majC] 

I V II IV VI VII III
1 5 2  4  6  7   3
0 4 1  3  5  6   2

> preferenceOrder = [0, 4, 1, 3, 5, 6, 2] :: [Int]
> toPref xs = map (xs !!) preferenceOrder

==============

GUESSING CHORDS FOR NON-TRICHORDS

> procFile5 fp minLen maxLen = do
>     ps <- procFile4 fp minLen maxLen 
>     return $ map fixDPoints ps

> fixDPoints :: [DPoint] -> [DPoint]
> fixDPoints ps = map (fixDPoint ps) [0..length ps-1]

> extract :: [Maybe a] -> [a]
> extract [] = []
> extract (Nothing:t) = extract t
> extract (Just h:t) = h:extract t


> testDPoints = [("foo", 8, Just (5,Major,[0,4,7])),
>                ("foo", 9, Just (5,Major,[5,9,0])),
>                ("foo", 10, Just (5,Major,[11,2,4,5])),
>                ("foo", 10.5, Just (5,Major,[11,2,5])),
>                ("foo", 11, Just (5,Major,[0,4,7])),
>                ("foo", 11.5, Just (5,Major,[6,9,0])),
>                ("foo", 12, Just (5,Major,[7,11])),
>                ("foo", 12.5, Just (5,Major,[7,11,2])),
>                ("foo", 13, Just (5,Major,[9,0,4])),
>                ("foo", 13.5, Just (5,Major,[9,11,0,4])),
>                ("foo", 14, Just (5,Major,[9,2])),
>                ("foo", 15, Just (5,Major,[7,11,2]))] :: [DPoint]

> fixDPoint :: [DPoint] -> Int -> DPoint
> fixDPoint ds i = 
>     let dp@(n,b,c) = ds !! i
>         ns = map (\(x,y,z) -> z) $ neighbors i ds -- find neighbors of the chord
>         ns' = extract ns -- get out of the Maybe type for neighbors
>     in  case c of Nothing -> dp -- ambiguous chord case
>                   Just c' -> (n,b, Just $ fixWith c' ns') -- adjusted chord case

> neighbors :: Int -> [a] -> [a]
> neighbors i xs = 
>     let inds = [i-1, i+1]
>     in  map (xs!!) $ filter (\i -> i >=0 && i < length xs) inds

> fixWith :: DChord -> [DChord] -> DChord
> fixWith (a,m,ps) ns = if length ps == 3 then (a,m,ps) else
>     let ns' = filter f ns
>         f (_,_,ps') = length ps' == 3 && 
>                       (subset ps ps' || subset ps' ps)
>     in  if null ns' then (a,m,ps) else head ns'

> subset xs ys = length (filter (\x -> elem x ys) xs) == length xs

==============

SPLITTING EXAMPLES APART

> --procFile4 :: FilePath -> Int -> Int -> IO [[DPoint]]
> procFile4 fp minLen maxLen = do
>     ps <- procFile3 fp -- sorted into dpoints
>     return $ filter ((>=minLen).length) $ concat $ 
>         map (chop maxLen) $ filter noAmb $ ps

> groupPs :: Int -> Int -> [[DPoint]] -> [[DPoint]]
> groupPs minLen maxLen ps = filter ((>=minLen).length) $ concat $ 
>         map (chop maxLen) $ filter noAmb $ ps 

> -- This version assumes things are sorted by file ONLY
> groupPs2 :: Double -> [[DPoint]] -> [[DPoint]]
> groupPs2 dMax ps = concatMap (groupPs2b 1 dMax) ps where
>     groupPs2b :: Double -> Double -> [DPoint] -> [[DPoint]]
>     groupPs2b i dMax ps = 
>         let dCurr = i * dMax
>             x = takeWhile ((<dCurr).snd3) ps
>             xs = drop (length x) ps
>         in  if length x >= length ps then [x] 
>             else x : groupPs2b (i+1) dMax xs

> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd3 (a,b,c) = c

> chop :: Int -> [a] -> [[a]]
> chop n xs = if length xs > n then take n xs : chop n (drop n xs)
>             else [xs]

> noAmb :: [DPoint] -> Bool
> noAmb = and . map f where
>     f (fn,d,Nothing) = False
>     f _ = True

> procFile3 :: FilePath -> IO [[DPoint]]
> procFile3 fp = do
>     ps <- procFile2 fp
>     return $ splitBy2 splitByAll ps

> average xs = fromIntegral (sum xs) / fromIntegral (length xs)

> splitByAll :: DPoint -> DPoint -> Bool
> splitByAll a b = splitByFile a b && 
>     splitByBeat a b && splitByAmb a b

> splitByFile :: DPoint -> DPoint -> Bool 
> splitByFile (f1,d1,c1) (f2,d2,c2) = f1 == f2 

> splitByBeat :: DPoint -> DPoint -> Bool
> splitByBeat (f1,d1,c1) (f2,d2,c2) = d2 > d1

> splitByAmb :: DPoint -> DPoint -> Bool
> splitByAmb (f1,d1,c1) (f2,d2,c2) = 
>     case (c1,c2) of (Just a, Just b) -> True
>                     --(Nothing, Nothing) -> True
>                     _ -> False

> splitBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
> splitBy2 f xs = splitBy2b f [] xs where 
>     splitBy2b :: (a -> a -> Bool) -> [a] -> [a] -> [[a]]
>     splitBy2b f ys [] = [reverse ys]
>     splitBy2b f ys (x:xs) = 
>         if null ys then splitBy2b f [x] xs 
>         else if f (head ys) x then splitBy2b f (x:ys) xs 
>              else reverse ys : splitBy2b f [x] (xs)


================

PARSING

> procFile2 :: FilePath -> IO [DPoint]
> procFile2 fp = do
>     s <- readFile fp
>     return $ map procLine2 $ map getCols $ tail $ tokens ['\n','\r'] s

return $ map procLine2 $ map getCols $ tail $ tokens ['\n','\r'] s

> getCols :: String -> [String]
> getCols s = 
>     let t = splitBy '\t' s
>     in  [t!!7, t!!0, t!!19, t!!20, t!!21]

> -- expects [filename, beat, key, mode, pcs]
> procLine2 :: [String] -> DPoint
> procLine2 t = 
>     let f i = read (t !! i)    
>     in  if (t!!2)=="Ambiguous" then (t!!0, f 1, Nothing)
>         else (t!!0, f 1, Just (f 2, procMode (t !! 3), f 4))

> procFile1 :: FilePath -> IO [DPoint]
> procFile1 fp = do
>     s <- readFile fp
>     return $ map procLine1 $ lines s

> amb :: String -> Bool
> amb s = tokens ['\t'] s !! 2 == "Ambiguous"

> procLine1 :: String -> DPoint
> procLine1 s0 = 
>     let t = tokens ['\t'] s0     
>         f i = read (t !! i)    
>     in  if (t!!2)=="Ambiguous" then (t!!0, f 1, Nothing)
>         else (t!!0, f 1, Just (f 2, procMode (t !! 3), f 4))

> procMode :: String -> Mode
> procMode s = case (map toLower s) of
>     "major" -> Major
>     "minor" -> Minor
>     _ -> error ("Unable to read mode: "++s)

> splitBy :: Char -> String -> [String]
> splitBy c [] = []
> splitBy c s = 
>     let i = findIndex (==c) s 
>     in  case i of 
>             Nothing -> [s]
>             Just j -> take j s : splitBy c (drop (j+1) s)

> tokens :: [Char] -> String -> [String]
> tokens c [] = []
> tokens c s = 
>     let i = findIndex (`elem` c) s 
>     in  case i of 
>             Nothing -> [s]
>             Just j -> take j s : tokens c (dropWhile (`elem` c) $ drop (j+1) s)