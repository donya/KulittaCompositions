Kulitta Examples
Donya Quick

This module provides examples for a number of ways that Kulitta
can be used to generate and learn from music. 

> module Examples where
> import Euterpea hiding (C, D, P)
> import JazzFG
> import ClassicalFG
> import GrammarMonad 
> import GrammarRules
> import System.Random
> import PostProc
> import Learning
> import PCFGtoPTGG
> import Rohrmeier hiding (S)
> import ChordSpaces hiding (i)
> import Data.List
> import TemporalGen
> import RuleUtils
> import LearningGen
> import Search
> import Play2
> import PlayK
> import Constraints



CLASSICAL FOREGROUND EXAMPLES

A test of the greedyLetT capability:

> c0Terms = Let "x" (Let "y" (iv qn) (S[Var "y", Var "y", v qn, i qn])) (S[Var "x", Var "x"])
> c0K = findInds [] c0Terms -- constraints from Term structure
> c0Chords = toAbsChords (expand [] c0Terms) Major
> c0CS = greedyLetT satbOP opcEq (vl7) nearFall c0K c0Chords (mkStdGen 5)
> (c0M, c0FG) = snd $ classicalFG' (mkStdGen 8) c0CS 
> c0Test = writeMidi "classicalTests\\ctest0.mid" c0FG >> -- foreground
>          writeMidi "classicalTests\\ctest0cs.mid" c0M -- chords

Additional tests:

> c1Seed = 5
> c1Terms = gen (ruleSet qn False) 5 c1Seed defm (i 4)
> (c1CSM, c1M) = snd $ classicalFG (mkStdGen 12) (0, Major) c1Terms
> c1Test = writeMidi "classicalTests\\ctest1.mid" c1M >> -- foreground
>          writeMidi "classicalTests\\ctest1cs.mid" c1CSM -- chords

> c2Seed1 = 30
> c2Seed2 = 31
> c2TSeed = Let "x" (i 2) $ S [Var "x", Var "x"]
> c2Terms = gen (ruleSet qn False) 5 c2Seed1 defm c2TSeed
> c2fn1 = "classicalTests\\ctest2.mid"
> c2fn2 = "classicalTests\\ctest2cs.mid"
> c2Test = classicalFG2 (mkStdGen c2Seed2) (0, Major) c2Terms c2fn1 c2fn2

An example using OPT-space with the same progression as in c1Terms:

> c3Chords = toAbsChords c1Terms Major 
> (c3cs, c3ds) = unzip $ map (\(k,d,a) -> (a,(k,d))) c3Chords
> c3cs' = greedyProg qOPTC optcEq vl7 nearFall (mkStdGen 6) c3cs where
>     qOPTC = satbR (mkStdGen 123) satbFilter2 optcEq 
> c3Chords' = zipWith (\(k,d) a -> (k,d,a)) c3ds c3cs'
> c3M = vsToMusicI [Bassoon, EnglishHorn, Clarinet, Oboe] $ toVoices c3Chords'
> c3Test = writeMidi "classicalTests\\ctest3cs.mid" c3M

============================

JAZZ FOREGROUND EXAMPLES

Format of gen calls:
	gen rules iters seed context term

Simple example:
	
> (defc, defm) = (0, Major)
> j1Terms = gen (ruleSet qn False) 6 4 defm (i 4)
> j1 = jazzFG1T (mkStdGen 0) (defc, defm) j1Terms []
> j1Test = writeMidi "jazzTests\\jtest1.mid" $ snd j1

Progression interpreted using both jazz foreground approaches:

> (defc2, defm2) = (12, Minor)
> j2Terms = gen (ruleSet wn False) 6 4 defm2 (i 8) -- create the Term
> j2a = jazzFG1T (mkStdGen 0) (defc2, defm2) j2Terms [] -- generate a simple foreground
> j2TestA = writeMidi "jazzTests\\jtest2a.mid" $ snd j2a -- write a MIDI file
> j2b = jazzFG2T (mkStdGen 1) (defc2, defm2) j2Terms [] -- create a bossa nova foreground
> j2TestB = writeMidi "jazzTests\\jtest2b.mid" $ snd j2b -- write a MIDI file
> j2TestT = writeFile "jazzTests\\jtest2t.txt" $ show $ flatten j2Terms -- write the Term

Another progression interpreted using both jazz foreground approaches:

> (defc3, defm3) = (4, Major)
> j3Terms = gen (ruleSet wn False) 6 14 defm3 (i 8)
> j3a = jazzFG1T (mkStdGen 12) (defc3, defm3) j3Terms []
> j3TestA = writeMidi "jazzTests\\jtest3a.mid" $ snd j3a
> j3b = jazzFG2T (mkStdGen 12) (defc3, defm3) j3Terms []
> j3TestB = writeMidi "jazzTests\\jtest3b.mid" $ snd j3b
> j3TestT = writeFile "jazzTests\\jtest3t.txt" $ show $ flatten j3Terms


The first classical example interpreted as jazz chords:

> j4a = jazzFG1T (mkStdGen 15) (defc, defm) c1Terms []
> j4TestA = writeMidi "jazzTests\\jtest4a.mid" $ snd j4a


===========================

Learning Production Probabilities

The learning module is too CPU-intensive to run well in GHCI.
It is parallelized, so it is best compiled in GHC and run from 
a command prompt using multiple cores when possible. This can 
be done by:

1. Compile Learning.lhs using 
	ghc -O2 Main.lhs -rtsopts -threaded -o Learning.exe
	
2. Run Learning.exe configFile +RTS -Nx
   configFile = name of configuration file
   x = number of cores (ex: use -N8 for 8 cores)

Bach PCFG example from thesis: config5.txt
Produces: tconfig_final.txt with results of all runs
Runs in a few minutes on an i7 at 3.8GHz with 8 threads.

PTGG example from synthetic data: tconfig5.txt
Produces: tconfig5_final.txt with results of all runs
Runs in a a couple of hours on an i7 at 3.8GHz with 8 threads.


============================

Examples of Learning + Generation

The following code generates musical examples using probabilities 
learned from a corpus of Bach chorales (config5.txt as referenced 
in the previous section).

Generation Steps:
1. Parse input PCFG or PTGG file from learning
2. Parse ouput probs file and take average over all runs
3. Convert the grammar if needed
4. Generate with the grammar in various ways over a collection of seeds


> pcfgFile = "RohrSimp.txt" -- input grammar
> pcfgFileE = "RohrSimpTSD2.txt" -- input grammar
> ptggFileA = "trules.txt" -- input grammar
> ptggFileB = "trules3.txt" -- input grammar

> pcfgPFile = "config5_final.txt" 
> pcfgPFileE = "config9_final.txt" -- output probabilities
> ptggPFileA = "tconfig4_final.txt" -- with identity rules
> ptggPFileB = "tconfig5_final.txt" -- no identity rules

> outFolder1 = "learning\\gen\\PCFG\\pcfg"
> outFolder1E = "learning\\gen\\PCFG_E\\pcfgE"
> outFolder2a = "learning\\gen\\PTGG_A\\ptggA"
> outFolder2b = "learning\\gen\\PTGG_B\\ptggB"

> seeds = [0..19]

> makeExamplesPCFG = pcfgGenExamples pcfgFile pcfgPFile outFolder1 seeds
> makeExamplesPCFG_E = pcfgGenExamplesE pcfgFileE pcfgPFileE outFolder1E seeds
> makeExamplesPTGG_A = ptggGenExamples ptggFileA ptggPFileA outFolder2a seeds 8
> makeExamplesPTGG_B = ptggGenExamples ptggFileB ptggPFileB outFolder2b seeds 3 



============================

Mixed-style examples

> m1Terms = gen (ruleSet qn False) 5 40 defm (i 4)
> (m1CS1, m1FG1) = snd $ classicalFG (mkStdGen 209) (0, Major) m1Terms
> m1J = snd $ jazzChords (mkStdGen 70) (toChords m1Terms Major) []
> (m1CS2, m1FG2) = snd $ classicalFG' (mkStdGen 231) m1J
> m1Test = writeMidi "jazzTests\\mtest1CS1.mid" m1CS1 >> 
>          writeMidi "jazzTests\\mtest1FG1.mid" m1FG1 >>
>          writeMidi "jazzTests\\mtest1CS2.mid" m1CS2 >> 
>          writeMidi "jazzTests\\mtest1FG2.mid" m1FG2 


=============================

> classicalExamples = 
>     putStrLn "Writing classical examples..." >> 
>     c1Test >> c2Test >> c3Test >>
>     putStrLn "Done."

> jazzExamples = 
>     putStrLn "Writing jazz examples..." >> 
>     j1Test >> j2TestA >> j2TestB >> j3TestA >> j3TestB >>
>     putStrLn "Done."

> mixedExamples = 
>     putStrLn "Writing mixed examples..." >>
>     m1Test >>
>     putStrLn "Done."

> makeLearningExamples = 
>     putStrLn "Writing learning examples..." >>
>     makeExamplesPCFG >> makeExamplesPCFG_E >>
>     makeExamplesPTGG_A >> makeExamplesPTGG_B >> 
>     putStrLn "Done."

> makeAllExamples = 
>     classicalExamples >> jazzExamples >> mixedExamples >> makeLearningExamples

