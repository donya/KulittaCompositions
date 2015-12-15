Special playback functions
Playback Function
Donya Quick
Last modified: 26-Sept-2014

Playing Music and MIDI values with stricter timing and to 
user-specified MIDI devices rather than just the default.

> module Play2 where
> import Euterpea
> import Codec.Midi
> import Sound.PortMidi
> import System.IO.Unsafe (unsafePerformIO)
> import Euterpea.IO.MIDI.MidiIO
> import System.Directory
> import Control.DeepSeq
> import qualified Euterpea.ExperimentalPlay as EP

CORRECT PLAYBACK TIMING

Version 1: this one fixes most of the delay, but sometimes
there is still a problem with the first chord when the
music value is huge.

> playS1 :: (Performable a, NFData a) => Music a -> IO ()
> playS1 m = m `deepseq` (playM $ testMidi m)

Version 2: timing is basically as good as can be expected 
from most MIDI players. 

> playS2 :: (Performable a, NFData a) => Music a -> IO ()
> playS2 m =  
>     let x = testMidi m 
>     in  x `deepseq` playM x


PLAYING TO DIFFERENT DEVICES

Function to list all available devices:

> devices = EP.devices

Original device-specific playback version: 
(fails on long delays, just like "play") 

> playDev :: Performable a => DeviceID -> Music a -> IO ()
> playDev devID m = playM' devID $ testMidi m

Redefinition of playM to support the above.

> playM' :: DeviceID -> Midi -> IO ()
> playM' devID midi = do 
>   initialize
>   playMidi (unsafeOutputID devID) midi 
>   terminate
>   return ()

Strict-timing version:

> playDevS :: Performable a => DeviceID -> Music a -> IO ()
> playDevS devID m = 
>     let x = testMidi m
>     in  x `deepseq` playM' devID x

=======================
