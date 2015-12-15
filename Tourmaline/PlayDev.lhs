> module PlayDev where
> import Euterpea.ExperimentalPlay
> import Euterpea.IO.MIDI.MidiIO
> import Euterpea.IO.MIDI.ToMidi
> import Control.DeepSeq
> import Euterpea


> playDev :: (NFData a, Performable a) => Int -> Music a -> IO ()
> playDev dev m = 
>     playC defParams{devID = mkID dev, strict=True, closeDelay = 2.0} (rest hn :+: m) where
>     mkID x = Just $ unsafeOutputID x