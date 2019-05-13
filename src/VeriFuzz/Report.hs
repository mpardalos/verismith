{-|
Module      : VeriFuzz.Report
Description : Generate a report from a fuzz run.
Copyright   : (c) 2019, Yann Herklotz Grave
License     : GPL-3
Maintainer  : ymherklotz [at] gmail [dot] com
Stability   : experimental
Portability : POSIX

Generate a report from a fuzz run.
-}

{-# LANGUAGE TemplateHaskell #-}

module VeriFuzz.Report
    ( SynthTool(..)
    , SynthStatus(..)
    , SimTool(..)
    , FuzzReport(..)
    , synthResults
    , simResults
    , synthStatus
    , defaultIcarusSim
    , defaultVivadoSynth
    , defaultYosysSynth
    , defaultXSTSynth
    , defaultQuartusSynth
    , defaultIdentitySynth
    , descriptionToSim
    , descriptionToSynth
    )
where

import           Control.DeepSeq       (NFData, rnf)
import           Control.Lens          hiding (Identity)
import           Data.ByteString       (ByteString)
import           Data.Maybe            (fromMaybe)
import           Prelude               hiding (FilePath)
import           Shelly                (fromText)
import           VeriFuzz.Config
import           VeriFuzz.Result
import           VeriFuzz.Sim.Icarus
import           VeriFuzz.Sim.Identity
import           VeriFuzz.Sim.Internal
import           VeriFuzz.Sim.Quartus
import           VeriFuzz.Sim.Vivado
import           VeriFuzz.Sim.XST
import           VeriFuzz.Sim.Yosys

-- | Common type alias for synthesis results
type UResult = Result Failed ()

-- | Commont type alias for simulation results
type BResult = Result Failed ByteString

data SynthTool = XSTSynth {-# UNPACK #-} !XST
               | VivadoSynth {-# UNPACK #-} !Vivado
               | YosysSynth {-# UNPACK #-} !Yosys
               | QuartusSynth {-# UNPACK #-} !Quartus
               | IdentitySynth {-# UNPACK #-} !Identity
               deriving (Eq)

instance NFData SynthTool where
    rnf (XSTSynth a)      = rnf a
    rnf (VivadoSynth a)   = rnf a
    rnf (YosysSynth a)    = rnf a
    rnf (QuartusSynth a)  = rnf a
    rnf (IdentitySynth a) = rnf a

instance Show SynthTool where
    show (XSTSynth xst)           = show xst
    show (VivadoSynth vivado)     = show vivado
    show (YosysSynth yosys)       = show yosys
    show (QuartusSynth quartus)   = show quartus
    show (IdentitySynth identity) = show identity

instance Tool SynthTool where
    toText (XSTSynth xst)           = toText xst
    toText (VivadoSynth vivado)     = toText vivado
    toText (YosysSynth yosys)       = toText yosys
    toText (QuartusSynth quartus)   = toText quartus
    toText (IdentitySynth identity) = toText identity

instance Synthesiser SynthTool where
    runSynth (XSTSynth xst)           = runSynth xst
    runSynth (VivadoSynth vivado)     = runSynth vivado
    runSynth (YosysSynth yosys)       = runSynth yosys
    runSynth (QuartusSynth quartus)   = runSynth quartus
    runSynth (IdentitySynth identity) = runSynth identity

    synthOutput (XSTSynth xst)           = synthOutput xst
    synthOutput (VivadoSynth vivado)     = synthOutput vivado
    synthOutput (YosysSynth yosys)       = synthOutput yosys
    synthOutput (QuartusSynth quartus)   = synthOutput quartus
    synthOutput (IdentitySynth identity) = synthOutput identity

    setSynthOutput (YosysSynth yosys)     = YosysSynth . setSynthOutput yosys
    setSynthOutput (XSTSynth xst)         = XSTSynth . setSynthOutput xst
    setSynthOutput (VivadoSynth vivado)   = VivadoSynth . setSynthOutput vivado
    setSynthOutput (QuartusSynth quartus) = QuartusSynth . setSynthOutput quartus
    setSynthOutput (IdentitySynth identity) = IdentitySynth . setSynthOutput identity

defaultYosysSynth :: SynthTool
defaultYosysSynth = YosysSynth defaultYosys

defaultQuartusSynth :: SynthTool
defaultQuartusSynth = QuartusSynth defaultQuartus

defaultVivadoSynth :: SynthTool
defaultVivadoSynth = VivadoSynth defaultVivado

defaultXSTSynth :: SynthTool
defaultXSTSynth = XSTSynth defaultXST

defaultIdentitySynth :: SynthTool
defaultIdentitySynth = IdentitySynth defaultIdentity

newtype SimTool = IcarusSim Icarus
                deriving (Eq)

instance NFData SimTool where
    rnf (IcarusSim a) = rnf a

instance Tool SimTool where
    toText (IcarusSim icarus) = toText icarus

instance Simulator SimTool where
    runSim (IcarusSim icarus) = runSim icarus
    runSimWithFile (IcarusSim icarus) = runSimWithFile icarus

instance Show SimTool where
    show (IcarusSim icarus) = show icarus

defaultIcarusSim :: SimTool
defaultIcarusSim = IcarusSim defaultIcarus

-- | The results from running a tool through a simulator. It can either fail or
-- return a result, which is most likely a 'ByteString'.
data SimResult = SimResult !SynthTool !SimTool !(BResult)
                 deriving (Eq)

instance Show SimResult where
    show (SimResult synth sim r) = show synth <> ", " <> show sim <> ": " <> show r

-- | The results of comparing the synthesised outputs of two files using a
-- formal equivalence checker. This will either return a failure or an output
-- which is most likely '()'.
data SynthResult = SynthResult !SynthTool !SynthTool !(UResult)
                   deriving (Eq)

instance Show SynthResult where
    show (SynthResult synth synth2 r) = show synth <> ", " <> show synth2 <> ": " <> show r

-- | The status of the synthesis using a simulator. This will be checked before
-- attempting to run the equivalence checks on the simulator, as that would be
-- unnecessary otherwise.
data SynthStatus = SynthStatus !SynthTool !(UResult)
                 deriving (Eq)

instance Show SynthStatus where
    show (SynthStatus synth r) = "synthesis " <> show synth <> ": " <> show r

-- | The complete state that will be used during fuzzing, which contains the
-- results from all the operations.
data FuzzReport = FuzzReport { _synthResults :: ![SynthResult]
                             , _simResults   :: ![SimResult]
                             , _synthStatus  :: ![SynthStatus]
                             }
                  deriving (Eq, Show)

$(makeLenses ''FuzzReport)

instance Semigroup FuzzReport where
    FuzzReport a1 b1 c1 <> FuzzReport a2 b2 c2 = FuzzReport (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid FuzzReport where
    mempty = FuzzReport [] [] []

descriptionToSim :: SimDescription -> SimTool
descriptionToSim (SimDescription "icarus") = defaultIcarusSim
descriptionToSim s =
    error $ "Could not find implementation for simulator '" <> show s <> "'"

-- | Convert a description to a synthesiser.
descriptionToSynth :: SynthDescription -> SynthTool
descriptionToSynth (SynthDescription "yosys" bin desc out) =
    YosysSynth
        . Yosys (fromText <$> bin) (fromMaybe (yosysDesc defaultYosys) desc)
        $ maybe (yosysOutput defaultYosys) fromText out
descriptionToSynth (SynthDescription "vivado" bin desc out) =
    VivadoSynth
        . Vivado (fromText <$> bin) (fromMaybe (vivadoDesc defaultVivado) desc)
        $ maybe (vivadoOutput defaultVivado) fromText out
descriptionToSynth (SynthDescription "xst" bin desc out) =
    XSTSynth
        . XST (fromText <$> bin) (fromMaybe (xstDesc defaultXST) desc)
        $ maybe (xstOutput defaultXST) fromText out
descriptionToSynth (SynthDescription "quartus" bin desc out) =
    QuartusSynth
        . Quartus (fromText <$> bin) (fromMaybe (quartusDesc defaultQuartus) $ desc)
        $ maybe (quartusOutput defaultQuartus) fromText out
descriptionToSynth (SynthDescription "identity" _ desc out) =
    IdentitySynth
        . Identity (fromMaybe (identityDesc defaultIdentity) $ desc)
        $ maybe (identityOutput defaultIdentity) fromText out
descriptionToSynth s =
    error $ "Could not find implementation for synthesiser '" <> show s <> "'"
