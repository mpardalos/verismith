{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Verismith.Report
-- Description : Generate a report from a fuzz run.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generate a report from a fuzz run.
module Verismith.Report
  ( SynthStatus (..),
    SynthResult (..),
    SimResult (..),
    FuzzReport (..),
    printResultReport,
    printSummary,
    synthResults,
    simResults,
    synthStatus,
    equivTime,
    fuzzDir,
    fileLines,
    reducTime,
    synthTime,
    descriptionToSim,
    descriptionToSynth,
  )
where

import Control.DeepSeq (NFData, rnf)
import Control.Lens hiding (Identity, (<.>))
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Time
import Shelly
  ( FilePath,
    fromText,
    toTextIgnore,
    (<.>),
    (</>),
  )
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Verismith.Config
import Verismith.Internal
import Verismith.Result
import Verismith.Tool
import Verismith.Tool.Internal
import Prelude hiding (FilePath)

-- | Common type alias for synthesis results
type UResult = Result Failed ()

-- | Commont type alias for simulation results
type BResult = Result Failed ByteString

-- | The results from running a tool through a simulator. It can either fail or
-- return a result, which is most likely a 'ByteString'.
data SimResult = SimResult !SynthTool !SimTool ![ByteString] !BResult !NominalDiffTime
  deriving (Eq)

instance Show SimResult where
  show (SimResult synth sim _ r d) = show synth <> ", " <> show sim <> ": " <> show (bimap show (T.unpack . showBS) r) <> " (" <> show d <> ")"

getSimResult :: SimResult -> UResult
getSimResult (SimResult _ _ _ (Pass _) _) = Pass ()
getSimResult (SimResult _ _ _ (Fail b) _) = Fail b

-- | The results of comparing the synthesised outputs of two files using a
-- formal equivalence checker. This will either return a failure or an output
-- which is most likely '()'.
data SynthResult = SynthResult !SynthTool !SynthTool !UResult !NominalDiffTime
  deriving (Eq)

instance Show SynthResult where
  show (SynthResult synth synth2 r d) = show synth <> ", " <> show synth2 <> ": " <> show r <> " (" <> show d <> ")"

getSynthResult :: SynthResult -> UResult
getSynthResult (SynthResult _ _ a _) = a

-- | The status of the synthesis using a simulator. This will be checked before
-- attempting to run the equivalence checks on the simulator, as that would be
-- unnecessary otherwise.
data SynthStatus = SynthStatus !SynthTool !UResult !NominalDiffTime
  deriving (Eq)

getSynthStatus :: SynthStatus -> UResult
getSynthStatus (SynthStatus _ a _) = a

instance Show SynthStatus where
  show (SynthStatus synth r d) = "synthesis " <> show synth <> ": " <> show r <> " (" <> show d <> ")"

-- | The complete state that will be used during fuzzing, which contains the
-- results from all the operations.
data FuzzReport = FuzzReport
  { _fuzzDir :: !FilePath,
    -- | Results of the equivalence check.
    _synthResults :: ![SynthResult],
    -- | Results of the simulation.
    _simResults :: ![SimResult],
    -- | Results of the synthesis step.
    _synthStatus :: ![SynthStatus],
    _fileLines :: {-# UNPACK #-} !Int,
    _synthTime :: !NominalDiffTime,
    _equivTime :: !NominalDiffTime,
    _reducTime :: !NominalDiffTime
  }
  deriving (Eq, Show)

$(makeLenses ''FuzzReport)

descriptionToSim :: SimDescription -> SimTool
descriptionToSim (SimDescription "icarus") = defaultIcarus
descriptionToSim s =
  error $ "Could not find implementation for simulator '" <> show s <> "'"

-- | Convert a description to a synthesiser.
descriptionToSynth :: SynthDescription -> SynthTool
descriptionToSynth (SynthDescription name bin desc out) =
  defaultTool
    { synthOutput = maybe (synthOutput defaultTool) fromText out,
      Verismith.Tool.synthBin = fromText <$> bin,
      Verismith.Tool.synthDesc = fromMaybe (Verismith.Tool.synthDesc defaultTool) desc
    }
  where
    defaultTool :: SynthTool
    defaultTool = case name of
      "vivado" -> defaultVivado
      "yosys" -> defaultYosys
      "xst" -> defaultXST
      "quartus" -> defaultQuartus
      "quartusLight" -> defaultQuartusLight
      "identity" -> defaultIdentity
      _ -> error $ "Could not find implementation for synthesiser '" <> show name <> "'"

status :: Result Failed () -> Html
status (Pass _) = H.td ! A.class_ "is-success" $ "Passed"
status (Fail EmptyFail) = H.td ! A.class_ "is-danger" $ "Failed"
status (Fail (EquivFail _)) = H.td ! A.class_ "is-danger" $ "Equivalence failed"
status (Fail (SimFail _)) = H.td ! A.class_ "is-danger" $ "Simulation failed"
status (Fail SynthFail) = H.td ! A.class_ "is-danger" $ "Synthesis failed"
status (Fail EquivError) = H.td ! A.class_ "is-danger" $ "Equivalence error"
status (Fail TimeoutError) = H.td ! A.class_ "is-warning" $ "Time out"

synthStatusHtml :: SynthStatus -> Html
synthStatusHtml (SynthStatus synth res diff) = H.tr $ do
  H.td . H.toHtml $ Verismith.Tool.synthDesc synth
  status res
  H.td . H.toHtml $ showT diff

synthResultHtml :: SynthResult -> Html
synthResultHtml (SynthResult synth1 synth2 res diff) = H.tr $ do
  H.td . H.toHtml $ Verismith.Tool.synthDesc synth1
  H.td . H.toHtml $ Verismith.Tool.synthDesc synth2
  status res
  H.td . H.toHtml $ showT diff

resultHead :: Text -> Html
resultHead name = H.head $ do
  H.title $ "Fuzz Report - " <> H.toHtml name
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.meta ! A.charset "utf8"
  H.link
    ! A.rel "stylesheet"
    ! A.href
      "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css"

resultReport :: Text -> FuzzReport -> Html
resultReport name (FuzzReport _ synth _ stat _ _ _ _) = H.docTypeHtml $ do
  resultHead name
  H.body
    . (H.section ! A.class_ "section")
    . (H.div ! A.class_ "container")
    $ do
      H.h1 ! A.class_ "title is-1" $ "Fuzz Report - " <> H.toHtml name
      H.h2 ! A.class_ "title is-2" $ "Synthesis"
      H.table ! A.class_ "table" $ do
        H.thead
          . H.toHtml
          $ ( H.tr
                . H.toHtml
                $ [H.th "Tool", H.th "Status", H.th "Run time"]
            )
        H.tbody . H.toHtml $ fmap synthStatusHtml stat
      H.h2 ! A.class_ "title is-2" $ "Equivalence Check"
      H.table ! A.class_ "table" $ do
        H.thead
          . H.toHtml
          $ ( H.tr
                . H.toHtml
                $ [ H.th "First tool",
                    H.th "Second tool",
                    H.th "Status",
                    H.th "Run time"
                  ]
            )
        H.tbody . H.toHtml $ fmap synthResultHtml synth

resultStatus :: Result a b -> Html
resultStatus (Pass _) = H.td ! A.class_ "is-success" $ "Passed"
resultStatus (Fail _) = H.td ! A.class_ "is-danger" $ "Failed"

meanVariance :: [Double] -> (Double, Double)
meanVariance l = (mean, variance)
  where
    mean = sum l / len
    variance = sum (squ . subtract mean <$> l) / (len - 1.0)
    squ x = x * x
    len = fromIntegral $ length l

fuzzStats ::
  (Real a1, Traversable t) =>
  ((a1 -> Const (Endo [a1]) a1) -> a2 -> Const (Endo [a1]) a2) ->
  t a2 ->
  (Double, Double)
fuzzStats sel fr = meanVariance converted
  where
    converted = fmap realToFrac $ fr ^.. traverse . sel

fuzzStatus :: Text -> FuzzReport -> Html
fuzzStatus name (FuzzReport dir s1 s2 s3 sz t1 t2 t3) = H.tr $ do
  H.td
    . ( H.a
          ! A.href
            ( H.textValue $
                toTextIgnore (dir <.> "html")
            )
      )
    $ H.toHtml name
  resultStatus $
    mconcat (fmap getSynthResult s1)
      <> mconcat (fmap getSimResult s2)
      <> mconcat (fmap getSynthStatus s3)
  H.td . H.string $ show sz
  H.td . H.string $ show t1
  H.td . H.string $ show t2
  H.td . H.string $ show t3

summary :: Text -> [FuzzReport] -> Html
summary name fuzz = H.docTypeHtml $ do
  resultHead name
  H.body
    . (H.section ! A.class_ "section")
    . (H.div ! A.class_ "container")
    $ do
      H.h1 ! A.class_ "title is-1" $ "FuzzReport - " <> H.toHtml name
      H.table ! A.class_ "table" $ do
        H.thead . H.tr $
          H.toHtml
            [ H.th "Name",
              H.th "Status",
              H.th "Size (loc)",
              H.th "Synthesis time",
              H.th "Equivalence check time",
              H.th "Reduction time"
            ]
        H.tbody
          . H.toHtml
          . fmap
            ( \(i, r) ->
                fuzzStatus ("Fuzz " <> showT (i :: Int)) r
            )
          $ zip [1 ..] fuzz
        H.tfoot . H.toHtml $ do
          H.tr $
            H.toHtml
              [ H.td $ H.strong "Total",
                H.td mempty,
                H.td
                  . H.string
                  . show
                  . sum
                  $ fuzz
                    ^.. traverse
                      . fileLines,
                sumUp synthTime,
                sumUp equivTime,
                sumUp reducTime
              ]
          H.tr $
            H.toHtml
              [ H.td $ H.strong "Mean",
                H.td mempty,
                fst $ bimap d2I d2I $ fuzzStats fileLines fuzz,
                fst $ meanVar synthTime,
                fst $ meanVar equivTime,
                fst $ meanVar reducTime
              ]
          H.tr $
            H.toHtml
              [ H.td $ H.strong "Variance",
                H.td mempty,
                snd $ bimap d2I d2I $ fuzzStats fileLines fuzz,
                snd $ meanVar synthTime,
                snd $ meanVar equivTime,
                snd $ meanVar reducTime
              ]
  where
    sumUp s = showHtml . sum $ fuzz ^.. traverse . s
    meanVar s = bimap d2T d2T $ fuzzStats s fuzz
    showHtml = H.td . H.string . show
    d2T = showHtml . (realToFrac :: Double -> NominalDiffTime)
    d2I = H.td . H.string . show

printResultReport :: Text -> FuzzReport -> Text
printResultReport t f = toStrict . renderHtml $ resultReport t f

printSummary :: Text -> [FuzzReport] -> Text
printSummary t f = toStrict . renderHtml $ summary t f
