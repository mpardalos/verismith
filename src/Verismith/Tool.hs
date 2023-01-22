{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Verismith.Tool
-- Description : Simulator implementations.
-- Copyright   : (c) 2019, Yann Herklotz Grave
-- License     : GPL-3
-- Maintainer  : yann [at] yannherklotz [dot] com
-- Stability   : experimental
-- Portability : POSIX
--
-- Simulator implementations.
module Verismith.Tool
  ( -- * Simulation
    SimTool (..),
    runSim,
    runSimWithFile,
    runSimIc,
    runSimIcEMI,
    runSimIcEC,

    -- ** Simulators
    defaultIcarus,

    -- * Synthesis
    SynthTool (..),
    SynthToolType (..),
    runSynth,
    logger,

    -- ** Synthesisers
    defaultYosys,
    defaultVivado,
    defaultXST,
    defaultQuartus,
    defaultQuartusLight,
    defaultIdentity,

    -- * Equivalence
    runEquiv,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Lens ((^.), (^..))
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Binary (encode)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char (digitToInt)
import Data.Either (fromRight)
import Data.Foldable (fold)
import Data.List (transpose)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Numeric (readInt)
import Shelly as S
import Shelly.Lifted (liftSh)
import Verismith.CounterEg
import Verismith.Result (Result (..), ResultT (..))
import Verismith.Tool.Internal
import Verismith.Verilog (SourceInfo)
import Verismith.Verilog.AST
import Verismith.Verilog.BitVec (BitVecF (BitVec))
import Verismith.Verilog.CodeGen
import Verismith.Verilog.Internal
import Verismith.Verilog.Mutate (fromPort, initMod, instantiateMod, makeTopAssert)
import Prelude hiding (FilePath)

data SynthToolType
  = XST
  | Vivado
  | Yosys
  | Quartus
  | QuartusLight
  | IdentitySynth
  deriving (Eq, Generic, NFData)

data SynthTool = SynthTool
  { synthType :: !SynthToolType,
    synthBin :: !(Maybe FilePath),
    synthDesc :: !Text,
    synthOutput :: !FilePath
  }
  deriving (Eq, Generic, NFData)

instance Show SynthTool where
  show = T.unpack . synthDesc

defaultIdentity :: SynthTool
defaultIdentity = SynthTool IdentitySynth Nothing "identity" "syn_identity.v"

defaultXST :: SynthTool
defaultXST = SynthTool XST Nothing "xst" "syn_xst.v"

defaultYosys :: SynthTool
defaultYosys = SynthTool Yosys Nothing "yosys" "syn_yosys.v"

defaultVivado :: SynthTool
defaultVivado = SynthTool Vivado Nothing "vivado" "syn_vivado.v"

defaultQuartusLight :: SynthTool
defaultQuartusLight = SynthTool QuartusLight Nothing "quartus" "syn_quartus.v"

defaultQuartus :: SynthTool
defaultQuartus = SynthTool Quartus Nothing "quartus" "syn_quartus.v"

runSynth :: Show ann => SynthTool -> SourceInfo ann -> ResultSh ()
runSynth synth (SourceInfo top src) = case synthType synth of
  XST -> do
    dir <- liftSh pwd
    let exec n =
          execute_
            SynthFail
            dir
            "xst"
            (maybe (fromText n) (</> fromText n) $ synthBin synth)
    liftSh $ do
      writefile xstFile $ xstSynthConfig top
      writefile prjFile "verilog work \"rtl.v\""
      writefile "rtl.v" $ genSource src
    exec "xst" ["-ifn", toTextIgnore xstFile]
    exec
      "netgen"
      [ "-w",
        "-ofmt",
        "verilog",
        toTextIgnore $ modFile <.> "ngc",
        toTextIgnore $ synthOutput synth
      ]
    liftSh . noPrint $
      run_
        "sed"
        [ "-i",
          "/^`ifndef/,/^`endif/ d; s/ *Timestamp: .*//;",
          toTextIgnore $ synthOutput synth
        ]
    where
      modFile = fromText top
      xstFile = modFile <.> "xst"
      prjFile = modFile <.> "prj"
  Vivado -> do
    dir <- liftSh pwd
    liftSh $ do
      writefile vivadoTcl . vivadoSynthConfig top . toTextIgnore $
        synthOutput
          synth
      writefile "rtl.v" $ genSource src
      run_
        "sed"
        [ "s/^module/(* use_dsp48=\"no\" *) (* use_dsp=\"no\" *) module/;",
          "-i",
          "rtl.v"
        ]
    let exec_ n =
          execute_
            SynthFail
            dir
            "vivado"
            (maybe (fromText n) (</> fromText n) $ synthBin synth)
    exec_ "vivado" ["-mode", "batch", "-source", toTextIgnore vivadoTcl]
    where
      vivadoTcl = fromText ("vivado_" <> top) <.> "tcl"
  Yosys -> do
    dir <- liftSh $ do
      dir' <- S.pwd
      S.writefile inpf $ genSource src
      return dir'
    execute_
      SynthFail
      dir
      "yosys"
      yosysPath
      [ "-p",
        "read_verilog " <> inp <> "; synth; write_verilog -noattr " <> out
      ]
    where
      inpf = "rtl.v"
      inp = S.toTextIgnore inpf
      out = S.toTextIgnore $ synthOutput synth
      yosysPath = maybe (S.fromText "yosys") (</> S.fromText "yosys") $ synthBin synth
  Quartus -> do
    dir <- liftSh pwd
    let ex = execute_ SynthFail dir "quartus"
    liftSh $ do
      writefile inpf $ genSource src
      noPrint $
        run_
          "sed"
          [ "-i",
            "s/^module/(* multstyle = \"logic\" *) module/;",
            toTextIgnore inpf
          ]
      writefile quartusSdc "create_clock -period 5 -name clk [get_ports clock]"
      writefile quartusTcl (quartusSynthConfig quartusSdc top inpf)
    ex (exec "quartus_sh") ["-t", toTextIgnore quartusTcl]
    liftSh $ do
      cp (fromText "simulation/vcs" </> fromText top <.> "vo") $
        synthOutput synth
      run_
        "sed"
        [ "-ri",
          "s,^// DATE.*,,; s,^tri1 (.*);,wire \\1 = 1;,; /^\\/\\/ +synopsys/ d;",
          toTextIgnore $ synthOutput synth
        ]
    where
      inpf = "rtl.v"
      exec s = maybe (fromText s) (</> fromText s) $ synthBin synth
      quartusTcl = fromText top <.> "tcl"
      quartusSdc = fromText top <.> "sdc"
  QuartusLight -> do
    dir <- liftSh pwd
    let ex = execute_ SynthFail dir "quartus"
    liftSh $ do
      writefile inpf $ genSource src
      noPrint $
        run_
          "sed"
          [ "-i",
            "s/^module/(* multstyle = \"logic\" *) module/;",
            toTextIgnore inpf
          ]
      writefile quartusSdc "create_clock -period 5 -name clk [get_ports clock]"
      writefile quartusTcl $ quartusLightSynthConfig quartusSdc top inpf
    ex (exec "quartus_sh") ["-t", toTextIgnore quartusTcl]
    liftSh $ do
      cp (fromText "simulation/vcs" </> fromText top <.> "vo") $
        synthOutput synth
      run_
        "sed"
        [ "-ri",
          "s,^// DATE.*,,; s,^tri1 (.*);,wire \\1 = 1;,; /^\\/\\/ +synopsys/ d;",
          toTextIgnore $ synthOutput synth
        ]
    where
      inpf = "rtl.v"
      exec s = maybe (fromText s) (</> fromText s) $ synthBin synth
      quartusTcl = fromText top <.> "tcl"
      quartusSdc = fromText top <.> "sdc"
  IdentitySynth -> liftSh $ writefile (synthOutput synth) (genSource $ SourceInfo top src)

runEquiv ::
  Show ann =>
  Maybe Text ->
  FilePath ->
  SynthTool ->
  SynthTool ->
  SourceInfo ann ->
  ResultSh ()
runEquiv mt datadir sim1 sim2 srcInfo = do
  dir <- liftSh S.pwd
  liftSh $ do
    S.writefile "top.v"
      . genSource
      . initMod
      . makeTopAssert
      $ srcInfo
        ^. mainModule
    replaceMods (synthOutput sim1) "_1" srcInfo
    replaceMods (synthOutput sim2) "_2" srcInfo
    S.writefile "proof.sby" $ sbyConfig mt datadir sim1 sim2 srcInfo
  e <- liftSh $ do
    exe dir "symbiyosys" "sby" ["-f", "proof.sby"]
    S.lastExitCode
  case e of
    0 -> ResultT . return $ Pass ()
    2 -> case mt of
      Nothing -> ResultT . return . Fail $ EquivFail Nothing
      Just _ ->
        ResultT $
          Fail
            . EquivFail
            . Just
            . fromRight mempty
            . parseCounterEg
            <$> readfile "proof/engine_0/trace.smtc"
    124 -> ResultT . return $ Fail TimeoutError
    _ -> ResultT . return $ Fail EquivError
  where
    exe dir name e = void . S.errExit False . logCommand dir name . timeout e

rename :: Text -> [Text] -> Text
rename end entries =
  T.intercalate "\n" $
    flip mappend end
      . mappend "rename "
      . doubleName
      <$> entries
{-# INLINE rename #-}

doubleName :: Text -> Text
doubleName n = n <> " " <> n
{-# INLINE doubleName #-}

outputText :: SynthTool -> Text
outputText = toTextIgnore . synthOutput

quartusLightSynthConfig :: FilePath -> Text -> FilePath -> Text
quartusLightSynthConfig sdc top fp =
  T.unlines
    [ "load_package flow",
      "",
      "project_new -overwrite " <> top,
      "",
      "set_global_assignment -name FAMILY \"Cyclone V\"",
      "set_global_assignment -name SYSTEMVERILOG_FILE " <> toTextIgnore fp,
      "set_global_assignment -name TOP_LEVEL_ENTITY " <> top,
      "set_global_assignment -name SDC_FILE " <> toTextIgnore sdc,
      "set_global_assignment -name INI_VARS \"qatm_force_vqm=on;\"",
      "set_global_assignment -name NUM_PARALLEL_PROCESSORS 2",
      "set_instance_assignment -name VIRTUAL_PIN ON -to *",
      "",
      "execute_module -tool map",
      "execute_module -tool fit",
      "execute_module -tool sta -args \"--mode=implement\"",
      "execute_module -tool eda -args \"--simulation --tool=vcs\"",
      "",
      "project_close"
    ]

quartusSynthConfig :: FilePath -> Text -> FilePath -> Text
quartusSynthConfig sdc top fp =
  T.unlines
    [ "load_package flow",
      "",
      "project_new -overwrite " <> top,
      "",
      "set_global_assignment -name FAMILY \"Cyclone 10 GX\"",
      "set_global_assignment -name SYSTEMVERILOG_FILE " <> toTextIgnore fp,
      "set_global_assignment -name TOP_LEVEL_ENTITY " <> top,
      "set_global_assignment -name SDC_FILE " <> toTextIgnore sdc,
      "set_global_assignment -name INI_VARS \"qatm_force_vqm=on;\"",
      "set_global_assignment -name NUM_PARALLEL_PROCESSORS 2",
      "set_instance_assignment -name VIRTUAL_PIN ON -to *",
      "",
      "execute_module -tool syn",
      "execute_module -tool eda -args \"--simulation --tool=vcs\"",
      "",
      "project_close"
    ]

xstSynthConfig :: Text -> Text
xstSynthConfig top =
  T.unlines
    [ "run",
      "-ifn " <> top <> ".prj -ofn " <> top <> " -p artix7 -top " <> top,
      "-iobuf NO -ram_extract NO -rom_extract NO -use_dsp48 NO",
      "-fsm_extract YES -fsm_encoding Auto",
      "-change_error_to_warning \"HDLCompiler:226 HDLCompiler:1832\""
    ]

vivadoSynthConfig :: Text -> Text -> Text
vivadoSynthConfig top outf =
  T.unlines
    [ "# CRITICAL WARNING: [Synth 8-5821] Potential divide by zero",
      "set_msg_config -id {Synth 8-5821} -new_severity {WARNING}",
      "",
      "read_verilog rtl.v",
      "synth_design -part xc7k70t -top " <> top,
      "write_verilog -force " <> outf
    ]

sbyConfig :: Maybe Text -> FilePath -> SynthTool -> SynthTool -> SourceInfo ann -> Text
sbyConfig mt datadir sim1 sim2 (SourceInfo top _) =
  T.unlines
    [ "[options]",
      "multiclock on",
      "mode prove",
      "aigsmt " <> fromMaybe "none" mt,
      "",
      "[engines]",
      "abc pdr",
      "",
      "[script]",
      readL,
      "read -formal " <> outputText sim1,
      "read -formal " <> outputText sim2,
      "read -formal top.v",
      "prep -top " <> top,
      "",
      "[files]",
      depList,
      outputText sim2,
      outputText sim1,
      "top.v"
    ]
  where
    deps = ["cells_cmos.v", "cells_cyclone_v.v", "cells_verific.v", "cells_xilinx_7.v", "cells_yosys.v"]
    depList =
      T.intercalate "\n" $
        toTextIgnore
          . (datadir </> fromText "data" </>)
          . fromText
          <$> deps
    readL = T.intercalate "\n" $ mappend "read -formal " <$> deps

icarusTestbench :: Show ann => FilePath -> Verilog ann -> SynthTool -> Text
icarusTestbench datadir t synth1 =
  T.unlines
    [ "`include \"" <> ddir <> "/data/cells_cmos.v\"",
      "`include \"" <> ddir <> "/data/cells_cyclone_v.v\"",
      "`include \"" <> ddir <> "/data/cells_verific.v\"",
      "`include \"" <> ddir <> "/data/cells_xilinx_7.v\"",
      "`include \"" <> ddir <> "/data/cells_yosys.v\"",
      "`include \"" <> toTextIgnore (synthOutput synth1) <> "\"",
      "",
      genSource t
    ]
  where
    ddir = toTextIgnore datadir

-- Simulators

data SimTool = Icarus
  { icarusPath :: FilePath,
    vvpPath :: FilePath
  }
  deriving (Eq, Generic, NFData)

instance Show SimTool where
  show (Icarus _ _) = "iverilog"

defaultIcarus :: SimTool
defaultIcarus = Icarus "iverilog" "vvp"

addDisplay :: [Statement ann] -> [Statement ann]
addDisplay s =
  concat $
    transpose
      [ s,
        replicate l $ TimeCtrl 1 Nothing,
        replicate l . SysTaskEnable $ Task "display" ["%b", Id "y"]
      ]
  where
    l = length s

assignFunc :: [Port] -> ByteString -> Statement ann
assignFunc inp bs =
  NonBlockAssign
    . Assign conc Nothing
    . Number
    . BitVec (B.length bs * 8)
    $ bsToI bs
  where
    conc = RegConcat (portToExpr <$> inp)

convert :: Text -> ByteString
convert =
  B.toStrict
    . (encode :: Integer -> L.ByteString)
    . maybe 0 fst
    . listToMaybe
    . readInt 2 (`elem` ("01" :: String)) digitToInt
    . T.unpack

mask :: Text -> Text
mask = T.replace "x" "0"

callback :: ByteString -> Text -> ByteString
callback b t = b <> convert (mask t)

fromBytes :: ByteString -> Integer
fromBytes = B.foldl' f 0 where f a b = a `shiftL` 8 .|. fromIntegral b

tbModule :: [ByteString] -> ModDecl ann -> Verilog ann
tbModule bss top =
  Verilog
    [ instantiateMod top $
        ModDecl
          "testbench"
          []
          []
          [ Initial $
              fold
                [ BlockAssign (Assign "clk" Nothing 0),
                  BlockAssign (Assign inConcat Nothing 0)
                ]
                <> foldMap
                  ( ( \r ->
                        TimeCtrl
                          10
                          (Just $ BlockAssign (Assign inConcat Nothing r))
                    )
                      . fromInteger
                      . fromBytes
                  )
                  bss
                <> (TimeCtrl 10 . Just . SysTaskEnable $ Task "finish" []),
            Always . TimeCtrl 5 . Just $
              BlockAssign
                (Assign "clk" Nothing (UnOp UnNot (Id "clk"))),
            Always . EventCtrl (EPosEdge "clk") . Just . SysTaskEnable $
              Task "strobe" ["%b", Id "y"]
          ]
          []
    ]
  where
    inConcat = RegConcat . filter (/= Id "clk") $ (Id . fromPort <$> (top ^. modInPorts))

tbModule' :: [Identifier] -> [ByteString] -> ModDecl ann -> Verilog ann
tbModule' ids bss top =
  Verilog
    [ instantiateMod top $
        ModDecl
          "testbench"
          []
          []
          [ Initial $
              fold
                [ BlockAssign (Assign "clk" Nothing 0),
                  BlockAssign (Assign inConcat Nothing 0),
                  if null ids then mempty else BlockAssign (Assign inIds Nothing 0)
                ]
                <> foldMap
                  ( ( \r ->
                        TimeCtrl
                          10
                          (Just $ BlockAssign (Assign inConcat Nothing r))
                    )
                      . fromInteger
                      . fromBytes
                  )
                  bss
                <> (TimeCtrl 10 . Just . SysTaskEnable $ Task "finish" []),
            Always . TimeCtrl 5 . Just $
              BlockAssign
                (Assign "clk" Nothing (UnOp UnNot (Id "clk"))),
            Always . EventCtrl (EPosEdge "clk") . Just . SysTaskEnable $
              Task "strobe" ["%b", Concat (NE.fromList $ fmap Id outputs)]
          ]
          []
    ]
  where
    inConcat =
      RegConcat
        . filter (flip notElem $ fmap Id ids)
        . filter (/= Id "clk")
        $ (Id . fromPort <$> (top ^. modInPorts))
    inIds = RegConcat $ fmap Id ids
    outputs = top ^.. modOutPorts . traverse . portName

counterTestBench :: CounterEg -> ModDecl ann -> Verilog ann
counterTestBench (CounterEg _ states) = tbModule filtered
  where
    filtered = convert . foldMap snd . filter ((/= "clk") . fst) <$> states

runSimIc' ::
  Show ann =>
  ([ByteString] -> ModDecl ann -> Verilog ann) ->
  FilePath ->
  SimTool ->
  SynthTool ->
  SourceInfo ann ->
  [ByteString] ->
  Maybe ByteString ->
  ResultSh ByteString
runSimIc' fun datadir sim1 synth1 srcInfo bss bs = do
  dir <- liftSh pwd
  let top = srcInfo ^. mainModule
  let tb = fun bss top
  liftSh . writefile tbname $ icarusTestbench datadir tb synth1
  liftSh $ exe dir "icarus" "iverilog" ["-o", exename, toTextIgnore tbname]
  s <-
    liftSh $
      B.take 8
        . BA.convert
        . (hash :: ByteString -> Digest SHA256)
        <$> logCommand
          dir
          "vvp"
          ( runFoldLines
              (mempty :: ByteString)
              callback
              (vvpPath sim1)
              [exename]
          )
  case (bs, s) of
    (Nothing, s') -> ResultT . return $ Pass s'
    (Just bs', s') ->
      if bs' == s'
        then ResultT . return $ Pass s'
        else ResultT . return $ Fail (SimFail s')
  where
    exe dir name e = void . errExit False . logCommand dir name . timeout e
    tbname = fromText $ synthDesc synth1 <> "_testbench.v"
    exename = synthDesc synth1 <> "_main"

runSimIc ::
  Show ann =>
  -- | Data directory.
  FilePath ->
  -- | Simulator.
  SimTool ->
  -- | Synthesis tool to be tested.
  SynthTool ->
  -- | Original generated program to test.
  SourceInfo ann ->
  -- | Test vectors to be passed as inputs to the generated Verilog.
  [ByteString] ->
  -- | What the correct output should be. If 'Nothing' is passed, then just return 'Pass ByteString'
  -- with the answer.
  Maybe ByteString ->
  ResultSh ByteString
runSimIc = runSimIc' tbModule

runSimIcEMI ::
  Show ann =>
  -- | EMI Ids
  [Identifier] ->
  -- | Data directory.
  FilePath ->
  -- | Simulator.
  SimTool ->
  -- | Synthesis tool to be tested.
  SynthTool ->
  -- | Original generated program to test.
  SourceInfo ann ->
  -- | Test vectors to be passed as inputs to the generated Verilog.
  [ByteString] ->
  -- | What the correct output should be. If 'Nothing' is passed, then just return 'Pass ByteString'
  -- with the answer.
  Maybe ByteString ->
  ResultSh ByteString
runSimIcEMI ids = runSimIc' (tbModule' ids)

runSimIcEC ::
  Show ann =>
  FilePath ->
  SimTool ->
  SynthTool ->
  SourceInfo ann ->
  CounterEg ->
  Maybe ByteString ->
  ResultSh ByteString
runSimIcEC a b c d e = runSimIc' (const $ counterTestBench e) a b c d []

runSim :: Show ann => SimTool -> SourceInfo ann -> [ByteString] -> ResultSh ByteString
runSim sim rinfo bss = do
  let tb =
        ModDecl
          "main"
          []
          []
          [ Initial $
              fold (addDisplay $ assignFunc (_modInPorts m) <$> bss)
                <> (SysTaskEnable $ Task "finish" [])
          ]
          []
  let newtb = instantiateMod m tb
  let modWithTb = Verilog [newtb, m]
  liftSh . writefile "main.v" $ genSource modWithTb
  annotate (SimFail mempty) $ runSimWithFile sim "main.v" bss
  where
    m = rinfo ^. mainModule

runSimWithFile :: SimTool -> FilePath -> [ByteString] -> ResultSh ByteString
runSimWithFile sim f _ = annotate (SimFail mempty) . liftSh $ do
  dir <- pwd
  logCommand_ dir "icarus" $
    run (icarusPath sim) ["-o", "main", toTextIgnore f]
  B.take 8 . BA.convert . (hash :: ByteString -> Digest SHA256)
    <$> logCommand
      dir
      "vvp"
      (runFoldLines (mempty :: ByteString) callback (vvpPath sim) ["main"])
