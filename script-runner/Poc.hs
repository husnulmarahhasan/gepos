{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import           AutomatedTestRunner
import           BlockParser ()
import           Data.Constraint (Dict (Dict))
import           Data.Default (def)
import           Data.Ix (range)
import           Formatting (Format, int, sformat, (%))
import           NodeControl (NodeHandle, NodeType (..), genSystemStart,
                     startNode, stopNode, mkTopo, keygen, NodeInfo(..))
import           PocMode
import           Pos.Chain.Update (ApplicationName (ApplicationName),
                     BlockVersion (BlockVersion),
                     BlockVersionData (bvdMaxBlockSize, bvdMaxTxSize),
                     BlockVersionModifier(bvmMaxTxSize), SoftwareVersion (SoftwareVersion))
import           Pos.DB.Class (gsAdoptedBVData)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (HasConfigurations, cfoSystemStart_L)
import           Pos.Util.Wlog (logInfo)
import           Serokell.Data.Memory.Units (Byte)
import           Universum hiding (on)
import qualified Turtle as T
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Pos.Client.CLI as CLI
import           Pos.Core (Timestamp (..))
import           Prelude (read)
import           Data.Time.Units (fromMicroseconds)

printbvd :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
printbvd Dict _diffusion = do
  let
    bvdfmt :: Format r (Byte -> Byte -> r)
    bvdfmt = "BVD: max-tx: " %int% ", max-block: " %int
  bar <- gsAdoptedBVData
  print $ sformat bvdfmt (bvdMaxTxSize bar) (bvdMaxBlockSize bar)

test4 :: String -> Example ()
test4 stateDir = do
  genesisConfig <- getGenesisConfig
  let
    proposal1 :: Dict HasConfigurations -> Diffusion PocMode -> PocMode ()
    proposal1 Dict diffusion = do
      let
        keyIndex :: Int
        keyIndex = 0
        blockVersion = BlockVersion 0 0 0
        softwareVersion = SoftwareVersion (ApplicationName "cardano-sl") 1
        blockVersionModifier :: BlockVersionModifier
        blockVersionModifier = def -- { bvmMaxTxSize = Just 131072 }
      doUpdate diffusion genesisConfig keyIndex blockVersion softwareVersion blockVersionModifier
      print ("done?"::String)
  onStartup $ \Dict _diffusion -> loadNKeys stateDir 4
  on (1,10) proposal1
  on (2,10) $ \Dict _diffusion -> do
    endScript
  forM_ (range (0,20)) $ \epoch -> on(epoch, 0) printbvd

main :: IO ()
main = do
  systemStart <- genSystemStart 10
  let
    -- cores run from 0-3, relays run from 0-0
    topo = mkTopo 3 0
    systemStartTs :: Timestamp
    systemStartTs = Timestamp $ fromMicroseconds $ (read systemStart) * 1000000
    createNodes :: T.Text -> ScriptRunnerOptions -> IO ([NodeHandle], [NodeHandle])
    createNodes stateDir opts = do
      let
        path = stateDir <> "/topology.yaml"
        -- the config for the script-runner is mirrored to the nodes it starts
        cfg = opts ^. srCommonNodeArgs_L . CLI.commonArgs_L
      BSL.writeFile (T.unpack path) (A.encode topo)
      keygen (cfg ^. CLI.configurationOptions_L)  stateDir
      corenodes <- forM (range (0,3)) $ \node -> startNode (NodeInfo node Core stateDir path cfg)
      relays <- forM (range (0,0)) $ \node -> startNode (NodeInfo node Relay stateDir path cfg)
      pure (corenodes, relays)
    cleanupNodes :: ([NodeHandle],[NodeHandle]) -> IO ()
    cleanupNodes (corenodes, relays) = do
      logInfo "stopping all nodes"
      mapM_ stopNode corenodes
      mapM_ stopNode relays
    optionsMutator :: ScriptRunnerOptions -> IO ScriptRunnerOptions
    optionsMutator optsin = do
      -- sets the systemStart inside the ScriptRunnerOptions to the systemStart generated at the start of main
      return $ optsin
             & srCommonNodeArgs_L
             . CLI.commonArgs_L
             . CLI.configurationOptions_L
             . cfoSystemStart_L
             .~ Just systemStartTs
    scriptGetter :: HasEpochSlots => String -> ([NodeHandle], [NodeHandle]) -> IO Script
    scriptGetter stateDir (_cores, _relays) = return $ getScript $ test4 stateDir
    runScript' :: String -> IO ()
    runScript' stateDir = do
      runScript (createNodes $ T.pack stateDir) cleanupNodes optionsMutator (scriptGetter stateDir)
  T.with (T.mktempdir "/tmp" "script-runner") $ \stateDir -> do
    runScript' $ T.encodeString stateDir
