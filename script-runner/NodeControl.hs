{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module NodeControl (NodeHandle, NodeControl.NodeType(..), startNode, stopNode, genSystemStart, mkTopo, keygen, NodeInfo(..)) where

import           Control.Concurrent.Async.Lifted.Safe
import           Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale,
                     formatTime, getCurrentTime)
import           Pos.Util.Wlog (logWarning)
import           System.Posix.Signals
import           System.Process
import           Universum hiding (on, state, when)
import Pos.Infra.Network.Yaml (Topology(..), NodeMetadata(..), NodeRegion(..), NodeRoutes(..), DnsDomains(..), AllStaticallyKnownPeers(..))
import Pos.Infra.Network.Types (NodeType(..), NodeName(..), Valency, Fallbacks)
import qualified Data.Map.Strict as M
import Pos.Infra.Network.DnsDomains (NodeAddr(..))
import qualified Data.Text as T
import           Data.Ix (range)
import Network.Broadcast.OutboundQueue (MaxBucketSize(BucketSizeUnlimited))
import Network.DNS.Types (Domain)
import           Pos.Launcher (cfoSystemStart_L, cfoFilePath_L, ConfigurationOptions)
import           Pos.Core.Slotting (Timestamp, getTimestamp)
import           Data.Time.Units (Second, convertUnit)
import qualified Pos.Client.CLI as CLI

data NodeHandle = NodeHandle (Async ()) ProcessHandle
data NodeInfo = NodeInfo
            { niIndex :: Integer
            , niType :: NodeControl.NodeType
            , stateRoot :: Text
            , topoPath :: Text
            , niCfgFilePath :: CLI.CommonArgs
            }
data NodeType = Core | Relay

startingPortOffset :: Num i => NodeControl.NodeType -> i
startingPortOffset Core = 100
startingPortOffset Relay = 0

mkTopo :: Integer -> Integer -> Topology
mkTopo cores relays = do
  let
    nmRegion = NodeRegion "none"
    nmSubscribe = DnsDomains []
    nmValency :: Valency
    nmValency = 1
    nmFallbacks :: Fallbacks
    nmFallbacks = 1
    nmKademlia = False
    nmPublicDNS = False
    nmMaxSubscrs = BucketSizeUnlimited
    mkNodeMeta :: Integer -> NodeControl.NodeType -> NodeMetadata
    mkNodeMeta idx typ = do
      let
        nmType = case typ of
          Core -> NodeCore
          Relay -> NodeRelay
        nmAddress :: NodeAddr (Maybe Domain)
        nmAddress = NodeAddrExact "127.0.0.1" (Just $ startingPortOffset typ + 3000 + (fromIntegral idx))
        mkRoute :: Integer -> [ NodeName ]
        mkRoute x = [ NodeName ("core-" <> show x) ]
        nmRoutes = case typ of
          Core -> NodeRoutes [ [ NodeName "relay-0" ] ]
          Relay -> NodeRoutes $ map mkRoute $ range (0,cores)
      NodeMetadata{..}
    mkCoreTup :: Integer -> (NodeName, NodeMetadata)
    mkCoreTup idx = (NodeName $ T.pack $ "core-" <> (show idx), mkNodeMeta idx Core)
    mkRelayTup idx = (NodeName $ T.pack $ "relay-" <> (show idx), mkNodeMeta idx Relay)
    allCoreNodes :: [ (NodeName, NodeMetadata) ]
    allCoreNodes = map mkCoreTup (range (0, cores))
    allRelayNodes :: [ (NodeName, NodeMetadata) ]
    allRelayNodes = map mkRelayTup (range (0, relays))
  TopologyStatic $ AllStaticallyKnownPeers $ M.fromList (allCoreNodes <> allRelayNodes)

typeToString :: NodeControl.NodeType -> String
typeToString Core = "core"
typeToString Relay = "relay"

commonNodeParams :: NodeInfo -> [ String ]
commonNodeParams (NodeInfo idx typ stateRoot topoPath cfg) = [
    "--configuration-file", cfg ^. CLI.configurationOptions_L . cfoFilePath_L
  , "--topology", T.unpack topoPath
  , "--db-path", (T.unpack stateRoot) <> "/poc-state/" <> (typeToString typ) <> (show idx) <> "-db"
  , "--node-id", (typeToString typ) <> "-" <> (show idx)
  , "--node-api-address", "127.0.0.1:" <> show (startingPortOffset typ + 8083 + idx)
  , "--no-tls"
  , "--node-doc-address", "127.0.0.1:" <> show (startingPortOffset typ + 8180 + idx)
  , "--json-log", "poc-state/" <> typeToString typ <> show idx <> ".json"
  , "--logs-prefix", "poc-state/logs-" <> typeToString typ <> show idx
  ] <> (maybeSystemStart $ cfg ^. CLI.configurationOptions_L . cfoSystemStart_L)
  <> (maybeLogConfig $ cfg ^. CLI.logConfig_L)

maybeSystemStart :: Maybe Timestamp -> [ String ]
maybeSystemStart Nothing = []
maybeSystemStart (Just ts) = [ "--system-start", show $ fromIntegral @Second (convertUnit $ getTimestamp ts) ]

maybeLogConfig :: Maybe FilePath -> [ String ]
maybeLogConfig Nothing = []
maybeLogConfig (Just logconfig) = [ "--log-config", logconfig ]

commonNodeStart :: String -> [ String ] -> NodeControl.NodeType -> Integer -> IO NodeHandle
commonNodeStart prog args typ idx = do
  childStdout <- openFile ("poc-state/" <> (typeToString typ) <> "-stdout-" <> show idx) WriteMode
  let
    pc :: CreateProcess
    pc = (proc prog args) { std_out = UseHandle childStdout }
  (_stdin, _stdout, _stderr, ph) <- createProcess pc
  later <- async $ do
    _ <- waitForProcess ph
    pure ()
  pure $ NodeHandle later ph

startNode :: NodeInfo -> IO NodeHandle
startNode info@(NodeInfo idx Core stateRoot _topoPath _cfg) = do
  let
    params = (commonNodeParams info) <>
             [ "--keyfile", T.unpack (stateRoot <> "/genesis-keys/generated-keys/rich/key" <> (show idx) <> ".sk")
             , "--listen", "127.0.0.1:" <> show (startingPortOffset Core + idx + 3000)
             ]
  commonNodeStart "cardano-node-simple" params Core idx
startNode info@(NodeInfo idx Relay stateRoot _topoPath _cfg) = do
  let
    params = (commonNodeParams info) <>
             [ "--keyfile", T.unpack (stateRoot <> "/relay" <> (show idx) <> ".sk")
             , "--listen", "127.0.0.1:" <> show (startingPortOffset Relay + idx + 3000)
             ]
  commonNodeStart "cardano-node-simple" params Relay idx

stopNode :: NodeHandle -> IO ()
stopNode (NodeHandle _async ph) = do
  maybePid <- getPid ph
  case maybePid of
    Just pid -> do
      signalProcess sigINT pid
    Nothing -> do
      logWarning "node already stopped when trying to stop it"

genSystemStart :: NominalDiffTime -> IO String
genSystemStart offset = formatTime defaultTimeLocale "%s" . addUTCTime offset <$> getCurrentTime

keygen :: ConfigurationOptions -> Text -> IO ()
keygen cfg stateRoot = do
  let
    params = [ "generate-keys-by-spec"
             , "--genesis-out-dir", T.unpack (stateRoot <> "/genesis-keys")
             , "--configuration-file", cfg ^. cfoFilePath_L
             ] <> (maybeSystemStart $ cfg ^. cfoSystemStart_L)
    pc :: CreateProcess
    pc = proc "cardano-keygen" params
  (_stdin, _stdout, _stderr, ph) <- createProcess pc
  _ <- waitForProcess ph
  pure ()
