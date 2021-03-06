{-# LANGUAGE DataKinds #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , runQueryCmd
  ) where

import           Prelude (String)
import           Cardano.Prelude

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Numeric (showEFloat)

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT)

import           Cardano.Crypto.Hash.Class (getHashBytesAsHex)

import           Cardano.Api
                   (Address, LocalStateQueryError, Network(..), getLocalTip,
                    queryFilteredUTxOFromLocalState, queryPParamsFromLocalState,
                    queryStakeDistributionFromLocalState)

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath)
import           Cardano.CLI.Helpers
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))

import           Cardano.Config.Shelley.Protocol (mkNodeClientProtocolTPraos)

import           Ouroboros.Consensus.Cardano (protocolClientInfo)
import           Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosStandardCrypto)
import           Ouroboros.Network.NodeToClient (withIOManager)

import           Ouroboros.Network.Block (getTipPoint)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.TxData (TxId (..), TxIn (..), TxOut (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..))
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr(..))
import           Shelley.Spec.Ledger.Keys
                   (Hash, KeyHash(..), KeyRole (..), VerKeyVRF)


data ShelleyQueryCmdError
  = ShelleyQueryEnvVarSocketErr !EnvSocketError
  | NodeLocalStateQueryError !LocalStateQueryError
  | ShelleyHelperError !HelpersError
  deriving Show


runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters network mOutFile ->
      runQueryProtocolParameters network mOutFile
    QueryTip network ->
      runQueryTip network
    QueryFilteredUTxO addr network mOutFile ->
      runQueryFilteredUTxO addr network mOutFile
    QueryStakeDistribution network mOutFile ->
      runQueryStakeDistribution network mOutFile
    _ -> liftIO $ putStrLn $ "runQueryCmd: " ++ show cmd

runQueryProtocolParameters
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  pparams <- firstExceptT NodeLocalStateQueryError $
    queryPParamsFromLocalState network sockPath (getTipPoint tip)
  writeProtocolParameters mOutFile pparams

writeProtocolParameters :: Maybe OutputFile -> PParams -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolParameters mOutFile pparams =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyHelperError . IOError' fpath) $
        LBS.writeFile fpath (encodePretty pparams)

runQueryTip
  :: Network
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip network = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  liftIO $ putTextLn (show tip)

runQueryFilteredUTxO
  :: Address
  -> Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryFilteredUTxO addr network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  filteredUtxo <- firstExceptT NodeLocalStateQueryError $
    queryFilteredUTxOFromLocalState network sockPath (Set.singleton addr) (getTipPoint tip)
  writeFilteredUTxOs mOutFile filteredUtxo

writeFilteredUTxOs :: Maybe OutputFile -> UTxO TPraosStandardCrypto -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs utxo
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyHelperError . IOError' fpath) $ LBS.writeFile fpath (encodePretty utxo)

printFilteredUTxOs :: UTxO TPraosStandardCrypto -> IO ()
printFilteredUTxOs (UTxO utxo) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    mapM_ printUtxo $ Map.toList utxo
  where
    title :: Text
    title =
      "                           TxHash                                 TxIx        Lovelace"

    printUtxo :: (TxIn TPraosStandardCrypto, TxOut TPraosStandardCrypto) -> IO ()
    printUtxo (TxIn (TxId txhash) txin , TxOut _ (Coin coin)) =
      Text.putStrLn $
        mconcat
          [ Text.pack (show txhash)
          , textShowN 6 txin
          , textShowN 18 coin -- enough to display maxLovelaceVal
          ]

    textShowN :: Show a => Int -> a -> Text
    textShowN len x =
      let str = show x
          slen = length str
      in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

runQueryStakeDistribution
  :: Network
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution network mOutFile = do
  sockPath <- firstExceptT ShelleyQueryEnvVarSocketErr readEnvSocketPath
  let ptclClientInfo = pClientInfoCodecConfig . protocolClientInfo $ mkNodeClientProtocolTPraos
  tip <- liftIO $ withIOManager $ \iomgr ->
    getLocalTip iomgr ptclClientInfo network sockPath
  stakeDistr <- firstExceptT NodeLocalStateQueryError $
    queryStakeDistributionFromLocalState network sockPath (getTipPoint tip)
  writeStakeDistribution mOutFile stakeDistr

writeStakeDistribution :: Maybe OutputFile
                       -> PoolDistr TPraosStandardCrypto
                       -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) (PoolDistr stakeDistr) =
    handleIOExceptT (ShelleyHelperError . IOError' outFile) $
      LBS.writeFile outFile (encodePretty stakeDistr)

writeStakeDistribution Nothing stakeDistr =
    liftIO $ printStakeDistribution stakeDistr

printStakeDistribution :: PoolDistr TPraosStandardCrypto -> IO ()
printStakeDistribution (PoolDistr stakeDistr) = do
    Text.putStrLn title
    putStrLn $ replicate (Text.length title + 2) '-'
    sequence_
      [ putStrLn $ showStakeDistr poolId stakeFraction vrfKeyId
      | (poolId, (stakeFraction, vrfKeyId)) <- Map.toList stakeDistr ]
  where
    title :: Text
    title =
      "                           PoolId                                 Stake frac"

    showStakeDistr :: KeyHash 'StakePool crypto
                   -> Rational
                   -> Hash crypto (VerKeyVRF crypto)
                   -> String
    showStakeDistr (KeyHash poolId) stakeFraction _vrfKeyId =
      concat
        [ BS.unpack (getHashBytesAsHex poolId)
        , "   "
        , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
-- TODO: we could show the VRF id, but it will then not fit in 80 cols
--      , show vrfKeyId
        ]

