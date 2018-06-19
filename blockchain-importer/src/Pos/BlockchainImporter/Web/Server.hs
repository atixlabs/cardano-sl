{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- API server logic

module Pos.BlockchainImporter.Web.Server
       ( blockchainImporterServeImpl
       , blockchainImporterApp
       , blockchainImporterHandlers

       -- pure functions
       , getBlockDifficulty

       -- api functions
       , getBlocksTotal
       ) where

import           Universum

import           Control.Error.Util (exceptT, hoistEither)
import           Formatting (build, sformat, string, (%))
import           Network.Wai (Application)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Wlog (logError)

import           Servant.Generic (AsServerT, toServant)
import           Servant.Server (Server, ServerT, serve)

import           Pos.Crypto (hash, hashHexF)

import           Pos.Diffusion.Types (Diffusion (..))

import           Pos.Core (difficultyL, getChainDifficulty)
import           Pos.Core.Block (Block)
import           Pos.Core.Txp (taTx)
import           Pos.Txp (MonadTxpLocal, txpProcessTx, verifyTx)
import           Pos.Txp.DB.Utxo (getTxOut)
import           Pos.Web (serveImpl)

import           Pos.BlockchainImporter.Aeson.ClientTypes ()
import           Pos.BlockchainImporter.BlockchainImporterMode (BlockchainImporterMode)
import           Pos.BlockchainImporter.ExtraContext (HasBlockchainImporterCSLInterface (..))
import           Pos.BlockchainImporter.Web.Api (BlockchainImporterApi,
                                                 BlockchainImporterApiRecord (..),
                                                 blockchainImporterApi)
import           Pos.BlockchainImporter.Web.ClientTypes (CEncodedSTx (..), decodeSTx)
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError (..))



----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

blockchainImporterServeImpl
    :: BlockchainImporterMode ctx m
    => m Application
    -> Word16
    -> m ()
blockchainImporterServeImpl app port = serveImpl loggingApp "*" port Nothing Nothing Nothing
  where
    loggingApp = logStdoutDev <$> app

blockchainImporterApp :: BlockchainImporterMode ctx m => m (Server BlockchainImporterApi) -> m Application
blockchainImporterApp serv = serve blockchainImporterApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

blockchainImporterHandlers
    :: forall ctx m. (BlockchainImporterMode ctx m, MonadTxpLocal m)
    => Diffusion m -> ServerT BlockchainImporterApi m
blockchainImporterHandlers _diffusion =
    toServant (BlockchainImporterApiRecord
        { _blockCount         = getBlocksTotal
        , _sendSignedTx       = sendSignedTx(_diffusion)
        }
        :: BlockchainImporterApiRecord (AsServerT m))

----------------------------------------------------------------
-- API Functions
----------------------------------------------------------------

-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: BlockchainImporterMode ctx m
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- getTipBlockCSLI
    pure $ getBlockDifficulty tipBlock


sendSignedTx
     :: (BlockchainImporterMode ctx m, MonadTxpLocal m)
     => Diffusion m
     -> CEncodedSTx
     -> m ()
sendSignedTx Diffusion{..} encodedSTx@(CEncodedSTx bs) = do
  let bytesHash = hash bs
  benchLog bytesHash "Received tx"
  exceptT' (hoistEither $ decodeSTx encodedSTx) (const $ throwM eInvalidEnc) $ \txAux -> do
    let txHash = hash $ taTx txAux
    benchLog bytesHash "Decoded tx"
    logHashEquality txHash bytesHash
    -- FIXME: We are using only the confirmed UTxO, we should also take into account the pending txs
    exceptT' (verifyTx getTxOut False txAux) (throwM . eInvalidTx txHash) $ \_ -> do
      benchLog bytesHash "Verified tx"
      txProcessRes <- txpProcessTx (txHash, txAux)
      benchLog bytesHash "Processed tx"
      whenLeft txProcessRes $ throwM . eProcessErr txHash
      wasAccepted <- sendTx txAux
      benchLog bytesHash "Sent tx"
      void $ unless wasAccepted $ (throwM $ eNotAccepted txHash)
        where eInvalidEnc = Internal "Tx not broadcasted: invalid encoded tx"
              eInvalidTx txHash reason = Internal $
                  sformat ("Tx not broadcasted "%build%": "%build) txHash reason
              eProcessErr txHash err = Internal $
                  sformat ("Tx not broadcasted "%build%": error during process "%build) txHash err
              eNotAccepted txHash = Internal $
                  sformat  ("Tx broadcasted "%build%", not accepted by any peer") txHash
              exceptT' e f g = exceptT f g e
              benchLog txHash msg = logError $ toText $ sformat ("["%build%"] "%string) txHash msg
              logHashEquality txHash bytesHash = logError $ sformat ("["%build%"] Has tx hash "%hashHexF) bytesHash txHash


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | A pure function that return the number of blocks.
getBlockDifficulty :: Block -> Integer
getBlockDifficulty tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL
