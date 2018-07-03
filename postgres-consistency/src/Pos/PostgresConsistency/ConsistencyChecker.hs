module Pos.PostgresConsistency.ConsistencyChecker
  (
    externalConsistency
  , internalConsistencyCheck
  , externalConsistencyWithTxRange
  ) where

import           Universum

import           Data.List (tail)
import           System.Wlog (logInfo)

import           Pos.Core (HasPrevBlock (prevBlockL), HeaderHash, headerHash)
import           Pos.DB (getHeader, getTipHeader)
import           Pos.GState.BlockExtra (resolveForwardLink)
import           Pos.PostgresConsistency.Properties
import           Pos.PostgresConsistency.Utils

----------------------------------------------------------------------------
-- Checks
----------------------------------------------------------------------------

{-
  Check consistency with the key-value db of a node up-to-date.
  Objective: Test consistency of the postgresdb generated after running full import,
             with the one generated in a Cardano node.
  Requires: Having the postgresdb also up-to-date
    - Checks that utxo from node are stored in postgresdb
    - Checks that tx in node from random blocks are stored postgresdb
        The random blocks to check are received as a parameter
-}
externalConsistency :: ConsistencyCheckerEnv m => [HeaderHash] -> m Bool
externalConsistency blkHashes = do
  validTxsHistory <- allTxsFromManyBlksFullfilProp isJust blkHashes
  validUtxos <- consistentUtxo
  pure $ validTxsHistory && validUtxos

{-
  Check consistency with the key-value db generated by the importer
  Objective: Test internal consistency of the importer, mainly when stopping
             it during the chain importing process.
    - Checks that utxo from importer are stored in postgresdb
    - Checks that txs in importer from (bestblock-blkRangeSize, bestblock]
      are stored in postgresdb
    - Check txs_addresses table consistency (with txs table)
    - Check best block consistency with tip block in node
 -}
internalConsistencyCheck :: ConsistencyCheckerEnv m => m Bool
internalConsistencyCheck = do
  lastNBlocks <- getLastNBlkHashes blkRangeSize
  validLast10BlksTxs <- allTxsFromManyBlksFullfilProp isJust lastNBlocks
  validUtxos <- consistentUtxo
  validTxAddr <- internalConsistentTxAddr
  validBestBlock <- consistentBestBlock
  pure $ validLast10BlksTxs && validUtxos && validTxAddr && validBestBlock

{-
  Check consistency of the latest blocks with the key-value db of an up-to-date node
  Objective: Test consistency of the previous and future txs (from the point of the importer),
             with the one's stored in a Cardano node
    - Checks that tx in node from (block-blkRangeSize, block] are stored in postgresdb
    - Checks that tx in node from (block, block+blkRangeSize) are not stored in postgresdb
-}
externalConsistencyWithTxRange :: ConsistencyCheckerEnv m => HeaderHash -> m Bool
externalConsistencyWithTxRange pgTipHash = do
  prevNBlock <- getPrevNBlkHashesFromHash blkRangeSize pgTipHash
  nextNBlock <- getNextNBlkHashesFromHash blkRangeSize pgTipHash
  logInfo "Checking txs previous blocks exist"
  validPrevNBlocks <- allTxsFromManyBlksFullfilProp isJust prevNBlock
  logInfo "Checking txs next blocks don't exist"
  validNextNBlocks <- allTxsFromManyBlksFullfilProp isNothing (tail nextNBlock)
  pure $ validPrevNBlocks && validNextNBlocks


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

getLastNBlkHashes :: ConsistencyCheckerEnv m => Int -> m [HeaderHash]
getLastNBlkHashes numBlks = do
  tipHeader <- getTipHeader
  let tipHash = headerHash tipHeader
  getPrevNBlkHashesFromHash numBlks tipHash

getPrevNBlkHashesFromHash :: ConsistencyCheckerEnv m => Int -> HeaderHash -> m [HeaderHash]
getPrevNBlkHashesFromHash n initialHash = if n <= 0 then pure [] else
  maybeT (getHeader initialHash) (pure []) $ \initialHeader -> do
      let prevBlockHash = initialHeader ^. prevBlockL
      (initialHash:) <$> getPrevNBlkHashesFromHash (n - 1) prevBlockHash

getNextNBlkHashesFromHash :: ConsistencyCheckerEnv m => Int -> HeaderHash -> m [HeaderHash]
getNextNBlkHashesFromHash n initialHash = if n <= 0 then pure [] else
  maybeT (getHeader initialHash) (pure []) $ \initialHeader ->
      maybeT (resolveForwardLink initialHeader) (pure [initialHash]) $ \nextHeaderHash -> do
          hashesFromNext <- getNextNBlkHashesFromHash (n - 1) nextHeaderHash
          pure $ initialHash : hashesFromNext

-- blkRangeSize selected to be 'k' (number of blocks rollbacked on new epoch) + 10
-- FIXME: 2160 should be obtained from the protocol constants
blkRangeSize :: Int
blkRangeSize = 2160 + 10
