{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level specification of BlockchainImporter API (via Servant).

module Pos.BlockchainImporter.Web.Api
       ( BlockchainImporterApi
       , blockchainImporterApi
       , BlockchainImporterApiRecord(..)
       ) where

import           Universum

import           Control.Exception.Safe (try)
import           Data.Proxy (Proxy (Proxy))
import           Servant.API ((:>), Capture, Get, JSON, Post, QueryParam, ReqBody, Summary)
import           Servant.Generic ((:-), AsApi, ToServant)
import           Servant.Server (ServantErr (..))

import           Pos.BlockchainImporter.Web.ClientTypes (Byte, CAda, CAddress, CAddressSummary,
                                                         CAddressesFilter, CBlockEntry,
                                                         CBlockSummary, CEncodedSTx,
                                                         CGenesisAddressInfo, CGenesisSummary,
                                                         CHash, CTxBrief, CTxEntry, CTxId,
                                                         CTxSummary)
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError)
import           Pos.Core (EpochIndex)
import           Pos.Util.Servant (DQueryParam, ModifiesApiRes (..), VerbMod)

type PageNumber = Integer

-- | API result modification mode used here.
data BlockchainImporterVerbTag

-- | Wrapper for Servants 'Verb' data type,
-- which allows to catch exceptions thrown by BlockchainImporter's endpoints.
type BlockchainImporterVerb verb = VerbMod BlockchainImporterVerbTag verb

-- | Shortcut for common api result types.
type ExRes verbMethod a = BlockchainImporterVerb (verbMethod '[JSON] a)

instance ModifiesApiRes BlockchainImporterVerbTag where
    type ApiModifiedRes BlockchainImporterVerbTag a = Either BlockchainImporterError a
    modifyApiResult
        :: Proxy BlockchainImporterVerbTag
        -> IO (Either ServantErr a)
        -> IO (Either ServantErr (Either BlockchainImporterError a))
    modifyApiResult _ action = try . try $ either throwM pure =<< action

-- | Servant API which provides access to blockchainImporter
type BlockchainImporterApi = "api" :> ToServant (BlockchainImporterApiRecord AsApi)

-- | Helper Proxy
blockchainImporterApi :: Proxy BlockchainImporterApi
blockchainImporterApi = Proxy

-- | A servant-generic record with all the methods of the API
data BlockchainImporterApiRecord route = BlockchainImporterApiRecord
  {
    _totalAda :: route
        :- "supply"
        :> "ada"
        :> ExRes Get CAda

  , _blocksPages :: route
        :- Summary "Get the list of blocks, contained in pages."
        :> "blocks"
        :> "pages"
        :> QueryParam "page" Word
        :> QueryParam "pageSize" Word
        :> ExRes Get (PageNumber, [CBlockEntry])

  , _blocksPagesTotal :: route
        :- Summary "Get the list of total pages."
        :> "blocks"
        :> "pages"
        :> "total"
        :> QueryParam "pageSize" Word
        :> ExRes Get PageNumber

  , _blocksSummary :: route
        :- Summary "Get block's summary information."
        :> "blocks"
        :> "summary"
        :> Capture "hash" CHash
        :> ExRes Get CBlockSummary

  , _blocksTxs :: route
        :- Summary "Get brief information about transactions."
        :> "blocks"
        :> "txs"
        :> Capture "hash" CHash
        :> QueryParam "limit" Word
        :> QueryParam "offset" Word
        :> ExRes Get [CTxBrief]

  , _txsLast :: route
        :- Summary "Get information about the N latest transactions."
        :> "txs"
        :> "last"
        :> ExRes Get [CTxEntry]

  , _txsSummary :: route
        :- Summary "Get summary information about a transaction."
        :> "txs"
        :> "summary"
        :> Capture "txid" CTxId
        :> ExRes Get CTxSummary

  , _addressSummary :: route
        :- Summary "Get summary information about an address."
        :> "addresses"
        :> "summary"
        :> Capture "address" CAddress
        :> ExRes Get CAddressSummary

  , _epochPages :: route
        :- Summary "Get epoch pages, all the paged slots in the epoch."
        :> "epochs"
        :> Capture "epoch" EpochIndex
        :> QueryParam "page" Int
        :> ExRes Get (Int, [CBlockEntry])

  , _epochSlots :: route
        :- Summary "Get the slot information in an epoch."
        :> "epochs"
        :> Capture "epoch" EpochIndex
        :> Capture "slot" Word16
        :> ExRes Get [CBlockEntry]

  , _genesisSummary :: route
        :- "genesis"
        :> "summary"
        :> ExRes Get CGenesisSummary

  , _genesisPagesTotal :: route
        :- "genesis"
        :> "address"
        :> "pages"
        :> "total"
        :> QueryParam "pageSize" Word
        :> DQueryParam "filter" CAddressesFilter
        :> ExRes Get PageNumber

  , _genesisAddressInfo :: route
        :- "genesis"
        :> "address"
        :> QueryParam "page" Word
        :> QueryParam "pageSize" Word
        :> DQueryParam "filter" CAddressesFilter
        :> ExRes Get [CGenesisAddressInfo]

  , _statsTxs :: route
        :- "stats"
        :> "txs"
        :> QueryParam "page" Word
        :> ExRes Get TxsStats

  , _blockCount :: route
        :- "stats"
        :> "blocksCount"
        :> ExRes Get Integer

  , _sendSignedTx :: route
        :- "txs"
        :> "signed"
        :> ReqBody '[JSON] CEncodedSTx
        :> ExRes Post Bool
  }
  deriving (Generic)

type TxsStats = (PageNumber, [(CTxId, Byte)])
