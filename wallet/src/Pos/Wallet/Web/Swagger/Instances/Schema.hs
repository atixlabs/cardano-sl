{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Instances of `ToSchema` & `ToParamSchema`

module Pos.Wallet.Web.Swagger.Instances.Schema where

import           Universum

import           Control.Lens                (ix, mapped, (?~))
import           Data.Swagger                (NamedSchema (..), SwaggerType (..),
                                              ToParamSchema (..), ToSchema (..),
                                              declareNamedSchema, declareSchema,
                                              declareSchemaRef,
                                              defaultSchemaOptions, format,
                                              genericDeclareNamedSchema, minItems,
                                              name, properties, required,
                                              sketchSchema, type_)
import           Data.Swagger.Internal.Schema
import           GHC.Generics
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Typeable               (Typeable, typeRep)
import           Data.Version                (Version)
import           Crypto.Hash                 (Digest)
import           Servant.Multipart           (FileData (..))

import           Pos.Client.Txp.Util         (InputSelectionPolicy(..))
import           Pos.Types                   (ApplicationName, BlockCount (..),
                                              Address (..), AddrAttributes (..),
                                              AddrStakeDistribution (..),
                                              CoinPortion (..), AddrType (..),
                                              BlockVersion, ChainDifficulty, Coin,
                                              SlotCount (..), SoftwareVersion,
                                              Script, mkCoin)
import           Pos.Txp.Core.Types          (Tx (..), TxIn (..), TxOut (..),
                                              TxAux (..), TxInWitness (..),
                                              TxSigData (..))
import           Pos.Data.Attributes         (Attributes (..), UnparsedFields (..))
import           Pos.Crypto.Hashing          (AbstractHash (..))
import           Pos.Crypto.HD               (HDAddressPayload (..))
import           Pos.Crypto.Signing          (Signature (..), RedeemSignature (..),
                                              PublicKey (..), RedeemPublicKey)
import qualified Crypto.Sign.Ed25519         as ED (Signature, PublicKey)
import           Cardano.Crypto.Wallet       (XSignature, XPub, ChainCode (..))
import           Pos.Util.BackupPhrase       (BackupPhrase)

import qualified Pos.Wallet.Web.ClientTypes  as CT
import qualified Pos.Wallet.Web.Error.Types  as ET

import           Pos.Aeson.Storage           ()
import           Pos.Wallet.Web.Methods.Misc (PendingTxsSummary, WalletStateSnapshot)

-- | Instances we need to build Swagger-specification for 'walletApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.

-- | This orphan instance prevents Generic-based deriving mechanism
-- to use 'ToSchema' 'ByteString' and instead defaults to 'binarySchema'.
instance GToSchema (K1 i BS.ByteString) where
  gdeclareNamedSchema _ _ _ = pure $ NamedSchema Nothing binarySchema

instance GToSchema (K1 i LBS.ByteString) where
  gdeclareNamedSchema _ _ _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema (Digest algo) where
  declareNamedSchema _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema UnparsedFields where
  declareNamedSchema _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema XSignature where
  declareNamedSchema _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema a => ToSchema (Attributes a)
instance ToSchema      (AbstractHash algo a)
instance ToSchema      TxIn
instance ToSchema      HDAddressPayload
instance ToSchema      AddrStakeDistribution
instance ToSchema      CoinPortion
instance ToSchema      AddrAttributes
instance ToSchema      AddrType
instance ToSchema      Address
instance ToSchema      TxOut
instance ToSchema      Tx
instance ToSchema      PublicKey
instance ToSchema      ED.Signature
instance ToSchema      ED.PublicKey
instance ToSchema      XPub
deriving instance Generic ChainCode
instance ToSchema      ChainCode
instance ToSchema      Script
instance ToSchema      RedeemPublicKey
instance ToSchema      (Signature TxSigData)
instance ToSchema      (RedeemSignature TxSigData)
instance ToSchema      TxInWitness
instance ToSchema      TxAux
instance ToSchema      Coin
instance ToParamSchema Coin
instance ToSchema      CT.CTxId
instance ToParamSchema CT.CTxId
instance ToSchema      CT.CTx
instance ToSchema      CT.CTxMeta
instance ToSchema      CT.CPtxCondition
instance ToSchema      CT.CHash
instance ToParamSchema CT.CHash
instance ToSchema      (CT.CId CT.Wal)
instance ToSchema      (CT.CId CT.Addr)
instance ToParamSchema (CT.CId CT.Wal)
instance ToParamSchema (CT.CId CT.Addr)
instance ToSchema      CT.CProfile
instance ToSchema      ET.WalletError

instance ToSchema      CT.CAccountId
instance ToParamSchema CT.CAccountId where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletKeyIndex"

instance ToSchema      CT.CWalletAssurance
instance ToSchema      CT.CAccountMeta
instance ToSchema      CT.CWalletMeta
instance ToSchema      CT.CAccountInit
instance ToSchema      CT.CWalletInit
instance ToSchema      CT.CWalletRedeem
instance ToSchema      CT.CWallet
instance ToSchema      CT.CAccount
instance ToSchema      CT.CAddress
instance ToSchema      CT.CPaperVendWalletRedeem
instance ToSchema      CT.CCoin
instance ToSchema      CT.CInitialized
instance ToSchema      CT.CElectronCrashReport
instance ToSchema      CT.CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      CT.SyncProgress
instance ToSchema      BlockCount
instance ToSchema      SlotCount
instance ToSchema      ChainDifficulty
instance ToSchema      InputSelectionPolicy
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase
instance ToParamSchema CT.CPassPhrase
instance ToParamSchema CT.ScrollOffset
instance ToParamSchema CT.ScrollLimit
instance ToSchema      CT.CFilePath
instance ToSchema      CT.ApiVersion
instance ToSchema      Version
instance ToSchema      CT.ClientInfo
instance ToSchema      CT.CEncodedData
instance ToSchema      CT.CEncTxWithWit

instance ToSchema WalletStateSnapshot where
    declareNamedSchema _ = pure $ NamedSchema (Just "WalletStateSnapshot") mempty

instance ToSchema PendingTxsSummary where
    declareNamedSchema _ = pure $ NamedSchema (Just "PendingTxsSummary") mempty

instance ToSchema FileData where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        filepathSchema <- declareSchemaRef (Proxy :: Proxy FilePath)
        return $ NamedSchema (Just "FileData") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("fdInputFile", textSchema)
                , ("fdFileName", textSchema)
                , ("fdFileCType", textSchema)
                , ("fdFilePath", filepathSchema)
                ]
            & required .~ [ "fdInputFile", "fdFileName", "fdFileCType", "fdFilePath"]

instance ToSchema CT.NewBatchPayment where
    declareNamedSchema _ = do
        cAccountIdSchema <- declareSchemaRef (Proxy @CT.CAccountId)
        return $ NamedSchema (Just "NewBatchPayment") $
            sketchSchema example
                & properties . ix "npbFrom" .~ cAccountIdSchema
      where
        example = CT.NewBatchPayment
            { CT.npbFrom = CT.CAccountId "<walletId@accountId>"
            , CT.npbTo   = (CT.CId (CT.CHash "<address>"), mkCoin 228) :|
                          [(CT.CId (CT.CHash "<address>"), mkCoin 701)]
            , CT.npbPolicy = OptimizeForSecurity
            }

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-}
         (Typeable a, ToSchema a) =>
         ToSchema (Either ET.WalletError a) where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped . name ?~ show (typeRep $ Proxy @(Either ET.WalletError a))

instance ToSchema a => ToSchema (NonEmpty a) where
    declareNamedSchema _ = do
        schema <- declareSchema (Proxy :: Proxy [a])
        pure $ NamedSchema Nothing $ schema
            & minItems ?~ 1