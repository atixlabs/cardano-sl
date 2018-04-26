{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module TestPostgres where

import           Control.Arrow (returnA)
import           Data.Maybe (fromMaybe)
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           GHC.Int (Int64)
import           Opaleye

type TxId = String
type TxAmount = Int

data TxRowPoly a b = TxRow {trId :: a, trAmount :: b} deriving (Show)

type TxRow = TxRowPoly TxId TxAmount
type TxRowPGW = TxRowPoly (Column PGText) (Column PGInt4)
type TxRowPGR = TxRowPoly (Column PGText) (Column PGInt4)

$(makeAdaptorAndInstance "pTx" ''TxRowPoly)

txTable :: Table TxRowPGW TxRowPGR
txTable = Table "tx" (pTx TxRow {trId = required "id", trAmount = required "amount"})

getTxs :: PGS.Connection -> IO ()
getTxs conn = do
  txs <- runQuery conn (queryTable txTable) :: IO [TxRow]
  print txs

insertTx :: PGS.Connection -> TxId -> TxAmount -> IO ()
insertTx conn id amount = do
  rows <- runInsertMany conn txTable [TxRow (pgString id) (constant amount)]
  putStrLn $ show rows ++ " row(s) inserted"

test :: IO ()
test = do
  conn <- PGS.connect PGS.defaultConnectInfo {PGS.connectDatabase = "cardano", PGS.connectPassword = "postgres"}
  getTxs conn
  insertTx conn "tx3" 789
  getTxs conn

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . fromMaybe "Empty query" . showSqlForPostgres
