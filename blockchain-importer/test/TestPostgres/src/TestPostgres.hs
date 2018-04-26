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
import           Opaleye

data TxRowPoly a b = TxRow {trId :: a, trAmount :: b} deriving (Show)

type TxRow = TxRowPoly String Int
type TxRowPGW = TxRowPoly (Column PGText) (Column PGInt4)
type TxRowPGR = TxRowPoly (Column PGText) (Column PGInt4)

$(makeAdaptorAndInstance "pTx" ''TxRowPoly)

txTable :: Table TxRowPGW TxRowPGR
txTable = Table "tx" (pTx TxRow {trId = required "id", trAmount = required "amount"})

getTxs :: IO [TxRow]
getTxs = do
  conn <- PGS.connectPostgreSQL "host='localhost' port=5432 dbname='cardano' user='postgres' password='postgres'"
  runQuery conn query
  where
    query :: Opaleye.Query TxRowPGR
    query = queryTable txTable

test :: IO ()
test = do
  txs <- getTxs
  print txs

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . fromMaybe "Empty query" . showSqlForPostgres
