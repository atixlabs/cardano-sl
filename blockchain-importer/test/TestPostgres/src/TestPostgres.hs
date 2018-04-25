{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module TestPostgres where

import           Opaleye (Column, Nullable, PGBool, PGDate, PGFloat8, PGInt4, PGInt8, PGText, Query,
                          QueryArr, Table, Unpackspec, aggregate, avg, count, groupBy, ifThenElse,
                          isNull, leftJoin, matchNullable, pgString, queryTable, restrict, runQuery,
                          showSqlForPostgres, sum, table, tableColumn, (.&&), (.++), (.<), (.<=),
                          (.==), (.===))

import           Control.Arrow (returnA)
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS


testTable :: Table (Column PGInt4, Column PGText)
                   (Column PGInt4, Column PGText)
testTable = table "test_table" (p2 (tableColumn "id", tableColumn "str"))


testTableQuery :: Query (Column PGInt4, Column PGText)
testTableQuery = queryTable testTable

runTestTableQuery :: PGS.Connection
                  -> Query (Column PGInt4, Column PGText)
                  -> IO [(Int, String)]
runTestTableQuery = runQuery

test :: IO ()
test = do
  conn <- PGS.connectPostgreSQL "host='localhost' port=5432 dbname='cardano' user='postgres' password='postgres'"
  res <- runTestTableQuery conn testTableQuery
  print res

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . maybe "Empty query" id . showSqlForPostgres
