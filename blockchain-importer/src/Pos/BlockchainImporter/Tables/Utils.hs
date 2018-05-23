{-# LANGUAGE OverloadedStrings #-}

module Pos.BlockchainImporter.Tables.Utils
  ( hashToString
  , addressToText
  , coinToInt64
  , runUpsertMany
  ) where

import           Universum

import           Database.PostgreSQL.Simple ()
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (sformat)
import qualified Opaleye as O

import           Pos.Core.Common (Address, Coin (..), addressF)
import           Pos.Crypto (hashHexF)
import           Pos.Crypto.Hashing (AbstractHash)
import           Pos.Txp.Toil.Types ()

hashToString :: AbstractHash algo a -> String
hashToString h = toString $ sformat hashHexF h

addressToText :: Address -> Text
addressToText addr = sformat addressF addr

coinToInt64 :: Coin -> Int64
coinToInt64 = fromIntegral . getCoin

{-  Insert rows into a table, only if they are not already present

    FIXME: Due to upsert not being yet implemented by Opaleye [1] this had to
    be manually implemented, which involved using the deprecated function
    'arrangeInsertManySql'. Once this feature gets released, the usage of this
    function will be removed.

    [1] https://github.com/tomjaguarpaw/haskell-opaleye/pull/385#issuecomment-384313025
-}
runUpsertMany :: PGS.Connection -> O.Table columns columns' -> [columns] -> String -> IO Int64
runUpsertMany conn table columns keyCol = case nonEmpty columns of
    Just neColumns -> PGS.execute_ conn . fromString $ strUpsertQuery neColumns
    Nothing        -> return 0
  where strInsertQuery col = O.arrangeInsertManySql table col
        strUpsertQuery col = (strInsertQuery col)  ++ " ON CONFLICT (" ++ keyCol  ++ ") DO NOTHING"

