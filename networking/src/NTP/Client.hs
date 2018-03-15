{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module implements functionality of NTP client.

module NTP.Client
    ( NtpMonad
    , spawnNtpClient
    , NtpClientSettings (..)
    , NtpStatus (..)
    , ntpSingleShot
    ) where

import           Universum

import           Control.Concurrent.STM (check, modifyTVar')
import           Control.Concurrent.STM.TVar (TVar, readTVar)
import           Control.Exception.Safe (Exception, MonadMask, catchAny, handleAny)
import           Control.Lens ((%=), (.=), _Just)
import           Control.Monad (forever)
import           Control.Monad.State (gets)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Binary (decodeOrFail, encode)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (catMaybes, isNothing)
import           Data.Time.Units (Microsecond, Second, toMicroseconds)
import           Data.Typeable (Typeable)
import           Formatting (sformat, shown, (%))
import           Network.Socket (AddrInfo, SockAddr (..), Socket, addrAddress, addrFamily, close)
import           Network.Socket.ByteString (recvFrom, sendTo)
import           Serokell.Util.Concurrent (modifyTVarS, threadDelay)
import           System.Wlog (LoggerName, WithLogger, logDebug, logError, logInfo, logWarning,
                              modifyLoggerName)

import           Mockable.Class (Mockable)
import           Mockable (Async, Concurrently, CurrentTime, Delay, concurrently, currentTime, forConcurrently,
                                      timeout, withAsync)
import           NTP.Packet (NtpPacket (..), evalClockOffset, mkCliNtpPacket, ntpPacketSize)
import           NTP.Util (createAndBindSock, resolveNtpHost, selectIPv4, selectIPv6,
                           udpLocalAddresses, withSocketsDoLifted)

data NtpStatus =
      NtpSyncOk
    | NtpDesync Microsecond
    | NtpSyncUnavailable
    deriving (Eq, Show)

data NtpClientSettings = NtpClientSettings
    { ntpServers         :: [String]
      -- ^ list of servers addresses
    , ntpLogName         :: LoggerName
      -- ^ logger name modifier
    , ntpResponseTimeout :: Microsecond
      -- ^ delay between making requests and response collection
    , ntpPollDelay       :: Microsecond
      -- ^ how often to send responses to server
    , ntpMeanSelection   :: [(Microsecond, Microsecond)] -> (Microsecond, Microsecond)
      -- ^ way to sumarize results received from different servers.
      -- this may accept list of lesser size than @length ntpServers@ in case
      -- some servers failed to respond in time, but never an empty list
    , ntpTimeDifferenceWarnInterval  :: Microsecond
      -- ^ NTP checking interval
    , ntpTimeDifferenceWarnThreshold :: Microsecond
      -- ^ Maxium tolerable difference between NTP time and locak time.
    }

data NtpClient = NtpClient
    { ncSockets  :: TVar Sockets
      -- ^ ntp client sockets: ipv4 / ipv6 / both
    , ncState    :: TVar (Maybe [(Microsecond, Microsecond)])
      -- ^ ntp client state, list of received values
    , ncStatus          :: TVar NtpStatus
      -- ^ got time callback (margin, time when client sent request)
    , ncSettings :: NtpClientSettings
      -- ^ client configuration
    }

mkNtpClient :: MonadIO m => NtpClientSettings -> TVar NtpStatus -> Sockets -> m NtpClient
mkNtpClient ncSettings ncStatus sock = liftIO $ do
    ncSockets <- newTVarIO sock
    ncState  <- newTVarIO Nothing
    return NtpClient{..}

data NoHostResolved = NoHostResolved
    deriving (Show, Typeable)

instance Exception NoHostResolved

type NtpMonad m =
    ( MonadIO m
    , MonadBaseControl IO m
    , MonadMask m
    , WithLogger m
    , Mockable Concurrently m
    , Mockable CurrentTime m
    , Mockable Async m
    , Mockable Delay m
    )

handleCollectedResponses :: NtpMonad m => NtpClient -> m ()
handleCollectedResponses cli = do
    mres <- readTVarIO (ncState cli)
    let selection = ntpMeanSelection (ncSettings cli)
    case mres of
        Nothing        -> logError "Protocol error: responses are not awaited"
        Just []        -> logWarning "No servers responded"
        Just responses -> handleE `handleAny` do
            let time = selection responses
            logInfo $ sformat ("Evaluated clock offset "%shown%
                " mcs for request at "%shown%" mcs")
                (toMicroseconds $ fst time)
                (toMicroseconds $ snd time)
            handler time
  where
    handleE = logError . sformat ("ntpMeanSelection: "%shown)

    handler :: NtpMonad m => (Microsecond, Microsecond) -> m ()
    handler (newMargin, transmitTime) = do
        let ntpTime = transmitTime + newMargin
        localTime <- currentTime
        let timeDiff = abs $ ntpTime - localTime
        -- If the @absolute@ time difference between the NTP time and the local time is
        -- bigger than the given threshold, it effectively means we are not synced, as we are
        -- either behind or ahead of the NTP time.
        let !status
                | timeDiff > (ntpTimeDifferenceWarnThreshold . ncSettings $ cli) = NtpDesync timeDiff
                | otherwise = NtpSyncOk
        atomically $ writeTVar (ncStatus cli) status


allResponsesGathered :: NtpClient -> STM Bool
allResponsesGathered cli = do
    responsesState <- readTVar $ ncState cli
    let servers = ntpServers $ ncSettings cli
    return $ case responsesState of
        Nothing        -> False
        Just responses -> length responses >= length servers

doSend :: NtpMonad m => SockAddr -> NtpClient -> m ()
doSend addr cli = do
    sock   <- readTVarIO $ ncSockets cli
    packet <- encode <$> mkCliNtpPacket
    handleAny handleE . void . liftIO $ sendDo addr sock (LBS.toStrict packet)
  where
    sendDo a@(SockAddrInet _ _) (IPv4Sock sock)      = sendTo' sock a
    sendDo a@(SockAddrInet _ _) (BothSock sock _)    = sendTo' sock a
    sendDo a@(SockAddrInet6 _ _ _ _) (IPv6Sock sock) = sendTo' sock a
    sendDo a@(SockAddrInet6 _ _ _ _) (BothSock _ sock)  = sendTo' sock a
    sendDo a sks                                           =
        error $ "SockAddr is " <> show a <> ", but sockets: " <> show sks
    sendTo' sock = flip (sendTo sock)

    -- just log; socket closure is handled by receiver
    handleE =
        logWarning . sformat ("Failed to send to "%shown%": "%shown) addr

startSend :: NtpMonad m => [SockAddr] -> NtpClient -> m ()
startSend addrs cli = do
    let respTimeout = ntpResponseTimeout (ncSettings cli)
    let poll    = ntpPollDelay (ncSettings cli)

    _ <- concurrently (threadDelay poll) $ do
        logDebug "Sending requests"
        atomically . modifyTVarS (ncState cli) $ identity .= Just []
        let sendRequests = forConcurrently addrs (flip doSend cli)
        let waitTimeout = void $ timeout respTimeout
                    (atomically $ check =<< allResponsesGathered cli)
        withAsync sendRequests $ \_ -> waitTimeout

        logDebug "Collecting responses"
        handleCollectedResponses cli
        atomically . modifyTVarS (ncState cli) $ identity .= Nothing

    startSend addrs cli

-- Try to create IPv4 and IPv6 socket.
mkSockets :: forall m . NtpMonad m => NtpClientSettings -> m Sockets
mkSockets settings = do
    (sock1MB, sock2MB) <- doMkSockets `catchAny` handlerE
    whenJust sock1MB logging
    whenJust sock2MB logging
    case (fst <$> sock1MB, fst <$> sock2MB) of
        (Just sock1, Just sock2) -> pure $ BothSock sock1 sock2
        (Just sock1, Nothing)    -> pure $ IPv4Sock sock1
        (Nothing, Just sock2)    -> pure $ IPv6Sock sock2
        (_, _)                   -> do
            logWarning "Couldn't create both IPv4 and IPv6 socket, retrying in 5 sec..."
            threadDelay (5 :: Second)
            mkSockets settings
  where
    logging (_, addrInfo) = logInfo $
        sformat ("Created socket (family/addr): "%shown%"/"%shown)
                (addrFamily addrInfo) (addrAddress addrInfo)
    doMkSockets :: m (Maybe (Socket, AddrInfo), Maybe (Socket, AddrInfo))
    doMkSockets = liftIO $ do
        serveraddrs <- udpLocalAddresses
        (,) <$> createAndBindSock selectIPv4 serveraddrs
            <*> createAndBindSock selectIPv6 serveraddrs
    handlerE e = do
        logWarning $
            sformat ("Failed to create sockets, retrying in 5 sec... (reason: "%shown%")")
            e
        threadDelay (5 :: Second)
        doMkSockets

handleNtpPacket :: NtpMonad m => NtpClient -> NtpPacket -> m ()
handleNtpPacket cli packet = do
    logDebug $ sformat ("Got packet "%shown) packet

    clockOffset <- evalClockOffset packet

    logDebug $ sformat ("Received time delta "%shown%" mcs")
        (toMicroseconds clockOffset)

    late <- atomically . modifyTVarS (ncState cli) $ do
        _Just %= ((clockOffset, ntpOriginTime packet) :)
        gets isNothing
    when late $
        logWarning "Response was too late"

doReceive :: NtpMonad m => Socket -> NtpClient -> m ()
doReceive sock cli = forever $ do
    (received, _) <- liftIO $ recvFrom sock ntpPacketSize
    let eNtpPacket = decodeOrFail $ LBS.fromStrict received
    case eNtpPacket of
        Left  (_, _, err)    ->
            logWarning $ sformat ("Error while receiving time: "%shown) err
        Right (_, _, packet) ->
            handleNtpPacket cli packet `catchAny` handleE
  where
    handleE = logWarning . sformat ("Error while handle packet: "%shown)

startReceive :: NtpMonad m => NtpClient -> m ()
startReceive cli = do
    sockets <- atomically . readTVar $ ncSockets cli
    case sockets of
        BothSock sIPv4 sIPv6 ->
            () <$ runDoReceive True sIPv4 `concurrently` runDoReceive False sIPv6
        IPv4Sock sIPv4 -> runDoReceive True sIPv4
        IPv6Sock sIPv6 -> runDoReceive False sIPv6
  where
    runDoReceive isIPv4 sock = doReceive sock cli `catchAny` handleE isIPv4 sock
    -- got error while receiving data, retrying in 5 sec
    handleE isIPv4 sock e = do
        logDebug $ sformat ("doReceive failed on socket"%shown%
                            ", reason: "%shown%
                            ", recreate socket in 5 sec") sock e
        threadDelay (5 :: Second)
        serveraddrs <- liftIO udpLocalAddresses
        newSockMB <- liftIO $
            if isIPv4 then
                traverse (overwriteSocket IPv4Sock . fst) =<< createAndBindSock selectIPv4 serveraddrs
            else
                traverse (overwriteSocket IPv6Sock . fst) =<< createAndBindSock selectIPv6 serveraddrs
        case newSockMB of
            Nothing      -> logWarning "Recreating of socket failed" >> handleE isIPv4 sock e
            Just newSock -> runDoReceive isIPv4 newSock
    overwriteSocket constr sock = sock <$
        (atomically .
         modifyTVar' (ncSockets cli) .
         flip mergeSockets .
         constr $ sock)

spawnNtpClient :: NtpMonad m => NtpClientSettings -> TVar NtpStatus -> m ()
spawnNtpClient settings ntpStatus =
    withSocketsDoLifted $
    modifyLoggerName (<> ntpLogName settings) $
    bracket (mkSockets settings) closeSockets $ \sock -> do
        cli <- mkNtpClient settings ntpStatus sock

        addrs <- catMaybes <$> mapM (resolveHost $ socketsToBoolDescr sock)
                                    (ntpServers settings)
        when (null addrs) $ throwM NoHostResolved
        () <$ startReceive cli `concurrently`
              startSend addrs cli `concurrently`
              logInfo "Launched NTP client"
  where
    closeSockets sockets = do
        logInfo "NTP client is stopped"
        forM_ (socketsToList sockets) (liftIO . close)
    resolveHost sockDescr host = do
        maddr <- liftIO $ resolveNtpHost host sockDescr
        case maddr of
            Nothing   -> do
                logWarning $ sformat ("Host "%shown%" is not resolved") host
                pure Nothing
            Just addr -> do
                logInfo $ sformat ("Host "%shown%" is resolved: "%shown) host addr
                pure $ Just addr

-- | Start client, wait for a while so that most likely it ticks once
-- and stop it.
ntpSingleShot
    :: (NtpMonad m)
    => NtpClientSettings
    -> TVar NtpStatus
    -> m ()
ntpSingleShot ntpSettings ntpStatus =
    () <$ timeout (ntpResponseTimeout ntpSettings) (spawnNtpClient ntpSettings ntpStatus)

-- Store created sockets.
-- If system supports IPv6 and IPv4 we create socket for IPv4 and IPv6.
-- Otherwise only one.
data Sockets
    = IPv4Sock !Socket
    | IPv6Sock !Socket
    | BothSock !Socket !Socket
    deriving Show

socketsToList :: Sockets -> [Socket]
socketsToList (BothSock s1 s2) = [s1, s2]
socketsToList (IPv4Sock s1)    = [s1]
socketsToList (IPv6Sock s1)    = [s1]

socketsToBoolDescr :: Sockets -> (Bool, Bool)
socketsToBoolDescr (BothSock _ _) = (True, True)
socketsToBoolDescr (IPv4Sock _)   = (True, False)
socketsToBoolDescr (IPv6Sock _)   = (False, True)

--              Old        New
mergeSockets :: Sockets -> Sockets -> Sockets
mergeSockets (BothSock _ v6) (IPv4Sock s) = BothSock s v6
mergeSockets (BothSock v4 _) (IPv6Sock s) = BothSock v4 s
mergeSockets (IPv6Sock _) (IPv6Sock s)    = IPv6Sock s
mergeSockets (IPv4Sock _) (IPv4Sock s)    = IPv4Sock s
mergeSockets _ _                          = error "Unexpected state of mergeSockets"
