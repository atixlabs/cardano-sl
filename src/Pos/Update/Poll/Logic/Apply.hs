{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic of application and verification of data in Poll.

module Pos.Update.Poll.Logic.Apply
       ( verifyAndApplyUSPayload
       , verifyAndApplyProposal
       , verifyAndApplyVoteDo
       ) where

import           Control.Monad.Except          (MonadError, throwError)
import           Data.List                     (partition)
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NE
import           Formatting                    (build, builder, int, sformat, (%))
import           System.Wlog                   (logDebug, logInfo, logNotice)
import           Universum

import           Pos.Constants                 (blkSecurityParam, genesisUpdateImplicit,
                                                genesisUpdateProposalThd,
                                                genesisUpdateVoteThd)
import           Pos.Crypto                    (hash, shortHashF)
import           Pos.Ssc.Class                 (Ssc)
import           Pos.Types                     (ChainDifficulty, Coin, EpochIndex,
                                                HeaderHash, MainBlockHeader,
                                                SlotId (siEpoch), SoftwareVersion (..),
                                                addressHash, applyCoinPortion,
                                                coinToInteger, difficultyL, epochIndexL,
                                                flattenSlotId, gbhExtra, headerHash,
                                                headerSlot, mehBlockVersion, sumCoins,
                                                unflattenSlotId, unsafeIntegerToCoin)
import           Pos.Update.Core               (UpId, UpdatePayload (..),
                                                UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Poll.Class         (MonadPoll (..), MonadPollRead (..))
import           Pos.Update.Poll.Failure       (PollVerFailure (..))
import           Pos.Update.Poll.Logic.Base    (canBeAdoptedBV, canCreateBlockBV,
                                                confirmBlockVersion, isDecided,
                                                mkTotNegative, mkTotPositive, mkTotSum,
                                                putNewProposal, voteToUProposalState)
import           Pos.Update.Poll.Logic.Version (verifyAndApplyProposalBVS,
                                                verifyBlockVersion, verifySoftwareVersion)
import           Pos.Update.Poll.Types         (ConfirmedProposalState (..),
                                                DecidedProposalState (..), DpsExtra (..),
                                                ProposalState (..),
                                                UndecidedProposalState (..),
                                                UpsExtra (..), psProposal)

type ApplyMode m = (MonadError PollVerFailure m, MonadPoll m)

-- | Verify UpdatePayload with respect to data provided by
-- MonadPoll. If data is valid it is also applied.  Otherwise
-- PollVerificationFailure is thrown using MonadError type class.
-- When first flag is true and proposal is present,
-- 'genesisUpdateProposalThd' is checked for it, otherwise it's not
-- checked.
-- When second argument is 'Left epoch', it means that temporary payload
-- for given slot is applied.
-- When it is 'Right header', it means that payload from block with
-- given header is applied.
verifyAndApplyUSPayload
    :: forall ssc m . (ApplyMode m, Ssc ssc)
    => Bool -> Either SlotId (MainBlockHeader ssc) -> UpdatePayload -> m ()
verifyAndApplyUSPayload considerPropThreshold slotOrHeader UpdatePayload {..} = do
    -- First of all, we verify data from header.
    either (const pass) verifyHeader slotOrHeader
    -- Then we split all votes into groups. One group consists of
    -- votes for proposal from payload. Each other group consists of
    -- votes for other proposals.
    let upId = hash <$> upProposal
    let votePredicate vote = maybe False (uvProposalId vote ==) upId
    let (curPropVotes, otherVotes) = partition votePredicate upVotes
    let otherGroups = NE.groupWith uvProposalId otherVotes
    -- When there is proposal in payload, it's verified and applied.
    whenJust upProposal $
        verifyAndApplyProposal considerPropThreshold slotOrHeader curPropVotes
    -- Then we also apply votes from other groups.
    -- ChainDifficulty is needed, because proposal may become approved
    -- and then we'll need to track whether it becomes confirmed.
    let cd = (,) <$> either (const Nothing) (Just . view difficultyL) slotOrHeader
                 <*> either (const Nothing) (Just . headerHash) slotOrHeader
    mapM_ (verifyAndApplyVotesGroup cd) otherGroups
    -- If we are applying payload from block, we also check implicit
    -- agreement rule and depth of decided proposals (they can become
    -- confirmed/discarded).
    case slotOrHeader of
        Left _ -> pass
        Right mainBlk -> do
            applyImplicitAgreement
                (mainBlk ^. headerSlot)
                (mainBlk ^. difficultyL)
                (headerHash mainBlk)
            applyDepthCheck
                (headerHash mainBlk)
                (mainBlk ^. difficultyL)

-- Here we verify all US-related data from header.
verifyHeader
    :: (MonadError PollVerFailure m, MonadPoll m)
    => MainBlockHeader __ -> m ()
verifyHeader header = do
    lastAdopted <- getAdoptedBV
    let versionInHeader = header ^. gbhExtra ^. mehBlockVersion
    unlessM (canCreateBlockBV versionInHeader) $
        throwError
            PollWrongHeaderBlockVersion
            {pwhpvGiven = versionInHeader, pwhpvAdopted = lastAdopted}

-- Get stake of stakeholder who issued given vote as per given epoch.
-- If stakeholder wasn't richman at that point, PollNotRichman is thrown.
resolveVoteStake
    :: (MonadError PollVerFailure m, MonadPollRead m)
    => EpochIndex -> Coin -> UpdateVote -> m Coin
resolveVoteStake epoch totalStake UpdateVote {..} = do
    let !id = addressHash uvKey
    stake <- note (mkNotRichman id Nothing) =<< getRichmanStake epoch id
    when (stake < threshold) $ throwError $ mkNotRichman id (Just stake)
    return stake
  where
    threshold = applyCoinPortion genesisUpdateVoteThd totalStake
    mkNotRichman id stake =
        PollNotRichman
        {pnrStakeholder = id, pnrThreshold = threshold, pnrStake = stake}

-- Do all necessary checks of new proposal and votes for it.
-- If it's valid, apply. Specifically, these checks are done:
--
-- 1. Check that there is no active proposal for given application.
-- 2. Check script version, it should be consistent with existing
--    script version dependencies. New dependency can be added.
-- 3. Check that numeric software version of application is 1 more than
--    of last confirmed proposal for this application.
-- 4. If 'considerThreshold' is true, also check that sum of positive votes
--    for this proposal is enough (at least 'genesisUpdateProposalThd').
--
-- If all checks pass, proposal is added. It can be in undecided or decided
-- state (if it has enough voted stake at once).
verifyAndApplyProposal
    :: forall ssc m.
       (MonadError PollVerFailure m, MonadPoll m, Ssc ssc)
    => Bool
    -> Either SlotId (MainBlockHeader ssc)
    -> [UpdateVote]
    -> UpdateProposal
    -> m ()
verifyAndApplyProposal considerThreshold slotOrHeader votes up@UpdateProposal {..} = do
    let epoch = slotOrHeader ^. epochIndexL
    let !upId = hash up
    whenJustM (getProposal upId) $
        const $ throwError $ PollProposalAlreadyActive upId
    -- Here we verify consistency with regards to data from 'BlockVersionState'
    -- and update relevant state if necessary.
    verifyAndApplyProposalBVS upId up
    -- Then we verify that protocol version from proposal can follow last
    -- adopted software version.
    verifyBlockVersion upId up
    -- We also verify that software version is expected one.
    verifySoftwareVersion upId up
    -- After that we resolve stakes of all votes.
    totalStake <- note (PollUnknownStakes epoch) =<< getEpochTotalStake epoch
    votesAndStakes <-
        mapM (\v -> (v, ) <$> resolveVoteStake epoch totalStake v) votes
    -- When necessary, we also check that proposal itself has enough
    -- positive votes to be included into block.
    when considerThreshold $ verifyProposalStake totalStake votesAndStakes upId
    -- Finally we put it into context of MonadPoll together with votes for it.
    putNewProposal slotOrHeader totalStake votesAndStakes up

-- Here we check that proposal has at least 'genesisUpdateProposalThd'
-- stake of total stake in all positive votes for it.
verifyProposalStake
    :: (MonadPollRead m, MonadError PollVerFailure m)
    => Coin -> [(UpdateVote, Coin)] -> UpId -> m ()
verifyProposalStake totalStake votesAndStakes upId = do
    let threshold = applyCoinPortion genesisUpdateProposalThd totalStake
    let thresholdInt = coinToInteger threshold
    let votesSum =
            sumCoins . map snd . filter (uvDecision . fst) $ votesAndStakes
    logDebug $
        sformat
            ("Verifying stake for proposal "%shortHashF%
             ", threshold is "%int%", voted stake is "%int)
            upId thresholdInt votesSum
    when (votesSum < thresholdInt) $
        throwError
            PollSmallProposalStake
            { pspsThreshold = threshold
            , pspsActual = unsafeIntegerToCoin votesSum
            , pspsUpId = upId
            }

-- Here we verify votes for proposal which is already active. Each
-- vote must have enough stake as per distribution from epoch where
-- proposal was added.
-- We also verify that what votes correspond to real proposal in
-- undecided state.
-- Votes are assumed to be for the same proposal.
verifyAndApplyVotesGroup
    :: ApplyMode m
    => Maybe (ChainDifficulty, HeaderHash) -> NonEmpty UpdateVote -> m ()
verifyAndApplyVotesGroup cd votes = mapM_ verifyAndApplyVote votes
  where
    upId = uvProposalId $ NE.head votes
    verifyAndApplyVote vote = do
        let !stakeholderId = addressHash . uvKey $ NE.head votes
            unknownProposalErr =
                PollUnknownProposal
                {pupStakeholder = stakeholderId, pupProposal = upId}
        ps <- note unknownProposalErr =<< getProposal upId
        case ps of
            PSDecided _     -> throwError $ PollProposalIsDecided upId stakeholderId
            PSUndecided ups -> verifyAndApplyVoteDo cd ups vote

-- Here we actually apply vote to stored undecided proposal.
verifyAndApplyVoteDo
    :: ApplyMode m
    => Maybe (ChainDifficulty, HeaderHash)
    -> UndecidedProposalState
    -> UpdateVote
    -> m ()
verifyAndApplyVoteDo cd ups v@UpdateVote {..} = do
    let e = siEpoch $ upsSlot ups
    totalStake <- note (PollUnknownStakes e) =<< getEpochTotalStake e
    voteStake <- resolveVoteStake e totalStake v
    newUPS@UndecidedProposalState {..} <-
        voteToUProposalState uvKey voteStake uvDecision ups
    let newPS
            | Just decision <-
                 isDecided
                     (mkTotPositive upsPositiveStake)
                     (mkTotNegative upsNegativeStake)
                     (mkTotSum totalStake) =
                PSDecided
                    DecidedProposalState
                    { dpsUndecided = newUPS
                    , dpsDecision = decision
                    , dpsDifficulty = fst <$> cd
                    , dpsExtra = DpsExtra . snd <$> cd <*> Just False
                    }
            | otherwise = PSUndecided newUPS
    addActiveProposal newPS

-- According to implicit agreement rule all proposals which were put
-- into blocks earlier than 'genesisUpdateImplicit' slots before slot
-- of current block become implicitly decided (approved or rejected).
-- If proposal's total positive stake is bigger than negative, it's
-- approved. Otherwise it's rejected.
applyImplicitAgreement
    :: MonadPoll m
    => SlotId -> ChainDifficulty -> HeaderHash -> m ()
applyImplicitAgreement (flattenSlotId -> slotId) cd hh
    | slotId < genesisUpdateImplicit = pass
    | otherwise = do
        let oldSlot = unflattenSlotId $ slotId - genesisUpdateImplicit
        mapM_ applyImplicitAgreementDo =<< getOldProposals oldSlot
  where
    applyImplicitAgreementDo ups = do
        let decided = makeImplicitlyDecided ups
        addActiveProposal $ PSDecided decided
        let upId = hash $ upsProposal ups
            status | dpsDecision decided = "approved"
                   | otherwise = "rejected"
        logInfo $ sformat ("Proposal "%build%" is implicitly "%builder)
            upId status
    makeImplicitlyDecided ups@UndecidedProposalState {..} =
        DecidedProposalState
        { dpsUndecided = ups
        , dpsDecision = upsPositiveStake > upsNegativeStake
        , dpsDifficulty = Just cd
        , dpsExtra = Just $ DpsExtra hh True
        }

-- All decided proposals which became decided more than
-- 'blkSecurityParam' blocks deeper than current block become
-- confirmed or discarded (approved become confirmed, rejected become
-- discarded).
applyDepthCheck
    :: ApplyMode m
    => HeaderHash -> ChainDifficulty -> m ()
applyDepthCheck hh cd
    | cd <= blkSecurityParam = pass
    | otherwise = do
        deepProposals <- getDeepProposals (cd - blkSecurityParam)
        let winners =
                concatMap toList $
                map resetAllDecisions $
                map (NE.sortBy proposalCmp) $
                NE.groupWith groupCriterion deepProposals
        mapM_ applyDepthCheckDo winners
  where
    upsAppName = svAppName . upSoftwareVersion . upsProposal
    resetAllDecisions (a:|xs) = a :| map (\x->x {dpsDecision = False}) xs
    groupCriterion a =
        ( upsAppName $ dpsUndecided a
        , dpsDifficulty a)
    mkTuple a extra =
        ( dpsDecision a
        , not $ deImplicit extra
        , upsPositiveStake $ dpsUndecided a
        , upsSlot $ dpsUndecided a)
    -- This comparator chooses the most appropriate proposal among
    -- proposals of one app and with same chain difficulty.
    proposalCmp a b
      | Just extraA <- dpsExtra a
      , Just extraB <- dpsExtra b =
          compare (mkTuple b extraB) (mkTuple a extraA)
      -- The following checks just in case,
      -- if there are proposals without dpsExtra
      | Just _ <- dpsExtra a = LT
      | Just _ <- dpsExtra b = GT
      | otherwise =
          compare  (upsSlot $ dpsUndecided b) (upsSlot $ dpsUndecided a)
    applyDepthCheckDo DecidedProposalState {..} = do
        let UndecidedProposalState {..} = dpsUndecided
        let sv = upSoftwareVersion upsProposal
        let bv = upBlockVersion upsProposal
        let upId = hash upsProposal
        let status | dpsDecision = "confirmed"
                   | otherwise = "discarded"
        when dpsDecision $ do
            setLastConfirmedSV sv
            DpsExtra {..} <-
                note (PollInternalError "DPS extra: expected Just, but got Nothing")
                      dpsExtra
            UpsExtra {..} <-
                note (PollInternalError "UPS extra: expected Just, but got Nothing")
                      upsExtra
            let cps = ConfirmedProposalState
                    { cpsUpdateProposal = upsProposal
                    , cpsVotes = upsVotes
                    , cpsPositiveStake = upsPositiveStake
                    , cpsNegativeStake = upsNegativeStake
                    , cpsImplicit = deImplicit
                    , cpsProposed = ueProposedBlk
                    , cpsDecided = deDecidedBlk
                    , cpsConfirmed = hh
                    , cpsAdopted = Nothing
                    }
            addConfirmedProposal cps
            proposals <- getProposalsByApp $ upsAppName dpsUndecided
            mapM_ (deactivateProposal . hash . psProposal) proposals
        needConfirmBV <- (dpsDecision &&) <$> canBeAdoptedBV bv
        if | needConfirmBV -> do
               confirmBlockVersion bv
               logInfo $ sformat (build%" is competing now") bv
           | otherwise -> do
               delBVState bv
               logInfo $ sformat ("State of "%build%" is deleted") bv
        deactivateProposal upId
        logNotice $ sformat ("Proposal "%shortHashF%" is "%builder) upId status
