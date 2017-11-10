-- | Types related to Poll monad.

module Pos.Update.Poll.Modifier
       (
         -- * Poll modifier
       , PollModifier (..)
       , pmBVsL
       , pmAdoptedBVFullL
       , pmConfirmedL
       , pmConfirmedPropsL
       , pmActivePropsL
       , pmSlottingDataL
       , pmEpochProposersL
       , modifyPollModifier

       , LocalVotes
       ) where

import           Universum

import           Control.Lens (makeLensesFor)
import           Data.Default (Default (def))
import qualified Data.Text.Buildable
import           Data.Time.Units (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core.Types (ApplicationName, BlockVersion, ChainDifficulty, Coin, EpochIndex,
                                 HeaderHash, NumSoftwareVersion, ScriptVersion, SlotId,
                                 SoftwareVersion, StakeholderId, mkCoin)
import           Pos.Core.Update (BlockVersionData (..), BlockVersionModifier (..), UpId,
                                  UpdateProposal (..), UpdateVote)
import           Pos.Crypto (PublicKey)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Util.Modifier (MapModifier)


-- | Type alias for set of votes from stakeholders
type LocalVotes = HashMap UpId (HashMap PublicKey UpdateVote)

----------------------------------------------------------------------------
-- Modifier
----------------------------------------------------------------------------

-- | PollModifier is used in verification. It represents operation which
-- one should apply to global state to obtain result of application of
-- MemPool or blocks which are verified.
data PollModifier = PollModifier
    { pmBVs            :: !(MapModifier BlockVersion BlockVersionState)
    , pmAdoptedBVFull  :: !(Maybe (BlockVersion, BlockVersionData))
    , pmConfirmed      :: !(MapModifier ApplicationName NumSoftwareVersion)
    , pmConfirmedProps :: !(MapModifier SoftwareVersion ConfirmedProposalState)
    , pmActiveProps    :: !(MapModifier UpId ProposalState)
    , pmSlottingData   :: !(Maybe SlottingData)
    , pmEpochProposers :: !(Maybe (HashSet StakeholderId))
    } deriving (Eq, Show, Generic)

flip makeLensesFor ''PollModifier
    [ ("pmBVs", "pmBVsL")
    , ("pmAdoptedBVFull", "pmAdoptedBVFullL")
    , ("pmConfirmed", "pmConfirmedL")
    , ("pmConfirmedProps", "pmConfirmedPropsL")
    , ("pmActiveProps", "pmActivePropsL")
    , ("pmSlottingData", "pmSlottingDataL")
    , ("pmEpochProposers", "pmEpochProposersL")
    ]

instance Default PollModifier where
    def =
        PollModifier
        { pmBVs = mempty
        , pmAdoptedBVFull = Nothing
        , pmConfirmed = mempty
        , pmConfirmedProps = mempty
        , pmActiveProps = mempty
        , pmSlottingData = Nothing
        , pmEpochProposers = mempty
        }

-- | Unite two PollModifiers. Second argument dominates, i. e. if
-- there are two confliciting modifications, the second one wins.
modifyPollModifier :: PollModifier -> PollModifier -> PollModifier
modifyPollModifier pmOld pmNew = PollModifier
    (pmBVs pmOld            <>  pmBVs pmNew)
    (pmAdoptedBVFull pmNew  <|> pmAdoptedBVFull pmOld)
    (pmConfirmed pmOld      <>  pmConfirmed pmNew)
    (pmConfirmedProps pmOld <>  pmConfirmedProps pmNew)
    (pmActiveProps pmOld    <>  pmActiveProps pmNew)
    (pmSlottingData pmNew   <|>  pmSlottingData pmOld)
    (pmEpochProposers pmNew <|> pmEpochProposers pmOld)


instance Semigroup PollModifier where

instance Monoid PollModifier where
    mempty = def
    mappend = modifyPollModifier
