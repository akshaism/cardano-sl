-- | Undo for block

module Pos.Core.Block.Undo
       ( Undo (..)
       , SlogUndo (..)
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation (DlgUndo)
import           Pos.Core.Slotting (unflattenSlotId)
import           Pos.Core.Txp (TxpUndo)
import           Pos.Core.Types (FlatSlotId, slotIdF)
import           Pos.Core.Update (USUndo)

-- | Structure for undo block during rollback
data Undo = Undo
    { undoTx   :: !TxpUndo
    , undoDlg  :: !DlgUndo
    , undoUS   :: !USUndo
    , undoSlog :: !SlogUndo
    } deriving (Generic)

instance NFData Undo

instance HasConfiguration => Buildable Undo where
    build Undo{..} =
        bprint ("Undo:\n"%
                "  undoTx: "%listJson%"\n"%
                "  undoDlg: "%build%"\n"%
                "  undoUS: "%build%"\n"%
                "  undoSlog: "%build)
               (map (bprint listJson) undoTx) undoDlg undoUS undoSlog

-- | Undo data from Slog, i. e. data which is necessary do rollback a
-- block inside Slog.
--
-- If block is one of the first 'blkSecurityParam' blocks, we don't
-- need to store anything. We also don't need to store anything for
-- genesis blocks. Otherwise we store 'FlatSlotId' of the oldest block
-- from those for which we stored slots before given block was
-- applied.
newtype SlogUndo = SlogUndo
    { getSlogUndo :: Maybe FlatSlotId
    } deriving (NFData, Generic)

instance HasConfiguration => Buildable SlogUndo where
    build (SlogUndo oldSlot) =
        "SlogUndo: " <>
        maybe "<nothing>" (bprint slotIdF . unflattenSlotId) oldSlot
