-- | Delegation types serialization.

module Pos.Binary.Core.Delegation
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Crypto ()
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation (DlgPayload (getDlgPayload), DlgUndo (..), mkDlgPayload)
import           Pos.Core.Types (ProxySKHeavy, StakeholderId)
import           Pos.Util.Util (eitherToFail)

instance HasConfiguration => Bi DlgPayload where
    encode = encode . getDlgPayload
    decode = decode >>= eitherToFail . mkDlgPayload

deriveSimpleBi ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]
