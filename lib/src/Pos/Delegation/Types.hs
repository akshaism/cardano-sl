-- | Delegation-related local types.

module Pos.Delegation.Types
       ( DlgPayload (..)
       , mkDlgPayload
       , ProxySKLightConfirmation
       , DlgUndo (..)
       , DlgMemPool
       , ProxySKBlockInfo
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson)

import           Pos.Binary.Core ()
import           Pos.Core (ProxySKHeavy, ProxySKLight, ProxySigLight, StakeholderId)
import           Pos.Core.Delegation (DlgPayload (..), mkDlgPayload)
import           Pos.Crypto (PublicKey)

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Confirmation of light cert type.
type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)

-- | Lightweight PSK or heavyweight PSK with real leader public key
-- (because heavyweight psks have redelegation feature, so pskIssuerPk
-- hPsk /= leader in general case). This is used to create a block
-- header only.
type ProxySKBlockInfo = Maybe (Either ProxySKLight (ProxySKHeavy, PublicKey))
