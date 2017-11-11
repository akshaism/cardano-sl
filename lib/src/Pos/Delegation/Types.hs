-- | Delegation-related local types.

module Pos.Delegation.Types
       ( ProxySKLightConfirmation
       , DlgMemPool
       , ProxySKBlockInfo
       , module Pos.Core.Delegation
       ) where

import           Universum

import           Pos.Binary.Core ()
import           Pos.Core (ProxySKHeavy, ProxySKLight, ProxySigLight)
import           Pos.Crypto (PublicKey)

-- Re-export
import           Pos.Core.Delegation

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Confirmation of light cert type.
type ProxySKLightConfirmation = (ProxySKLight, ProxySigLight ProxySKLight)

-- | Lightweight PSK or heavyweight PSK with real leader public key
-- (because heavyweight psks have redelegation feature, so pskIssuerPk
-- hPsk /= leader in general case). This is used to create a block
-- header only.
type ProxySKBlockInfo = Maybe (Either ProxySKLight (ProxySKHeavy, PublicKey))
