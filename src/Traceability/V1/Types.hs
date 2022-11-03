{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.V1.Types 
(
   ETValidatorParams(..)

)where

import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Integer)
import qualified    Prelude as Haskell                  (Show)




-- | The NFT minting policy params passes parameters 
--   into the minting poicy which will make the NFT policy unique
data ETValidatorParams = ETValidatorParams
    { 
      etvVersion                 :: !Integer  
    , etvSplit                   :: !Integer
    , etvMerchantPkh             :: !Address.PaymentPubKeyHash
    , etvDonorPkh                :: !Address.PaymentPubKeyHash
    , etvAdminPkh                :: !Address.PaymentPubKeyHash
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ETValidatorParams [('ETValidatorParams,0)] 
PlutusTx.makeLift ''ETValidatorParams

