{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.V1.Types 
(
    ETRedeemer(..)  
  , ETValidatorParams(..)

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

-- | The LCRedemeer used to indicate if the action is to mint or burn littercoin or
--   to add and remove Ada from the littercoin contract.   Also specify the amount 
--   as well in the redeemer.
data ETRedeemer = 
       Spend            -- spend earthtrust locked Ada and send to merchant and donor 
     | Refund Integer   -- refund locked Ada to customer

    deriving Haskell.Show

PlutusTx.makeIsDataIndexed
  ''ETRedeemer
  [ ('Spend, 0),
    ('Refund, 1)
  ]
PlutusTx.makeLift ''ETRedeemer

