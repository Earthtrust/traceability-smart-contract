{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-} 
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traceability.V1.Types 
(
     MintPolicyRedeemer(..)
   , ETMintPolicyParams(..)

)where

import qualified    Ledger.Address as Address           (PaymentPubKeyHash(..))
import qualified Ledger.Value as Value                  (TokenName(..))
import qualified    PlutusTx                            (makeIsDataIndexed, makeLift)
import              PlutusTx.Prelude                    (Bool(..), Integer)
import qualified    Prelude as Haskell                  (Show)


-- | The mint policy reeemder indicates if the token is to be minted or burned
data MintPolicyRedeemer = MintPolicyRedeemer
    { 
      mpPolarity                  :: !Bool              -- True = Mint, False = Burn
    , mpAdaAmount                 :: !Integer           -- The total amount of the order 
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''MintPolicyRedeemer [('MintPolicyRedeemer,0)] 
PlutusTx.makeLift ''MintPolicyRedeemer


-- | The NFT minting policy params passes parameters 
--   into the minting poicy which will make the NFT policy unique
data ETMintPolicyParams = ETMintPolicyParams
    { 
      etpVersion                 :: !Integer  
    , etpSplit                   :: !Integer
    , etpMerchantPkh             :: !Address.PaymentPubKeyHash
    , etpDonorPkh                :: !Address.PaymentPubKeyHash
    , etpTokenName               :: !Value.TokenName
    } deriving Haskell.Show

PlutusTx.makeIsDataIndexed ''ETMintPolicyParams [('ETMintPolicyParams,0)] 
PlutusTx.makeLift ''ETMintPolicyParams

