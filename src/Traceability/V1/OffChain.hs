{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Traceability.V1.OffChain    
    (   
        TokenSchema
    ,   useEndpoint
    ,   TokenParams (..)
    ,   RedeemerParams (..)
    ) where

import           Traceability.V1.OnChain            (etCurSymbol, etPolicy, minAda)
import           Traceability.V1.Types              (ETMintPolicyParams(..), MintPolicyRedeemer(..))
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map as Map                    (keys)
import qualified Data.Text as T                     (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import           Ledger                             (getCardanoTxId)
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), pubKeyHashAddress)
import           Ledger.Constraints as Constraints  (mintingPolicy, mustMintValueWithRedeemer, 
                                                     mustPayToPubKey,  
                                                     mustSpendPubKeyOutput,  
                                                     unspentOutputs)
import           Ledger.Scripts as Scripts          (Redeemer(..))
import           Ledger.Value as Value              (singleton, TokenName(..))
import           Playground.Contract as Playground  (ToSchema)
import qualified Plutus.Contract as Contract        ( 
                                                     awaitTxConfirmed, awaitPromise, 
                                                     Contract, Endpoint, 
                                                     endpoint, handleError, logError, 
                                                     submitTxConstraintsWith, 
                                                     logInfo, utxosAt)
import           Plutus.Contract.Request as Request (ownPaymentPubKeyHash)
import           PlutusPrelude                      (void)
import           PlutusTx                           (toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, 
                                                     Maybe (..), ($), divide,  (-), (*))                             
import qualified Prelude as Haskell                 (Semigroup ((<>)), Show (..), String)
import           Text.Printf                        (printf)


-- | TokenParams are parameters that are passed to the endpoints
data TokenParams = TokenParams
    { 
      tpVersion                     :: !Integer  
    , tpSplit                       :: !Integer
    , tpMerchantPkh                 :: !Address.PaymentPubKeyHash
    , tpDonorPkh                    :: !Address.PaymentPubKeyHash
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)


data RedeemerParams = RedeemerParams
    { 
      rpPolarity                  :: !Bool    -- True = Mint, False = Burn
    , rpOrderId                   :: !BuiltinByteString -- The order number
    , rpAdaAmount                 :: !Integer -- The total amount of the order
    , rpSplit                     :: !Integer -- used for testing 
    , rpMerchantPkh               :: !Address.PaymentPubKeyHash -- used for testing 
    , rpDonorPkh                  :: !Address.PaymentPubKeyHash -- used for testing 
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)


-- | mintETT the order token.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintETToken :: RedeemerParams -> TokenParams -> Contract.Contract () TokenSchema T.Text ()
mintETToken rp tp = do
     
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName "Earthtrust"
                merchSplit = (rpAdaAmount rp) * (rpSplit rp)
                donorSplit = (rpAdaAmount rp) * (100 - (tpSplit tp))
                merchAmount = divide merchSplit 100
                donorAmount = divide donorSplit 100
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpAdaAmount = rpAdaAmount rp
                     }
                mintParams = ETMintPolicyParams 
                    {
                        etpVersion = tpVersion tp
                    ,   etpSplit = tpSplit tp
                    ,   etpMerchantPkh = tpMerchantPkh tp
                    ,   etpDonorPkh = tpDonorPkh tp
                    ,   etpTokenName = tn
                    }
  
            let etVal  = Value.singleton (etCurSymbol mintParams) tn 1
                lookups = Constraints.mintingPolicy (etPolicy mintParams) Haskell.<> 
                          Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValueWithRedeemer red etVal Haskell.<> 
                          Constraints.mustPayToPubKey (rpMerchantPkh rp) (Ada.lovelaceValueOf merchAmount) Haskell.<> 
                          Constraints.mustPayToPubKey (rpDonorPkh rp) (Ada.lovelaceValueOf donorAmount) Haskell.<> 
                          Constraints.mustPayToPubKey (rpDonorPkh rp) (minAda Haskell.<> etVal) Haskell.<> 
                          Constraints.mustSpendPubKeyOutput oref

            ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ Contract.awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @Haskell.String $ printf "mintETT: Forged %s" (Haskell.show etVal)
            Contract.logInfo @Haskell.String $ printf "mintETT: Token params %s" (Haskell.show mintParams)

-- | TokenSchema type is defined and used by the PAB Contracts
type TokenSchema = Contract.Endpoint "mintETT" (RedeemerParams, TokenParams)

useEndpoint :: Contract.Contract () TokenSchema T.Text ()
useEndpoint = forever
              $ Contract.handleError Contract.logError
              $ Contract.awaitPromise
              $ Contract.endpoint @"mintETT" $ \(rp, tp) -> mintETToken rp tp


