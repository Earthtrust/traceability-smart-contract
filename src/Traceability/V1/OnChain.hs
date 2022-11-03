{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-} 
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Traceability.V1.OnChain 
    (
      ETDatum(..)
    , etHash
    , etValidator
    , typedETValidator
    , untypedETValidator
    ) where

import           Data.Aeson                         (FromJSON, ToJSON)
import           GHC.Generics                       (Generic)
import           Ledger                             (ScriptContext(..), TxInfo(..),  txSignedBy, )
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (Address, PaymentPubKeyHash(..), pubKeyHashAddress)
import qualified Ledger.Contexts as Contexts        (TxInInfo(..), TxInInfo(txInInfoResolved))
import qualified Ledger.Scripts as Scripts          (mkValidatorScript, Script, Validator, ValidatorHash)                                                  
import qualified Ledger.Tx as Tx                    (TxOut(..))
import qualified Ledger.Typed.Scripts.Validators as Validators (unsafeMkTypedValidator)
import qualified Ledger.Typed.TypeUtils as TypeUtils (Any)
import qualified Ledger.Typed.Scripts as TScripts   (TypedValidator, validatorScript, validatorHash)
import qualified Ledger.Value as Value              (Value)
import           Plutus.V1.Ledger.Api as Ledger     (unsafeFromBuiltinData, unValidatorScript)
import qualified PlutusTx                           (applyCode, compile, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                   (Bool(..), BuiltinData, BuiltinByteString, check, divide, Integer, 
                                                     Maybe(..),otherwise, traceIfFalse, (&&), (==), 
                                                     ($), (*), (-), (+))
import qualified Prelude as Haskell                 (Show)
import           Traceability.V1.Types              (ETRedeemer(..), ETValidatorParams(..))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- | ETDatum is used to record the total amount of the order
data ETDatum = ETDatum
    {   etdAmount           :: Integer 
    ,   etdOrderId          :: BuiltinByteString
    ,   etdServiceFee       :: Integer
    ,   etdRefundAddr       :: !Address.PaymentPubKeyHash                                                                                                            
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ETDatum [('ETDatum, 0)]
PlutusTx.makeLift ''ETDatum

                            
-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Address.Address -> Value.Value -> [Tx.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (Tx.txOutAddress x == scriptAddr) && (Tx.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs


-- | Check that the value is there for the provided outputs
{-# INLINABLE validOutput #-}
validOutput :: Value.Value -> [Tx.TxOut] -> Bool
validOutput _ [] = False
validOutput txVal (x:xs)
    | (Tx.txOutValue x == txVal) = True
    | otherwise = validOutput txVal xs


-- | Check to see if the buy token is in the list of inputs locked at an address
{-# INLINABLE validInput #-}
validInput :: Value.Value -> [Contexts.TxInInfo] -> Bool
validInput _ [] = False
validInput txVal (x:xs)
    | validOutput txVal [Contexts.txInInfoResolved x] = True
    | otherwise = validInput txVal xs


-- | mkETValidator is the minting policy is for creating an Earthtrust order token when
--   an order is submitted.
{-# INLINABLE mkETValidator #-}
mkETValidator :: ETValidatorParams -> ETDatum -> ETRedeemer -> ScriptContext -> Bool
mkETValidator params dat red ctx = 
    case red of
        Spend -> traceIfFalse "ETV1" checkMerchantOutput 
                && traceIfFalse "ETV2" checkDonorOutput 
                && traceIfFalse "ETV3" checkInput  
                && traceIfFalse "ETV4" signedByAdmin
        Refund amt -> (traceIfFalse "ETV5" $ checkRefundOutput amt)
                && (traceIfFalse "ETV6" $ checkRefundInput amt)
                && traceIfFalse "ETV7" signedByAdmin
                
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        split :: Integer
        split = etvSplit params

        adaAmount :: Integer
        adaAmount = etdAmount dat

        serviceFee :: Integer
        serviceFee = etdServiceFee dat

        merchantAddress :: Address.Address
        merchantAddress = Address.pubKeyHashAddress (etvMerchantPkh params) Nothing

        merchantAmount :: Value.Value
        merchantAmount = Ada.lovelaceValueOf (divide (adaAmount * split) 100)

        donorAddress :: Address.Address
        donorAddress = Address.pubKeyHashAddress (etvDonorPkh params) Nothing

        refundAddress :: Address.Address
        refundAddress = Address.pubKeyHashAddress (etdRefundAddr dat) Nothing

        donorAmount :: Value.Value
        donorAmount = Ada.lovelaceValueOf (divide (adaAmount * (100 - split)) 100)

        -- | Admin signature required to run the smart contract
        signedByAdmin :: Bool
        signedByAdmin =  txSignedBy info $ Address.unPaymentPubKeyHash (etvAdminPkh params)

        -- | Check that both the split amount value is correct and at the correct
        --   address for the merchant     
        checkMerchantOutput :: Bool
        checkMerchantOutput = validOutputs merchantAddress merchantAmount (txInfoOutputs info)

        -- | Check that both the split amount value is correct and at the correct
        --   address for the donor  
        checkDonorOutput :: Bool
        checkDonorOutput = validOutputs donorAddress donorAmount (txInfoOutputs info)

        -- | Checks that the amount in the datum matches the actual amount in the input
        --   transaction
        checkInput :: Bool
        checkInput = validInput (Ada.lovelaceValueOf (adaAmount + serviceFee)) (txInfoInputs info)

        -- | Checks that the amount in the datum matches the actual amount in the input
        --   transaction
        checkRefundInput :: Integer -> Bool
        checkRefundInput refundAmt = validInput (Ada.lovelaceValueOf (refundAmt + serviceFee)) (txInfoInputs info)

        -- | Check that the refund output contains the correct ada amount and
        --   goes to the refund address
        checkRefundOutput :: Integer -> Bool
        checkRefundOutput refundAmt = validOutputs refundAddress (Ada.lovelaceValueOf refundAmt) (txInfoOutputs info)


-- | Creating a wrapper around littercoin validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapETValidator #-}
wrapETValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapETValidator params dat red ctx =
   check $ mkETValidator (unsafeFromBuiltinData params) (unsafeFromBuiltinData dat) (unsafeFromBuiltinData red) (unsafeFromBuiltinData ctx)


untypedETValidator :: BuiltinData -> Scripts.Validator
untypedETValidator params = Scripts.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapETValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    

-- | We need a typedValidator for offchain mkTxConstraints, so 
-- created it using the untyped validator
typedETValidator :: BuiltinData -> TScripts.TypedValidator TypeUtils.Any
typedETValidator params =
  Validators.unsafeMkTypedValidator $ untypedETValidator params


mkETScript :: BuiltinData -> Scripts.Script
mkETScript params = unValidatorScript $ untypedETValidator params


etValidator :: BuiltinData -> Scripts.Validator
etValidator params = TScripts.validatorScript $ typedETValidator params


etHash :: BuiltinData -> Scripts.ValidatorHash
etHash params = TScripts.validatorHash $ typedETValidator params

