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

module Traceability.V2.OnChain 
    (
      ETDatum(..)
    , etHash
    , etValidator
    , typedETValidator
    ) where


import           Data.Aeson                                     (FromJSON, ToJSON)
import           GHC.Generics                                   (Generic)
import qualified Ledger.Ada as Ada                              (lovelaceValueOf)
import qualified Ledger.Address as Address                      (Address, pubKeyHashAddress, PaymentPubKeyHash(..))
import qualified Plutus.Script.Utils.Typed as Typed             (Any, validatorScript)
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2       (Validator, ValidatorHash)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2 (TypedValidator)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as ValidatorsV2 (unsafeMkTypedValidator, 
                                                                 validatorHash)
import qualified Ledger.Value as Value                          (Value)
import qualified Plutus.V2.Ledger.Contexts as ContextsV2        (ScriptContext, TxInfo(..), TxInInfo(..),  
                                                                txInfoOutputs, TxOut(..), txOutValue, txSignedBy)
import qualified Plutus.V2.Ledger.Api as PlutusV2               (mkValidatorScript, scriptContextTxInfo,  
                                                                 unsafeFromBuiltinData)
import qualified PlutusTx                                       (applyCode, compile, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                               (Bool(..), BuiltinData, BuiltinByteString, check, divide, Integer, Maybe(..), 
                                                                 otherwise, traceIfFalse, (&&), (==), ($), (-), (*), (+), (<))
import           Prelude                                        (Show (..))
import           Traceability.V2.Types                          (ETRedeemer(..), ETValidatorParams(..))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

-- | The minimum amount of Ada that has to be sent with a transaction
minAda :: Integer
minAda = 1000000

-- | ETDatum is used to record the total amount of the order and the order id
data ETDatum = ETDatum
    {   etdOrderAmount      :: Integer 
    ,   etdOrderId          :: BuiltinByteString
    ,   etdAdaUSDPrice      :: BuiltinByteString                                                                                                           
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeIsDataIndexed ''ETDatum [('ETDatum, 0)]
PlutusTx.makeLift ''ETDatum

                                                      
-- | Check that the value is there for the provided outputs
{-# INLINABLE validOutput #-}
validOutput :: Value.Value -> [ContextsV2.TxOut] -> Bool
validOutput _ [] = False
validOutput txVal (x:xs)
    | (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutput txVal xs
    
                                                      
-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutput' #-}
validOutput' :: Address.Address -> Value.Value -> [ContextsV2.TxOut] -> Bool
validOutput' _ _ [] = False
validOutput' scriptAddr txVal (x:xs)
    | (ContextsV2.txOutAddress x == scriptAddr) && (ContextsV2.txOutValue x == txVal) = True
    | otherwise = validOutput' scriptAddr txVal xs


-- | Check to see if the value is part of the input
{-# INLINABLE validInput #-}
validInput :: Value.Value -> [ContextsV2.TxInInfo] -> Bool
validInput _ [] = False
validInput txVal (x:xs)
    | validOutput txVal [ContextsV2.txInInfoResolved x] = True
    | otherwise = validInput txVal xs


-- | mkETValidator is the validator that confirms if it is ok to spend the Ada locked
--   at the earthtrust smart contract.   All of the business rules must be satisfied in
--   order to spend the transaction.
{-# INLINABLE mkETValidator #-}
mkETValidator :: ETValidatorParams -> ETDatum -> ETRedeemer -> ContextsV2.ScriptContext -> Bool
mkETValidator params dat red ctx = 
    case red of
        Spend ->   traceIfFalse "ETV1" checkInput  
                && traceIfFalse "ETV2" checkDonorOutput 
                && traceIfFalse "ETV3" checkMerchantOutput 
                && traceIfFalse "ETV4" signedByAdmin
        Refund ->  traceIfFalse "ETV5" checkInput
                && traceIfFalse "ETV6" checkRefundOutput
                && traceIfFalse "ETV7" signedByAdmin
                
    where
        info :: ContextsV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx  

        split :: Integer
        split = etvSplit params

        serviceFee :: Integer
        serviceFee = etvServiceFee params

        adaOrderAmount :: Integer
        adaOrderAmount = etdOrderAmount dat
        
        adaDonationAmount :: Integer
        adaDonationAmount 
            | donationAmount < minAda = minAda
            | otherwise = donationAmount

            where
                donationAmount = (divide (adaOrderAmount * (100 - split)) 100)
        

        -- | Admin signature required to run the smart contract
        signedByAdmin :: Bool
        signedByAdmin =  ContextsV2.txSignedBy info $ Address.unPaymentPubKeyHash (etvAdminPkh params)

        -- | Checks that the amount in the datum matches the actual amount in the input
        --   transaction
        checkInput :: Bool
        checkInput = validInput (Ada.lovelaceValueOf (adaOrderAmount + serviceFee)) (ContextsV2.txInfoInputs info)

        -- | Check that both the split amount value is correct and at the correct
        --   address for the merchant     
        checkMerchantOutput :: Bool
        checkMerchantOutput = validOutput' merchantAddress merchantAmount (ContextsV2.txInfoOutputs info)
          where
            merchantAmount :: Value.Value
            merchantAmount = Ada.lovelaceValueOf (adaOrderAmount - adaDonationAmount)

            merchantAddress :: Address.Address
            merchantAddress = Address.pubKeyHashAddress (etvMerchantPkh params) Nothing


        -- | Check that both the split amount value is correct and at the correct
        --   address for the donor  
        checkDonorOutput :: Bool
        checkDonorOutput = validOutput' donorAddress donorAmount (ContextsV2.txInfoOutputs info)
          where
            donorAmount :: Value.Value
            donorAmount = Ada.lovelaceValueOf adaDonationAmount
            
            donorAddress :: Address.Address
            donorAddress = Address.pubKeyHashAddress (etvDonorPkh params) Nothing


        -- | Check that the refund output contains the correct ada amount 
        checkRefundOutput :: Bool
        checkRefundOutput = validOutput' refundAddress refundAmount (ContextsV2.txInfoOutputs info)
          where
            refundAmount :: Value.Value
            refundAmount = Ada.lovelaceValueOf adaOrderAmount
            
            refundAddress :: Address.Address
            refundAddress = Address.pubKeyHashAddress (etvRefundPkh params) Nothing
            


-- | Creating a wrapper around the validator for 
--   performance improvements by not using a typed validator
{-# INLINABLE wrapETValidator #-}
wrapETValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapETValidator params dat red ctx =
   check $ mkETValidator (PlutusV2.unsafeFromBuiltinData params) (PlutusV2.unsafeFromBuiltinData dat) (PlutusV2.unsafeFromBuiltinData red) (PlutusV2.unsafeFromBuiltinData ctx)


untypedETValidator :: BuiltinData -> PSU.V2.Validator
untypedETValidator params = PlutusV2.mkValidatorScript $
    $$(PlutusTx.compile [|| wrapETValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params
    

-- | We need a typedValidator for offchain mkTxConstraints, so 
--   created it using the untyped validator
typedETValidator :: BuiltinData -> PSU.V2.TypedValidator Typed.Any
typedETValidator params =
  ValidatorsV2.unsafeMkTypedValidator $ untypedETValidator params


etValidator :: BuiltinData -> PSU.V2.Validator
etValidator params = Typed.validatorScript $ typedETValidator params


etHash :: BuiltinData -> PSU.V2.ValidatorHash
etHash params = ValidatorsV2.validatorHash  $ typedETValidator params
