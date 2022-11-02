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
import           Ledger                             (ScriptContext(..), scriptCurrencySymbol, 
                                                     TxInfo(..),  txSignedBy, TxId(getTxId ))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (Address, PaymentPubKeyHash(..), pubKeyHashAddress)
import qualified Ledger.Contexts as Contexts        (spendsOutput, TxOut)
import qualified Ledger.Scripts as Scripts          (Datum(..), DatumHash, mkValidatorScript, Script, Validator, ValidatorHash, validatorHash)                                                  
import qualified Ledger.Tx as Tx                    (TxOut(..), TxOutRef(..))
import qualified Ledger.Typed.Scripts.Validators as Validators (unsafeMkTypedValidator)
import qualified Ledger.Typed.TypeUtils as TypeUtils (Any)
import qualified Ledger.Typed.Scripts as TScripts   (TypedValidator, validatorScript, validatorHash)
import qualified Ledger.Value as Value              (flattenValue, singleton, TokenName(..), Value)
import           Plutus.V1.Ledger.Api as Ledger     (unsafeFromBuiltinData, unValidatorScript)
import qualified PlutusTx                           (applyCode, compile, fromBuiltinData, liftCode, makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, BuiltinData, check, divide, find, Integer, Maybe(..),otherwise, traceIfFalse, (&&), (==), ($), (<=), (>=), (<>), (<$>), (-), (*), (+))
import qualified Prelude as Haskell                 (Show)
import           Traceability.V1.Types              (ETValidatorParams(..))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------


-- | ETDatum is used to record the amount of Littercoin minted and the amount
--   of Ada locked at the smart contract.  This is then used during Littercoin
--   burning to payout the corresponding amount of Ada per Littercoin to the merchant.
data ETDatum = ETDatum
    {   etdAmount           :: Integer                                                                                                             
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


-- | mkETValidator is the minting policy is for creating an Earthtrust order token when
--   an order is submitted.
{-# INLINABLE mkETValidator #-}
mkETValidator :: ETValidatorParams -> ETDatum -> () -> ScriptContext -> Bool
mkETValidator params dat _ ctx = 
    traceIfFalse "ETV1" checkMerchantOutput 
    && traceIfFalse "ETV2" checkDonorOutput 
    -- check that must be signed by admin
                
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    split :: Integer
    split = etvSplit params

    adaAmount :: Integer
    adaAmount = etdAmount dat

    merchantAddress :: Address.Address
    merchantAddress = Address.pubKeyHashAddress (etvMerchantPkh params) Nothing

    merchantAmount :: Value.Value
    merchantAmount = Ada.lovelaceValueOf (divide (adaAmount * split) 100)

    donorAddress :: Address.Address
    donorAddress = Address.pubKeyHashAddress (etvDonorPkh params) Nothing

    donorAmount :: Value.Value
    donorAmount = Ada.lovelaceValueOf (divide (adaAmount * (100 - split)) 100)

    -- | Check that both the split amount value is correct and at the correct
    --   address for the merchant     
    checkMerchantOutput :: Bool
    checkMerchantOutput = validOutputs merchantAddress merchantAmount (txInfoOutputs info)

    -- | Check that both the split amount value is correct and at the correct
    --   address for the donor  
    checkDonorOutput :: Bool
    checkDonorOutput = validOutputs donorAddress donorAmount (txInfoOutputs info)


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

