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
      minAda
    , etCurSymbol
    , etPolicy
    , etTokenValue
    ) where

import           Traceability.V1.Types              (ETMintPolicyParams(..), 
                                                     MintPolicyRedeemer(..))
import           Ledger                             (mkMintingPolicyScript, ScriptContext(..), 
                                                     scriptCurrencySymbol, 
                                                     TxInfo(..))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import qualified Ledger.Address as Address          (Address, pubKeyHashAddress)                                                 
import qualified Ledger.Tx as Tx                    (TxOut(..))
import qualified Ledger.Typed.Scripts as TScripts   (MintingPolicy, wrapMintingPolicy)
import qualified Ledger.Value as Value              (CurrencySymbol, flattenValue, singleton, 
                                                     TokenName(..), Value)
import qualified PlutusTx                           (applyCode, compile, liftCode)
import           PlutusTx.Prelude                   (Bool(..),  
                                                     divide, Integer, Maybe(..), otherwise, 
                                                     traceIfFalse, (&&), (==), ($), (-), (*))

------------------------------------------------------------------------
-- On Chain Code
------------------------------------------------------------------------

{-# INLINABLE minAda #-}
minAda :: Value.Value
minAda = Ada.lovelaceValueOf 2000000

                            
-- | Check that the value is locked at an address for the provided outputs
{-# INLINABLE validOutputs #-}
validOutputs :: Address.Address -> Value.Value -> [Tx.TxOut] -> Bool
validOutputs _ _ [] = False
validOutputs scriptAddr txVal (x:xs)
    | (Tx.txOutAddress x == scriptAddr) && (Tx.txOutValue x == txVal) = True
    | otherwise = validOutputs scriptAddr txVal xs


-- | mkETPolicy is the minting policy is for creating an Earthtrust order token when
--   an order is submitted.
{-# INLINABLE mkETPolicy #-}
mkETPolicy :: ETMintPolicyParams -> MintPolicyRedeemer -> ScriptContext -> Bool
mkETPolicy params (MintPolicyRedeemer polarity adaAmount) ctx = 

    case polarity of
        True ->    traceIfFalse "ETP1" checkMintedAmount
                && traceIfFalse "ETP2" checkMerchantOutput 
                && traceIfFalse "ETP3" checkDonorOutput 
                
        False ->   False   -- no burning allowed

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx  

    split :: Integer
    split = etpSplit params

    tokenName :: Value.TokenName
    tokenName = etpTokenName params

    merchantAddress :: Address.Address
    merchantAddress = Address.pubKeyHashAddress (etpMerchantPkh params) Nothing

    merchantAmount :: Value.Value
    merchantAmount = Ada.lovelaceValueOf (divide (adaAmount * split) 100)

    donorAddress :: Address.Address
    donorAddress = Address.pubKeyHashAddress (etpDonorPkh params) Nothing

    donorAmount :: Value.Value
    donorAmount = Ada.lovelaceValueOf (divide (adaAmount * (100 - split)) 100)


    -- Check that there is only 1 token minted
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tokenName && amt == 1
        _               -> False
          
    -- | Check that both the split amount value is correct and at the correct
    --   address for the merchant     
    checkMerchantOutput :: Bool
    checkMerchantOutput = validOutputs merchantAddress merchantAmount (txInfoOutputs info)

    -- | Check that both the split amount value is correct and at the correct
    --   address for the donor  
    checkDonorOutput :: Bool
    checkDonorOutput = validOutputs donorAddress donorAmount (txInfoOutputs info)


-- | Wrap the minting policy using the boilerplate template haskell code
etPolicy :: ETMintPolicyParams -> TScripts.MintingPolicy
etPolicy mpParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mpParams' -> TScripts.wrapMintingPolicy $ mkETPolicy mpParams' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mpParams


-- | Provide the currency symbol of the minting policy which requires MintPolicyParams
--   as a parameter to the minting policy
{-# INLINABLE etCurSymbol #-}
etCurSymbol :: ETMintPolicyParams -> Value.CurrencySymbol
etCurSymbol mpParams = scriptCurrencySymbol $ etPolicy mpParams 

-- | Return the value of the nftToken
{-# INLINABLE etTokenValue #-}
etTokenValue :: Value.CurrencySymbol -> Value.TokenName -> Value.Value
etTokenValue cs' tn' = Value.singleton cs' tn' 1

