{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Traceability.V2.Deploy
    ( main
    ) where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise                      (serialise)
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Char8                as B (ByteString)
import qualified Data.ByteString.Base16               as B16 (decode)
import qualified Data.ByteString.Lazy                 as LBS (toStrict, writeFile)
import qualified Data.ByteString.Short                as SBS(ShortByteString, toShort)
import           Data.Functor                         (void)
import qualified Ledger.Address                       as Address
import           Ledger.Value                         as Value
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx                             (toBuiltinData)
import           PlutusTx.Prelude                     (BuiltinByteString, Bool(..), Either(..), 
                                                       emptyByteString , Integer, Maybe(..), return,  
                                                       toBuiltin, ($))
import           Prelude                              (IO, String, (.))
import           Traceability.V2.Types
import           Traceability.V2.OnChain


-------------------------------------------------------------------------------------
-- START - traceability Minting Policy Parameters 
-------------------------------------------------------------------------------------
-- These are dummy values and need to be replaced with real values for
-- the appropriate enviornment (eg devnet, testnet or mainnet)
-- **************** WARNING ************************
-- Any changes will require a new deployment of the nftMintingPolicy plutus script
-- 1) cd to top level of project
-- 2) nix-shell
-- 3) cabal repl
-- 4) Deploy> main
-- 5) Deploy> q:
-- 6) Update app with a new nftMintingPolicy address and policy id
-- 7) cd scripts/cardano-cli/[preview|preprod|mainnet]/
-- 8) Update global-export-properties.sh with new Admin UTXO (and collateral)
-- 9) cd ..
-- 10) ./init-tx.sh [preview|preprod|mainnet]
-- 11) Wait for tx to be confirmed on the blockchain before proceeding
-------------------------------------------------------------------------------------

-- Version number
version :: Integer
version = 2

-- Split of the order total amount between merchant and donor
amountSplit :: Integer
amountSplit = 95   -- 95% goes to the merchant, 5% goes to the donor

-- Merchant Pkh
merchantPubKeyHashBS :: B.ByteString
merchantPubKeyHashBS = "3d62bfdff66855d150b6cf97e4509ef78f5ea6245f642adf7629338c"

-- Admin public key payment hash
donorPubKeyHashBS :: B.ByteString
donorPubKeyHashBS = "b2b0a5ceaf7bc9a56fe619819b8891e6bafeff5c2cb275e333f97a9f"

-- Token Name
etTokName :: Value.TokenName
etTokName = "Earthtrust"


-------------------------------------------------------------------------------------
-- END - traceability Minting Policy Parameters 
-------------------------------------------------------------------------------------


-------------------------------------------------------------------------------------
-- START - Derived values
-------------------------------------------------------------------------------------

merchantPaymentPkh :: Address.PaymentPubKeyHash
merchantPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex merchantPubKeyHashBS)

donorPaymentPkh :: Address.PaymentPubKeyHash
donorPaymentPkh = Address.PaymentPubKeyHash (PlutusV2.PubKeyHash $ decodeHex donorPubKeyHashBS)

etMintParams :: ETMintPolicyParams
etMintParams = ETMintPolicyParams 
                {
                  etpVersion = version
                , etpSplit = amountSplit
                , etpMerchantPkh = merchantPaymentPkh
                , etpDonorPkh = donorPaymentPkh
                , etpTokenName = etTokName
                }

-------------------------------------------------------------------------------------
-- END - Derived values 
-------------------------------------------------------------------------------------
 
main::IO ()
main = do

    -- Generate token name 
    writeETTokenName

    -- Generate redeemers
    writeRedeemerMintET

    -- Generate plutus scripts and hashes
    writeETMintingPolicy
    writeETMintingPolicyHash

    return ()


writeETTokenName :: IO ()
writeETTokenName = 
    LBS.writeFile "deploy/token-name.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData etTokName)    


writeRedeemerMintET :: IO ()
writeRedeemerMintET = 
    let red = PlutusV2.Redeemer $ PlutusTx.toBuiltinData $ MintPolicyRedeemer 
             {
                mpPolarity = True     
             ,  mpAdaAmount  = 0   
             }
    in
        LBS.writeFile "deploy/redeemer-mint-token.json" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData red)


writeETMintingPolicy :: IO ()
writeETMintingPolicy = void $ writeFileTextEnvelope "deploy/token-minting-policy.plutus" Nothing serialisedScript
  where
    script :: PlutusV2.Script
    script = PlutusV2.unMintingPolicyScript $ etPolicy etMintParams 

    scriptSBS :: SBS.ShortByteString
    scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

    serialisedScript :: PlutusScript PlutusScriptV2
    serialisedScript = PlutusScriptSerialised scriptSBS


writeETMintingPolicyHash :: IO ()
writeETMintingPolicyHash = 
    LBS.writeFile "deploy/token-minting-policy.id" $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData mph)
  where
    mph = PlutusTx.toBuiltinData $ PSU.V2.mintingPolicyHash $ etPolicy etMintParams




-- | Decode from hex base 16 to a base 10 bytestring is needed because
--   that is how it is stored in the ledger onchain
decodeHex :: B.ByteString -> BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                --putStrLn $ "Tx name: " ++ show t
                toBuiltin(decHex)  
            Left _ -> do
                --putStrLn $ "No Token name: " ++ show e
                emptyByteString 
                
        where        
            getTx :: Either String B.ByteString = B16.decode hexBS
