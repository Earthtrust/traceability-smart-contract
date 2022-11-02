{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Traceability.V1.OffChain    
    (   
        ETSchema
    ,   useEndpoint
    ,   ETParams (..)
    ) where


import           Control.Lens                       (review)
import           Control.Monad                      (forever)
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Map as Map                    (singleton, toList, keys)
import           Data.Monoid                        (Last (..))
import           Data.Text as T                     (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (AsContractError (_ConstraintResolutionContractError), awaitPromise, awaitTxConfirmed, Contract, Endpoint, endpoint, handleError, logError, 
                                                    logInfo, mapError, select, submitTxConstraintsWith, tell, throwError, type (.\/), utxosAt)
import           PlutusTx                           (fromBuiltinData, toBuiltinData)
import           PlutusTx.Prelude                   (Bool(..), BuiltinByteString, Integer, Maybe (..), ($))
import           Ledger                             (getCardanoTxId)
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), pubKeyHashAddress, scriptHashAddress)
import           Ledger.Constraints as Constraints  (adjustUnbalancedTx, mintingPolicy, mustMintValueWithRedeemer, mustBeSignedBy, mustPayToPubKey, mustPayToTheScript, mustSpendPubKeyOutput, mustSpendScriptOutput, otherScript, typedValidatorLookups, unspentOutputs)
import           Ledger.Scripts as Scripts          (Datum(..), Redeemer(..))
import qualified Ledger.Tx as Tx                    (ChainIndexTxOut (_ciTxOutValue,_ciTxOutDatum), TxOutRef(..))
import           Ledger.TxId as TxId                (TxId(..))  
import           Ledger.Value as Value              (CurrencySymbol, singleton, split, TokenName(..), valueOf)
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (mkTxContract, submitTxConfirmed, ownPaymentPubKeyHash)
import           Plutus.Contract.Wallet as Wallet   (getUnspentOutput)
import           PlutusPrelude                      (void)
import           PlutusTx.Prelude                   (divide, sha2_256, (+), (-), (++), (==), (*))
import qualified Prelude as Haskell                 (Either(..), return, Semigroup ((<>)), Show (..), String)
import           Text.Printf                        (printf)
import           Traceability.V1.OnChain            (ETDatum(..), etHash, etValidator, typedETValidator, untypedETValidator)
import           Traceability.V1.Types              (ETValidatorParams(..))


-- | ETParams are parameters that are passed to the endpoints
data ETParams = ETParams
    { 
      etpVersion                     :: !Integer  
    , etpSplit                       :: !Integer
    , etpMerchantPkh                 :: !Address.PaymentPubKeyHash
    , etpDonorPkh                    :: !Address.PaymentPubKeyHash
    , testAmount                     :: !Integer
    , testSplit                      :: !Integer
    , testMerchantPkh                :: !Address.PaymentPubKeyHash
    , testDonorPkh                   :: !Address.PaymentPubKeyHash
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Playground.ToSchema)


-- | Find the Earthtrust datum for each utxo at the script address
findETDatum :: ETValidatorParams -> Contract.Contract w s T.Text (Tx.TxOutRef, Tx.ChainIndexTxOut, ETDatum)
findETDatum params = do
    utxos <- Contract.utxosAt $ Address.scriptHashAddress $ etHash $ PlutusTx.toBuiltinData params
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [(oref, o)] -> case Tx._ciTxOutDatum o of
            Haskell.Left _          -> Contract.throwError "findETDatum: datum missing"
            Haskell.Right (Scripts.Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> Contract.throwError "findETDatum: datum has wrong type"
                Just d@ETDatum{} -> Haskell.return (oref, o, d)
        _           -> Contract.throwError "findETDatum: utxo not found"


lockAdaTx :: ETParams -> Contract.Contract () ETSchema T.Text ()
lockAdaTx etp = do

    let etvParams = ETValidatorParams
            {   
                etvVersion        = etpVersion etp
            ,   etvSplit          = etpSplit etp
            ,   etvMerchantPkh    = etpMerchantPkh etp
            ,   etvDonorPkh       = etpDonorPkh etp
            }
        adaAmount = testAmount etp
        etDatum = ETDatum
            {   
                etdAmount = adaAmount                                                 
            }
        dat = PlutusTx.toBuiltinData etDatum
        lookups = Constraints.typedValidatorLookups (typedETValidator $ PlutusTx.toBuiltinData etvParams) Haskell.<> 
                  Constraints.otherScript (etValidator $ PlutusTx.toBuiltinData etvParams) 
        tx = Constraints.mustPayToTheScript dat (Ada.lovelaceValueOf adaAmount)  

    utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    Request.submitTxConfirmed adjustedUtx


unLockAdaTx :: ETParams -> Contract.Contract () ETSchema T.Text ()
unLockAdaTx etp = do

    let etvParams = ETValidatorParams
            {   
                etvVersion        = etpVersion etp
            ,   etvSplit          = etpSplit etp
            ,   etvMerchantPkh    = etpMerchantPkh etp
            ,   etvDonorPkh       = etpDonorPkh etp
            }
        
    (oref, o, etd@ETDatum{}) <- findETDatum etvParams
    Contract.logInfo $ "lockTx: found utxo with datum= " ++ Haskell.show etd
    Contract.logInfo $ "lockTx: found utxo oref= " ++ Haskell.show oref
    Contract.logInfo $ "lockTx: hash= " ++ Haskell.show (etHash $ PlutusTx.toBuiltinData etvParams)

    ownPkh <- Request.ownPaymentPubKeyHash
    let adaAmount = testAmount etp
        split = testSplit etp
        merchantAmount = divide (adaAmount * split) 100
        donorAmount = divide (adaAmount * (100 - split)) 100
        red = Scripts.Redeemer $ PlutusTx.toBuiltinData ()

        lookups = Constraints.typedValidatorLookups (typedETValidator $ PlutusTx.toBuiltinData etvParams) Haskell.<> 
                  Constraints.otherScript (etValidator $ PlutusTx.toBuiltinData etvParams) Haskell.<> 
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx =      Constraints.mustPayToPubKey (testMerchantPkh etp) (Ada.lovelaceValueOf merchantAmount) Haskell.<> 
                  Constraints.mustPayToPubKey (testDonorPkh etp) (Ada.lovelaceValueOf donorAmount) Haskell.<> 
                  Constraints.mustSpendScriptOutput oref red Haskell.<> 
                  Constraints.mustBeSignedBy ownPkh

    utx <- Contract.mapError (review Contract._ConstraintResolutionContractError) (Request.mkTxContract lookups tx)
    let adjustedUtx = Constraints.adjustUnbalancedTx utx
    Request.submitTxConfirmed adjustedUtx

{-


-- | mintETT the order token.   This offchain function is only used by the PAB
--   simulator to test the validation rules of the minting policy validator. 
mintETToken :: RedeemerParams -> ETParams -> Contract.Contract () TokenSchema T.Text ()
mintETToken rp tp = do
     
    ownPkh <- Request.ownPaymentPubKeyHash
    utxos <- Contract.utxosAt (Address.pubKeyHashAddress ownPkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @Haskell.String "mintToken: No utxo found"
        oref : _ -> do
            let tn = Value.TokenName "Earthtrust"
                merchSplit = (rpAdaAmount rp) * (rpSplit rp)
                donorSplit = (rpAdaAmount rp) * (100 - (etpSplit tp))
                merchAmount = divide merchSplit 100
                donorAmount = divide donorSplit 100
                red = Scripts.Redeemer $ toBuiltinData $ MintPolicyRedeemer 
                     {
                        mpPolarity = True  -- mint token
                     ,  mpAdaAmount = rpAdaAmount rp
                     }
                mintParams = ETMintPolicyParams 
                    {
                        etpVersion = etpVersion tp
                    ,   etpSplit = etpSplit tp
                    ,   etpMerchantPkh = etpMerchantPkh tp
                    ,   etpDonorPkh = etpDonorPkh tp
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


-}


-- | ETSchema type is defined and used by the PAB Contracts
type ETSchema = Contract.Endpoint "lock" (ETParams)
                Contract..\/ Contract.Endpoint "unlock" (ETParams)


-- | The endpoints are called via the PAB simulator in the Main-sim.hs file in the app directory
useEndpoint :: Contract.Contract () ETSchema Text ()
useEndpoint = forever $ Contract.handleError Contract.logError $ Contract.awaitPromise $ 
                lockTx `Contract.select`
                unLockTx 
                   
    where
        lockTx = Contract.endpoint @"lock" $ \(tp) -> lockAdaTx tp 
        unLockTx = Contract.endpoint @"unlock" $ \(tp) -> unLockAdaTx tp

