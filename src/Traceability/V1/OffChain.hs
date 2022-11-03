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
import qualified Data.Map as Map                    (singleton, toList)
import           Data.Text as T                     (Text)
import           GHC.Generics                       (Generic)
import qualified Plutus.Contract as Contract        (AsContractError (_ConstraintResolutionContractError), 
                                                     awaitPromise, Contract, Endpoint, endpoint, handleError, 
                                                     logError, 
                                                    logInfo, mapError, select, throwError, type (.\/), utxosAt)
import           PlutusTx                           (fromBuiltinData, toBuiltinData)
import           PlutusTx.Prelude                   (Integer, Maybe (..), ($))
import qualified Ledger.Ada as Ada                  (lovelaceValueOf)
import           Ledger.Address as Address          (PaymentPubKeyHash(..), scriptHashAddress)
import           Ledger.Constraints as Constraints  (adjustUnbalancedTx, mustBeSignedBy, mustPayToPubKey, 
                                                     mustPayToTheScript, mustSpendScriptOutput, otherScript, 
                                                     typedValidatorLookups, unspentOutputs)
import           Ledger.Scripts as Scripts          (Datum(..), Redeemer(..))
import qualified Ledger.Tx as Tx                    (ChainIndexTxOut (_ciTxOutDatum), TxOutRef(..))
import           Playground.Contract as Playground  (ToSchema)
import           Plutus.Contract.Request as Request (mkTxContract, submitTxConfirmed, ownPaymentPubKeyHash)
import           PlutusTx.Prelude                   (divide, (-), (++), (*))
import qualified Prelude as Haskell                 (Either(..), return, Semigroup ((<>)), Show (..))
import           Traceability.V1.OnChain            (ETDatum(..), etHash, etValidator, typedETValidator)
import           Traceability.V1.Types              (ETValidatorParams(..))


-- | ETParams are parameters that are passed to the endpoints
data ETParams = ETParams
    { 
      etpVersion                     :: !Integer  
    , etpSplit                       :: !Integer
    , etpMerchantPkh                 :: !Address.PaymentPubKeyHash
    , etpDonorPkh                    :: !Address.PaymentPubKeyHash
    , etpAdminPkh                    :: !Address.PaymentPubKeyHash
    , testAmount                     :: !Integer
    , datumAmount                    :: !Integer
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
            ,   etvAdminPkh       = etpAdminPkh etp
            }
        adaAmount = testAmount etp
        etDatum = ETDatum
            {   
                etdAmount = datumAmount etp                                                 
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
            ,   etvAdminPkh       = etpAdminPkh etp
            }
        
    (oref, o, etd@ETDatum{}) <- findETDatum etvParams
    Contract.logInfo $ "lockTx: found utxo with datum= " ++ Haskell.show etd
    Contract.logInfo $ "lockTx: found utxo oref= " ++ Haskell.show oref
    Contract.logInfo $ "lockTx: hash= " ++ Haskell.show (etHash $ PlutusTx.toBuiltinData etvParams)

    ownPkh <- Request.ownPaymentPubKeyHash
    let adaAmount = etdAmount etd
        splitAmount = testSplit etp
        merchantAmount = divide (adaAmount * splitAmount) 100
        donorAmount = divide (adaAmount * (100 - splitAmount)) 100
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

