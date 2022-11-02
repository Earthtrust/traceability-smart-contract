{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main
    ( main
    ) where
        
import           Traceability.V1.OffChain       
import           Control.Monad                          (void)
import           Control.Monad.Freer                    (interpret)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Data.Default                           (def)
import           Ledger.Address                         (PaymentPubKeyHash)
import qualified Ledger.CardanoWallet as CW
import           PabContract                            (Contracts(..))
import           Plutus.PAB.Effects.Contract.Builtin    (Builtin, BuiltinHandler(contractHandler), handleBuiltin)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import           Wallet.Emulator.Wallet                 (knownWallet)


adminPkh :: PaymentPubKeyHash
adminPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 2)

merchantPkh :: PaymentPubKeyHash
merchantPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 8)

donorPkh :: PaymentPubKeyHash
donorPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 9)


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    let w1 = knownWallet 1
        w2 = knownWallet 2
        w3 = knownWallet 3


    --setLocaleEncoding utf8
    Simulator.logString @(Builtin Contracts) "Starting PAB webserver on port 8080"
    shutdown <- PAB.Server.startServerDebug   

    Simulator.logString @(Builtin Contracts) "********* PAB server is running *********"
    Simulator.logString @(Builtin Contracts) "To start PAB simulation press return"
    void $ liftIO getLine
        
    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 1"
    buyer <- Simulator.activateContract w1 UseContract
    admin <- Simulator.activateContract w2 UseContract
    fraud <- Simulator.activateContract w3 UseContract

    ------------------------------------------------------------------------------------------------------
    -- Test Case #1, lock order amount at earthtrust smart contract, and send the correct amount 
    -- to merchant and donor 
    ------------------------------------------------------------------------------------------------------
     
    let etp1 = ETParams
                { 
                  etpVersion = 1 
                , etpSplit = 95       
                , etpMerchantPkh = merchantPkh
                , etpDonorPkh = donorPkh
                , etpAdminPkh = adminPkh 
                , testAmount = 100000000
                , testSplit = 95
                , testMerchantPkh = merchantPkh
                , testDonorPkh = donorPkh
                }


    Simulator.logString @(Builtin Contracts) "-----------------------------------------------------------------------"
    Simulator.logString @(Builtin Contracts) "Test Case #1, send order amount total to earthtrust contract"
    Simulator.logString @(Builtin Contracts) $ show etp1
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance buyer "lock" etp1

    Simulator.waitNSlots 5  

    balances_et1 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et1

    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    Simulator.logString @(Builtin Contracts) "Test Case #1 unlock Ada amount at smart contract for correct amount"
    Simulator.logString @(Builtin Contracts) $ show etp1
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance admin "unlock" etp1

    Simulator.waitNSlots 5   

    balances_et2 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et2


    ------------------------------------------------------------------------------------------------------
    -- Test Case #2, lock order amount at earthtrust smart contract and send incorrect amount
    -- to merchant and donor
    ------------------------------------------------------------------------------------------------------
     
    let etp2 = ETParams
                { 
                  etpVersion = 1 
                , etpSplit = 95       
                , etpMerchantPkh = merchantPkh
                , etpDonorPkh = donorPkh
                , etpAdminPkh = adminPkh 
                , testAmount = 100000000
                , testSplit = 100
                , testMerchantPkh = merchantPkh
                , testDonorPkh = donorPkh
                }


    Simulator.logString @(Builtin Contracts) "-----------------------------------------------------------------------"
    Simulator.logString @(Builtin Contracts) "Test Case #2, send order amount total to earthtrust contract"
    Simulator.logString @(Builtin Contracts) $ show etp2
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance buyer "lock" etp2

    Simulator.waitNSlots 5  

    balances_et3 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et3

    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    Simulator.logString @(Builtin Contracts) "Test Case #2 unlock Ada amount at smart contract for incorrect amount"
    Simulator.logString @(Builtin Contracts) $ show etp2
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance admin "unlock" etp2

    Simulator.waitNSlots 5   

    balances_et4 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et4

    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    ------------------------------------------------------------------------------------------------------
    -- Test Case #3, unlock order amount at earthtrust smart contract to incorrect donor address
    ------------------------------------------------------------------------------------------------------
   
    let etp3 = ETParams
                { 
                  etpVersion = 1 
                , etpSplit = 95       
                , etpMerchantPkh = merchantPkh
                , etpDonorPkh = donorPkh
                , etpAdminPkh = adminPkh 
                , testAmount = 100000000
                , testSplit = 95
                , testMerchantPkh = merchantPkh
                , testDonorPkh = adminPkh
                }

    Simulator.logString @(Builtin Contracts) "Test Case #3 unlock Ada amount at smart contract to incorrect donor address"
    Simulator.logString @(Builtin Contracts) $ show etp3
    Simulator.logString @(Builtin Contracts) "Press return to continue"

    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance admin "unlock" etp3

    Simulator.waitNSlots 5   

    balances_et5 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et5

    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    ------------------------------------------------------------------------------------------------------
    -- Test Case #4, unlock order amount at earthtrust smart contract to incorrect merchant address
    ------------------------------------------------------------------------------------------------------

    let etp4 = ETParams
                { 
                  etpVersion = 1 
                , etpSplit = 95       
                , etpMerchantPkh = merchantPkh
                , etpDonorPkh = donorPkh
                , etpAdminPkh = adminPkh 
                , testAmount = 100000000
                , testSplit = 95
                , testMerchantPkh = adminPkh
                , testDonorPkh = donorPkh
                }

    Simulator.logString @(Builtin Contracts) "Test Case #4 unlock Ada amount at smart contract to incorrect merchant address"
    Simulator.logString @(Builtin Contracts) $ show etp4
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance admin "unlock" etp4

    Simulator.waitNSlots 5   

    balances_et6 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et6

    ------------------------------------------------------------------------------------------------------
    -- Test Case #5, unlock order amount at earthtrust smart contract not being admin
    ------------------------------------------------------------------------------------------------------

    let etp5 = ETParams
                { 
                  etpVersion = 1 
                , etpSplit = 95       
                , etpMerchantPkh = merchantPkh
                , etpDonorPkh = donorPkh
                , etpAdminPkh = adminPkh 
                , testAmount = 100000000
                , testSplit = 95
                , testMerchantPkh = merchantPkh
                , testDonorPkh = donorPkh
                }

    Simulator.logString @(Builtin Contracts) "Test Case #5, unlock order amount at earthtrust smart contract not being admin"
    Simulator.logString @(Builtin Contracts) $ show etp5
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance fraud "unlock" etp5

    Simulator.waitNSlots 5   

    balances_et7 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et7

    shutdown


handlers :: Simulator.SimulatorEffectHandlers (Builtin Contracts)
handlers = Simulator.mkSimulatorHandlers def $ interpret (contractHandler handleBuiltin)

