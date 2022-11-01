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
import           PlutusTx.Prelude                       (BuiltinByteString)
import           Wallet.Emulator.Wallet                 (knownWallet)

merchantPkh :: PaymentPubKeyHash
merchantPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 8)

donorPkh :: PaymentPubKeyHash
donorPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 9)

fraudPkh :: PaymentPubKeyHash
fraudPkh = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 10)


main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do

    let w1 = knownWallet 1
        w2 = knownWallet 2
        w3 = knownWallet 3
        w4 = knownWallet 4
        w5 = knownWallet 5
        w6 = knownWallet 6
        tp = TokenParams
            {   tpVersion = 1 :: Integer
            ,   tpSplit = 95 :: Integer
            ,   tpMerchantPkh = merchantPkh
            ,   tpDonorPkh = donorPkh
            }

    --setLocaleEncoding utf8
    Simulator.logString @(Builtin Contracts) "Starting PAB webserver on port 8080"
    shutdown <- PAB.Server.startServerDebug   

    Simulator.logString @(Builtin Contracts) "********* PAB server is running *********"
    Simulator.logString @(Builtin Contracts) "To start PAB simulation press return"
    void $ liftIO getLine
        
    Simulator.logString @(Builtin Contracts) "Initializing contract handle for wallet 1"
    h1 <- Simulator.activateContract w1 UseContract

    ------------------------------------------------------------------------------------------------------
    -- Test Case #1, mint multiple tokens at different addresses for each order number, with correct split
    ------------------------------------------------------------------------------------------------------
     
    let rp1 = RedeemerParams
            {
              rpPolarity = True                         -- Mint     
            , rpOrderId = "123" :: BuiltinByteString    -- Order Id
            , rpAdaAmount = 100000000 :: Integer        -- 100 Ada
            , rpSplit = 95 :: Integer
            , rpMerchantPkh = merchantPkh
            , rpDonorPkh = donorPkh
            }
    Simulator.logString @(Builtin Contracts) "-----------------------------------------------------------------------"
    Simulator.logString @(Builtin Contracts) "Test Case #1, mint multiple tokens at different address for each order number with correct split"
    Simulator.logString @(Builtin Contracts) "Test Case #1, Order #123"
    Simulator.logString @(Builtin Contracts) $ show tp
    Simulator.logString @(Builtin Contracts) $ show rp1
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance h1 "mintETT" (rp1, tp)

    Simulator.waitNSlots 5  

    h2 <- Simulator.activateContract w2 UseContract

    let rp2 = RedeemerParams
            {
              rpPolarity = True                         -- Mint     
            , rpOrderId = "124" :: BuiltinByteString    -- Order Id
            , rpAdaAmount = 100000000 :: Integer        -- 100 Ada
            , rpSplit = 95 :: Integer
            , rpMerchantPkh = merchantPkh
            , rpDonorPkh = donorPkh
            }

    Simulator.logString @(Builtin Contracts) "Test Case #1, Order #124"
    Simulator.logString @(Builtin Contracts) $ show tp
    Simulator.logString @(Builtin Contracts) $ show rp2
    --Simulator.logString @(Builtin Contracts) "Press return to continue"
    --void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance h2 "mintETT" (rp2, tp)

    Simulator.waitNSlots 5   

    h3 <- Simulator.activateContract w3 UseContract   

    let rp3 = RedeemerParams
            {
              rpPolarity = True                         -- Mint     
            , rpOrderId = "125" :: BuiltinByteString    -- Order Id
            , rpAdaAmount = 100000000 :: Integer        -- 100 Ada
            , rpSplit = 95 :: Integer
            , rpMerchantPkh = merchantPkh
            , rpDonorPkh = donorPkh
            }

    Simulator.logString @(Builtin Contracts) "Test Case #1, Order #125"
    Simulator.logString @(Builtin Contracts) $ show tp
    Simulator.logString @(Builtin Contracts) $ show rp3
    --Simulator.logString @(Builtin Contracts) "Press return to continue"
    --void $ liftIO getLine
    void $ Simulator.callEndpointOnInstance h3 "mintETT" (rp3, tp)

    Simulator.waitNSlots 5    

    balances_et1 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et1


    ------------------------------------------------------------------------------------------------------
    -- Test Case #2, mint order token with incorrect split
    ------------------------------------------------------------------------------------------------------
    
    h4 <- Simulator.activateContract w4 UseContract  

    let rp4 = RedeemerParams
            {
              rpPolarity = True                        -- Mint     
            , rpOrderId = "126" :: BuiltinByteString   -- Order Id
            , rpAdaAmount = 100000000 :: Integer       -- 100 Ada
            , rpSplit = 100 :: Integer
            , rpMerchantPkh = merchantPkh
            , rpDonorPkh = donorPkh
            }

    Simulator.logString @(Builtin Contracts) "-----------------------------------------------------------------------"
    Simulator.logString @(Builtin Contracts) "Test Case #2, mint order token with incorrect split"
    Simulator.logString @(Builtin Contracts) $ show tp
    Simulator.logString @(Builtin Contracts) $ show rp4
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    void $ Simulator.callEndpointOnInstance h4 "mintETT" (rp4, tp)

    Simulator.waitNSlots 5    

    balances_et2 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et2


    ------------------------------------------------------------------------------------------------------
    -- Test Case #3, send funds to incorrect merchant wallet
    ------------------------------------------------------------------------------------------------------

    h5 <- Simulator.activateContract w5 UseContract

    let rp5 = RedeemerParams
            {
              rpPolarity = True                        -- Mint     
            , rpOrderId = "127" :: BuiltinByteString   -- Order Id
            , rpAdaAmount = 100000000 :: Integer       -- 100 Ada
            , rpSplit = 95 :: Integer
            , rpMerchantPkh = fraudPkh
            , rpDonorPkh = donorPkh
            }

    Simulator.logString @(Builtin Contracts) "-----------------------------------------------------------------------"
    Simulator.logString @(Builtin Contracts) "Test Case #3, send funds to incorrect merchant wallet"
    Simulator.logString @(Builtin Contracts) $ show tp
    Simulator.logString @(Builtin Contracts) $ show rp5
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    void $ Simulator.callEndpointOnInstance h5 "mintETT" (rp5, tp)

    Simulator.waitNSlots 5    

    balances_et3 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et3


    ------------------------------------------------------------------------------------------------------
    -- Test Case #4, send funds to incorrect donor wallet
    ------------------------------------------------------------------------------------------------------

    h6 <- Simulator.activateContract w6 UseContract

    let rp6 = RedeemerParams
            {
              rpPolarity = True                        -- Mint     
            , rpOrderId = "128" :: BuiltinByteString   -- Order Id
            , rpAdaAmount = 100000000 :: Integer       -- 100 Ada
            , rpSplit = 95 :: Integer
            , rpMerchantPkh = merchantPkh
            , rpDonorPkh = fraudPkh
            }

    Simulator.logString @(Builtin Contracts) "-----------------------------------------------------------------------"
    Simulator.logString @(Builtin Contracts) "Test Case #4, send funds to incorrect donor wallet"
    Simulator.logString @(Builtin Contracts) $ show tp
    Simulator.logString @(Builtin Contracts) $ show rp6
    Simulator.logString @(Builtin Contracts) "Press return to continue"
    void $ liftIO getLine

    void $ Simulator.callEndpointOnInstance h6 "mintETT" (rp6, tp)

    Simulator.waitNSlots 5    

    balances_et4 <- Simulator.currentBalances
    Simulator.logBalances @(Builtin Contracts) balances_et4

    shutdown


handlers :: Simulator.SimulatorEffectHandlers (Builtin Contracts)
handlers = Simulator.mkSimulatorHandlers def $ interpret (contractHandler handleBuiltin)

