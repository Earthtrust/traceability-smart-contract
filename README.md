# The Traceability Smart Contract


## Smart Contract Design
The traceability smart contract consists of a single smart contract where Ada is locked and unlocked.   The Ada is locked as part of buying a product and paying with Ada through a web browser wallet.  The locking Ada transaction also includes a datum that contains key information about the order which is used during the smart contract validation.  The unlocking of an Ada transaction is executed by an admin shell script using the cardano-cli tool.   The admin only has the ability to execute the smart contract and cannot arbitrarily spend the Ada aside from what is defined in the smart contract.   The smart contract has hard coded values for the merchant, donor and refund address.   In addition, the smart contract also validates that the amount of the donation matches the amount defined in the smart contract.    

Below is the smart contract design for the traceability smart contract

<<image>>



## Process Flows

## System Implementation

## Reporting

