# The Traceability Smart Contract

- [Smart Contract Design](#smart-contract-design)
- [Process Flows](#process-flows)
- [System Implementation](#system-implementation)
- [Setup to rebuild plutus scripts](#setting-up-to-re-build-the-plutus-scripts)
- [Support/Issues/Community](#)

## Smart Contract Design
The traceability smart contract consists of a single smart contract where Ada is locked and unlocked.   The Ada is locked as part of buying a product and paying with Ada through a web browser wallet.  The locking Ada transaction also includes a datum that contains key information about the order which is used during the smart contract validation.  The unlocking of an Ada transaction is executed by an admin shell script using the cardano-cli tool.   The admin only has the ability to execute the smart contract and cannot arbitrarily spend the Ada aside from what is defined in the smart contract.   The smart contract has hard coded values for the merchant, donor and refund address.   In addition, the smart contract also validates that the amount of the donation matches the amount defined in the smart contract.    

Below is the smart contract design for the traceability smart contract

![Traceability Smart Design](/images/smart-contract-design.png)



## Process Flows
The following Ada payment flow shows both the spend and refund smart contract execution scenarios.

![Ada Payment Flow](/images/ada-payment-flow.png)


## System Implementation
Below is a simplified view of the components deployed as part of the traceability smart contract launch

![System Component View](/images/system-components.png)



## Setting up to re-build the plutus scripts

### Cabal+Nix build

Use the Cabal+Nix build if you want to develop with incremental builds, but also have it automatically download all dependencies.

Set up your machine to build things with `Nix`, following the [Plutus README](https://github.com/input-output-hk/plutus/blob/master/README.adoc) (make sure to set up the binary cache!).

To enter a development environment, simply open a terminal on the project's root and use `nix-shell` to get a bash shell:

```
$ nix-shell
```

and you'll have a working development environment for now and the future whenever you enter this directory.

The build should not take too long if you correctly set up the binary cache. If it starts building GHC, stop and setup the binary cache.

Afterwards, the command `cabal build` from the terminal should work (if `cabal` couldn't resolve the dependencies, run `cabal update` and then `cabal build`).

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor.
See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

### Run cabal repl to generate the plutus scripts

```
[nix-shell:~/src/traceability-smart-contract]$ cabal repl
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - traceability-0.1.0.0 (lib) (ephemeral targets)
Preprocessing library for traceability-0.1.0.0..
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
...
[1 of 3] Compiling Traceability.V2.Types ( src/Traceability/V2/Types.hs, /home/lawrence/src/traceability-smart-contract/dist-newstyle/build/x86_64-linux/ghc-8.10.7/traceability-0.1.0.0/build/Traceability/V2/Types.o )
[2 of 3] Compiling Traceability.V2.OnChain ( src/Traceability/V2/OnChain.hs, /home/lawrence/src/traceability-smart-contract/dist-newstyle/build/x86_64-linux/ghc-8.10.7/traceability-0.1.0.0/build/Traceability/V2/OnChain.o )
[3 of 3] Compiling Traceability.V2.Deploy ( src/Traceability/V2/Deploy.hs, /home/lawrence/src/traceability-smart-contract/dist-newstyle/build/x86_64-linux/ghc-8.10.7/traceability-0.1.0.0/build/Traceability/V2/Deploy.o )
Ok, three modules loaded.
Prelude Traceability.V2.Deploy> main
Prelude Traceability.V2.Deploy> :q
Leaving GHCi.

```
The new plutus scripts will be created in the traceability-smart-contract/deploy directory

```
[nix-shell:~/src/traceability-smart-contract]$ ls -l deploy/*
-rw-rw-r-- 1 lawrence lawrence   92 Nov 15 09:21 deploy/datum-earthtrust.json
-rw-rw-r-- 1 lawrence lawrence   62 Nov 15 09:21 deploy/earthtrust-validator.hash
-rw-rw-r-- 1 lawrence lawrence 7291 Nov 15 09:21 deploy/earthtrust-validator.plutus
-rw-rw-r-- 1 lawrence lawrence   29 Nov 15 09:21 deploy/redeemer-earthtrust-refund.json
-rw-rw-r-- 1 lawrence lawrence   29 Nov 15 09:21 deploy/redeemer-earthtrust-spend.json

```


## Support/Issues/Community

TBC
