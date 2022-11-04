#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail

# enabled debug flag for bash shell
set -x

# check if command line argument is empty or not present
if [ -z $1 ]; 
then
    echo "refund-tx.sh:  Invalid script arguments"
    echo "Usage: refund-tx.sh [devnet|preview|preprod|mainnet]"
    exit 1
fi
ENV=$1

# Pull in global export variables
MY_DIR=$(dirname $(readlink -f $0))
source $MY_DIR/$ENV/global-export-variables.sh

if [ "$ENV" == "mainnet" ];
then
    network="--mainnet"
else
    network="--testnet-magic $TESTNET_MAGIC"
fi

echo "Socket path: $CARDANO_NODE_SOCKET_PATH"

ls -al "$CARDANO_NODE_SOCKET_PATH"

mkdir -p $WORK
mkdir -p $WORK-backup
rm -f $WORK/*
rm -f $WORK-backup/*


# generate values from cardano-cli tool
$CARDANO_CLI query protocol-parameters $network --out-file $WORK/pparms.json

# load in local variable values
validator_script="$BASE/scripts/cardano-cli/$ENV/data/earthtrust-validator.plutus"
validator_script_addr=$($CARDANO_CLI address build --payment-script-file "$validator_script" $network)
redeemer_file_path="$BASE/scripts/cardano-cli/$ENV/data/redeemer-earthtrust-refund.json"
admin_pkh=$(cat $ADMIN_PKH)


################################################################
# Refund the earthtrust UTXO
################################################################

# Step 1: Get UTXOs from admin
# There needs to be at least 2 utxos that can be consumed; one for spending of the token
# and one uxto for collateral

admin_utxo_addr=$($CARDANO_CLI address build $network --payment-verification-key-file "$ADMIN_VKEY")
$CARDANO_CLI query utxo --address "$admin_utxo_addr" --cardano-mode $network --out-file $WORK/admin-utxo.json

cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace > '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-valid.json
admin_utxo_in=$(echo $admin_utxo_valid_array | tr -d '\n')

cat $WORK/admin-utxo.json | jq -r 'to_entries[] | select(.value.value.lovelace == '$COLLATERAL_ADA' ) | .key' > $WORK/admin-utxo-collateral-valid.json
readarray admin_utxo_valid_array < $WORK/admin-utxo-collateral-valid.json
admin_utxo_collateral_in=$(echo $admin_utxo_valid_array | tr -d '\n')


# Step 2: Get the earthtrust smart contract utxos
$CARDANO_CLI query utxo --address $validator_script_addr $network --out-file $WORK/validator-utxo.json

# Specify the utxo at the smart contract address we want to refund
order_utxo_in="3408fce3b102cea3fcca34085afd5735a51b856ff3840ad6185c59b8284916ec#0"

# Sepcify the amount of the refund
refund_amount=101130000


order_datum_in=$(jq -r 'to_entries[] 
| select (.key == "'$order_utxo_in'") 
| .value.inlineDatum' $WORK/validator-utxo.json)


echo -n "$order_datum_in" > $WORK/datum-in.json

# get the refund address
refund_pkh=$(jq -r '.fields[3].bytes' $WORK/datum-in.json)

echo -n $refund_pkh > $WORK/refund.vkey

refund_addr=$($CARDANO_CLI address build $network --payment-verification-key-file $WORK/refund.vkey)


# Upate the redeemer with the amount of add being added
cat $redeemer_file_path | \
jq -c '
  .fields[0].int          |= '$refund_amount'' > $WORK/redeemer-earthtrust-refund.json 



# Step 3: Build and submit the transaction
$CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  $network \
  --change-address "$admin_utxo_addr" \
  --tx-in-collateral "$admin_utxo_collateral_in" \
  --tx-in "$admin_utxo_in" \
  --tx-in "$order_utxo_in" \
  --spending-tx-in-reference "$VAL_REF_SCRIPT" \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file "$redeemer_file_path" \
  --tx-out "$refund_addr+$refund_amount" \
  --required-signer-hash "$admin_pkh" \
  --protocol-params-file "$WORK/pparms.json" \
  --out-file $WORK/add-ada-tx-alonzo.body

echo "tx has been built"

$CARDANO_CLI transaction sign \
  --tx-body-file $WORK/add-ada-tx-alonzo.body \
  $network \
  --signing-key-file "${ADMIN_SKEY}" \
  --out-file $WORK/add-ada-tx-alonzo.tx

echo "tx has been signed"

#echo "Submit the tx with plutus script and wait 5 seconds..."
#$CARDANO_CLI transaction submit --tx-file $WORK/add-ada-tx-alonzo.tx $network
