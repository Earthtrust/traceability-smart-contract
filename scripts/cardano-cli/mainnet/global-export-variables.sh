#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/admin/src/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
export BECH32=/usr/local/bin/bech32
export CARDANO_NODE_SOCKET_PATH=/ipc/node.socket
export ADMIN_VKEY=
export ADMIN_SKEY=
export ADMIN_PKH=
export MIN_ADA_OUTPUT_TX=2000000
export MIN_ADA_OUTPUT_TX_REF=20000000
export COLLATERAL_ADA=5000000
export MERCHANT_ADDR=addr1v9lw2vr6vn7zj83njcr7xm76npl9vjkuft7j0mfh85zd5rcku807l
export DONOR_ADDR=addr1v9wjlg0e9sz3q5tl4cxu3gaurasdvuvnw5fmnhwefyngphcpugugh
export REFUND_ADDR=addr1v9x5aqdztj3t4356p58cny9zqv8vwggsu4sqraycrat385s8nkxje
export VAL_REF_SCRIPT=85ac254e30cd8326742085b4bce562888423aa7ca81f7e37f490d3b24dcdfe3d#1
export SPLIT=99
export BLOCKFROST_API=https://cardano-mainnet.blockfrost.io/api/v0
