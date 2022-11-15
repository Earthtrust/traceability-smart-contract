#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -o pipefail


# Define export variables

export BASE=/home/admin/src/traceability-smart-contract
export WORK=$BASE/work
export CARDANO_CLI=/usr/local/bin/cardano-cli
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
export VAL_REF_SCRIPT=0a87acbce1a58baa209c6676ab4e0c0afa16d7f53f7fcb36b7a5d2f004f63b43#1
export SPLIT=99
export NEXT_PUBLIC_SHOP="https://test1-2803.myshopify.com/"   # Shopify store URL
export NEXT_PUBLIC_ACCESS_TOKEN="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"
