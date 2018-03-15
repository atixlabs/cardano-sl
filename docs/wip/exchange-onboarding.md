Table of Contents
=================

   * [Communication](#communication)
   * [Requirements](#requirements)
      * [Nix](#nix)
         * [Optional: Enable IOHK's binary cache](#optional-enable-iohks-binary-cache)
      * [Miscellaneous Utilities](#miscellaneous-utilities)
   * [Wallet](#wallet)
      * [Backup local state](#backup-local-state)
      * [Fetch latest code](#fetch-latest-code)
      * [Generate custom configuration](#generate-custom-configuration)
      * [Build and run in the nix store](#build-and-run-in-the-nix-store)
      * [Build and run a docker image](#build-and-run-a-docker-image)
   * [Migrating from V0 to V1 API](#migrating-from-v0-to-v1-api)
   * [Usage FAQs](#usage-faqs)
      * [What are recommended hardware/software requirements for exchange wallets?](#what-are-recommended-hardwaresoftware-requirements-for-exchange-wallets)
      * [How do I customize the wallet configuration?](#how-do-i-customize-the-wallet-configuration)
      * [How do I export the CA certificate for the API?](#how-do-i-export-the-ca-certificate-for-the-api)
      * [How do I know when the wallet has fetched all the blocks?](#how-do-i-know-when-the-wallet-has-fetched-all-the-blocks)
      * [Creating a New Wallet](#creating-a-new-wallet)
      * [Sending Money](#sending-money)
      * [Where can I find the API documentation?](#where-can-i-find-the-api-documentation)
      * [How can I inspect runtime metrics and statistics?](#how-can-i-inspect-runtime-metrics-and-statistics)

# Communication

* An e-mail list needs to be provided by the exchange for IOHK to broadcast issues and upddates.
* A slack room will be created for each exchange for direct communication with IOHK team.

# Requirements

## Nix

The wallet is built using [nix package manager](https://nixos.org/nix/). To install it on
most Linux distros download and run the installation script.

    curl https://nixos.org/nix/install > install-nix.sh
    . install-nix.sh

Follow the directions and then log out and back in.

### Optional: Enable IOHK's binary cache

Skip this section if you prefer to build all code from IOHK
locally. When the binary cache is enabled build steps will tend
go faster.

    sudo mkdir -p /etc/nix
    cat <<EOF | sudo tee /etc/nix/nix.conf
    binary-caches            = https://cache.nixos.org https://hydra.iohk.io
    binary-cache-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    EOF

## Miscellaneous Utilities

Use `nix` to install essential utilities.

    nix-env -i git

# Wallet

## Backup local state

Skip to the next section if this is target machine doesn't yet have
`cardano-sl` set up.

To avoid catastrophic data loss, stop the wallet and backup the
local databases, keys, certificates, and logs. By default, the
local state will be in `./state-wallet-mainnet`, but may be
elsewhere (see `stateDir` attribute in `./custom-wallet-config.nix`).

## Fetch latest code

Clone the [cardano-sl repository](https://github.com/input-output-hk/cardano-sl) or `cd` into a preexisting copy.

    git clone https://github.com/input-output-hk/cardano-sl.git
    cd cardano-sl

Switch to the `master` branch and pull the latest code.

    git checkout master
    git pull

Dump the current revision and confirm with IOHK whether it is as
expected.

    git rev-parse HEAD

## Generate custom configuration

The `cardano-sl` repo defaults to using end user topology and settings.
For exchanges we recommend creating the following `custom-wallet-config.nix`
prior to building `cardano-sl`:

    {
      #walletListen = "127.0.0.1:8090";
      #ekgListen = "127.0.0.1:8000";
      stateDir = "./state-wallet-mainnet";
      topologyFile = ./exchange-topology.yaml;
    
      ## See https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/runtime_control.html#running-a-compiled-program
      #ghcRuntimeArgs = "-N2 -qg -A1m -I0 -T";
    
      ## Primarily used for troubleshooting.
      #additionalNodeArgs = "";
    }

The rest of this document will assume the above configuration file. Please
alter any commands for example related to `stateDir` path to reflect your
setup.

You will also need to add the following `exchange-topology.yaml` file to use the
private relays:

    TODO: private relays should be inserted here in document given to exchanges


## Build and run in the nix store

By default the wallet's local state goes in
`./state-wallet-mainnet`. If you prefer to have this sensitive data
outside the git repository, see the FAQ about supported custom
configuration settings (See section 2.4.1).

Build the wallet and generate the shell script to connect to
mainnet (use `connectScripts.stagingWallet` for testnet)

    nix-build -A connectScripts.mainnetWallet -o "./launch_$(date -I)_$(git rev-parse --short HEAD)"

After the build finishes the generated connection script is
available as a symlink called `./launch_2018-01-30_0d4f79eea`, or
similar. Run that symlink as a script to start the wallet.

## Build and run a docker image

Follow the above instructions for customization and dependencies. To build a docker
container and import the image run
(use `connectScripts.stagingWallet` for testnet):

    docker load < $(nix-build -A dockerImages.mainnetWallet))

This will create an image `cardano-container-mainnet:latest`
(or `cardano-container-staging:latest` for testnet)

After this image is built, it can be used like any other docker image being pushed
into a registry and pulled down using your preferred docker orchestration tool.

The image can be ran using the following:

    docker run --name cardano-mainnet-wallet --rm -it -p 127.0.0.1:8090:8090 -p 127.0.0.1:8000:8000 -v state-wallet-mainnet:/wallet cardano-container-mainnet:latest

The above command will create a docker volume named `state-wallet-mainnet` and will mount
that to /wallet. Note: if no volume is mounted to `/wallet` the container startup
script will refused to execute `cardano-node` and the container will exit.

The location of `/wallet` cannot be changed, but you can mount any kind of volume
you want in that directory that docker supports.

Note that if you give this a different name than is specified above, use the
name you used for any future docker commands in examples in the document.

# Migrating from V0 to V1 API

# Usage FAQs

## What are recommended hardware/software requirements for exchange wallets?

RAM: 8 GB for building, 4 GB for running
CPU: ???
Disk: SSD recommended of ??? size
Operating System: NixOS or CentOS 7 recommended, although any linux distribution should work with nix package manager used for building the binaries
Software Requirements: Nix package manager for building standalone and docker containers. On systems using docker, docker > ??? required.

## How do I customize the wallet configuration?

Before building the wallet copy `./sample-wallet-config.nix` to
`./custom-wallet-config.nix` and edit as needed.

Supported options include:

-   **`walletListen`:** Wallet API server
-   **`ekgListen`:** Runtime metrics server
-   **`stateDir`:** Directory for the wallet's local state. Must be
    enclosed in double quotes.
-   **`topologyFile`:** Used to connect to a custom set of nodes on
    the network. When unspecified an appropriate
    default topology is generated.

## How do I export the CA certificate for the API?

The certificate is generated inside the wallet in the file `tls/server.cert`

If you are using the docker container, this can be output using the command:

`docker exec -it cardano-mainnet-wallet cat /wallet/state-wallet-mainnet/tls/server.cert`

Please refer to your OS or browser documentation for how to import the CA
certificate into your trusted `ca-certificates` file. The rest of this
document will assume the certificate is trusted.

## How do I know when the wallet has fetched all the blocks?

You can check the sync progress using the API to get detailed json output:

    curl -X GET "https://127.0.0.1:8090/api/v1/node-info" -H "accept: application/json;charset=utf-8"
    {"data":{"syncProgress":{"quantity":100,"unit":"percent"},"blockchainHeight":{"quantity":738268,"unit":"blocks"},"localBlockchainHeight":{"quantity":738268,"unit":"blocks"},"localTimeDifference":{"quantity":0,"unit":"microseconds"}},"status":"success","meta":{"pagination":{"totalPages":1,"page":1,"perPage":1,"totalEntries":1}}}

The following command can be used to see the percentage completion of the sync only:

    nix-shell -p jq curl --run 'curl -X GET "https://127.0.0.1:8090/api/v1/node-info" -H "accept: application/json;charset=utf-8" | jq .data.syncProgress.quantity'

## Creating a New Wallet
You can create your first wallet using the POST /api/v1/wallets endpoint as follow:

    curl -X POST https://localhost:8090/api/v1/wallets                     \
         -H "Content-Type: application/json; charset=utf-8"                \
         -H "Accept: application/json; charset=utf-8"                      \
         --cacert ./scripts/tls-files/ca.crt                               \
         -d '{                                                             \
      "operation": "create",                                               \
      "backupPhrase": ["squirrel", "material", "silly", "twice", "direct", \
        "slush", "pistol", "razor", "become", "junk", "kingdom", "flee"],  \
      "assuranceLevel": "normal",                                          \
      "name": "MyFirstWallet"                                              \
    }'
Warning: Those 12 mnemonic words given for the backup phrase act as an example. Do not use them on a production system. See the section below about mnemonic codes for more information.

As a response, the API provides you with a wallet id used in subsequent requests to uniquely identity the wallet. Make sure to store it / write it down. Note that every API response is jsend-compliant; Cardano also augments responses with meta-data specific to pagination. More details in the section below about Pagination.

    {
        "status": "success",
        "data": {
            "id": "Ae2tdPwUPE...8V3AVTnqGZ",
            "name": "MyFirstWallet",
            "balance": 0
        },
        "meta": {
            "pagination": {
                "totalPages": 1,
                "page": 1,
                "perPage": 1,
                "totalEntries": 1
            }
        }
    }
You have just created your first wallet. Information about this wallet can be retrieved using the GET /api/v1/wallets/{walletId} endpoint as follow:

    curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}} \
         -H "Accept: application/json; charset=utf-8"              \
         --cacert ./scripts/tls-files/ca.crt                       \

##Receiving Money

To receive money from other users you should provide your address. This address can be obtained from an account. Each wallet contains at least one account, you can think of account as a pocket inside of your wallet. Besides, you can view all existing accounts of a wallet by using the GET /api/v1/wallets/{{walletId}}/accounts endpoint as follow:

    curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}}/accounts?page=1&per_page=10 \
         -H "Accept: application/json; charset=utf-8"                                          \
         --cacert ./scripts/tls-files/ca.crt                                                   \
Since you have, for now, only a single wallet, you’ll see something like this:

    {
        "status": "success",
        "data": [
            {
                "index": 2147483648,
                "addresses": [
                    "DdzFFzCqrh...fXSru1pdFE"
                ],
                "amount": 0,
                "name": "Initial account",
                "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"
            }
        ],
        "meta": {
            "pagination": {
                "totalPages": 1,
                "page": 1,
                "perPage": 10,
                "totalEntries": 1
            }
        }
    }

Each account has at least one address, all listed under the addresses field. You can communicate one of these addresses to receive money on the associated account.

## Sending Money

In order to send money from one of your account to another address, you can create a new payment transaction using the POST /api/v1/transactions endpoint as follow:

    curl -X POST https://localhost:8090/api/v1/transactions \
         -H "Content-Type: application/json; charset=utf-8" \
         -H "Accept: application/json; charset=utf-8"       \
         --cacert ./scripts/tls-files/ca.crt                \
         -d '{                                              \
      "destinations": [{                                    \
        "amount": 14,                                       \
        "address": "A7k5bz1QR2...Tx561NNmfF"                \
      }],                                                   \
      "source": {                                           \
        "accountIndex": 0,                                  \
        "walletId": "Ae2tdPwUPE...8V3AVTnqGZ"               \
      }                                                     \
    }'

Note that, in order to perform a transaction, you need to have some existing coins on the source account! Beside, the Cardano API is designed to accomodate multiple recipients payments out-of-the-box; notice how destinations is a list of addresses.

When the transaction succeeds, funds are becomes unavailable from the sources addresses, and available to the destinations in a short delay. Note that, you can at any time see the status of your wallets by using the GET /api/v1/transactions/{{walletId}} endpoint as follow:

    curl -X GET https://localhost:8090/api/v1/wallets/{{walletId}}?account_index=0  \
         -H "Accept: application/json; charset=utf-8"                               \
         --cacert ./scripts/tls-files/ca.crt                                        \

We have here constrainted the request to a specific account, with our previous transaction the output should look roughly similar to this:

    {
        "status": "success",
        "data": [
            {
                "amount": 14,
                "inputs": [{
                  "amount": 14,
                  "address": "DdzFFzCqrh...fXSru1pdFE"
                }],
                "direction": "outgoing",
                "outputs": [{
                  "amount": 14,
                  "address": "A7k5bz1QR2...Tx561NNmfF"
                }],
                "confirmations": 42,
                "id": "43zkUzCVi7...TT31uDfEF7",
                "type": "local"
            }
        ],
        "meta": {
            "pagination": {
                "totalPages": 1,
                "page": 1,
                "perPage": 10,
                "totalEntries": 1
            }
        }
    }

## Where can I find the API documentation?

Run the latest wallet and go to <https://127.0.0.1:8090/docs/v1/index>.

## How can I inspect runtime metrics and statistics?

Current metrics and stats are available at <http://127.0.0.1:8000/>.
