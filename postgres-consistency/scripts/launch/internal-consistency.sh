#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./internal-consistency.sh IMPORTER_KV_DB_LOCATION NODE_KV_DB_LOCATION

# TODO
# - Add setting up using local or staging db
# - Add setting up using mainnet or staging

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoDir="${scriptDir}/../../.."
logsFile="${repoDir}/postgres-consistency/internalConsistency.log"

# Parameters
kvDBLocationImporter="$1"
kvDBLocationNode="$2"

# FIXME: Use also in external consistency? Import from utils file
logWithTimestamp () {
  echo "[$(date --rfc-3339='ns')] $1"
}

# FIXME: Use also in external consistency? Import from utils file
check_succeded_on_logs () {
  logWithTimestamp "Checking result obtained from $1"
  consistencyResultSucceeded=$(grep "Consistency check succeeded" $2)
  consistencyResultFailed=$(grep "Consistency check failed" $2)
  if [ "${consistencyResultSucceeded}" != "" ]; then
    logWithTimestamp "Consistency check $1 succeeded"
    rm ${logsFile}
    return 1
  elif [ "${consistencyResultFailed}" != "" ]; then
    logWithTimestamp "Consistency check failed. Check logs on file $2."
    return 0
  else
    logWithTimestamp "Unknown error happened. Check logs on file $2."
    return 0
  fi
}

logWithTimestamp "Doing setup"
${repoDir}/scripts/build/cardano-sl.sh postgres-consistency > /dev/null
printf "wallet:\n relays: [[{ host: relays.awstest.iohkdev.io }]]\n valency: 1\n fallbacks: 7" > /tmp/topology-staging.yaml

logWithTimestamp "Running internal consistency test"
stack exec -- cardano-postgres-consistency int-const \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/logs-staging" \
           --db-path ${kvDBLocationImporter} \
           --keyfile "${repoDir}/secret-staging.key" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

check_succeded_on_logs "Internal consistency" ${logsFile}

# If Internal consistency check failed, exit
if [ $? = 0 ]; then
  exit
fi

logWithTimestamp "Getting hash of the tip block"
stack exec -- cardano-postgres-consistency get-tip-hash \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/logs-staging" \
           --db-path ${kvDBLocationImporter} \
           --keyfile "${repoDir}/secret-staging.key" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

tipHash=$(grep -oP "Tip hash: [A-Za-z0-9]{64}$" ${logsFile})
tipHash=${tipHash:10}
rm ${logsFile}

logWithTimestamp "Running external tx range consistency test"
stack exec -- cardano-postgres-consistency ext-range-const --tip-hash "${tipHash}" \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/logs-staging" \
           --db-path ${kvDBLocationNode} \
           --keyfile "${repoDir}/secret-staging.key" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

check_succeded_on_logs "External range consistency" ${logsFile}
if [ $? = 1 ]; then
  logWithTimestamp "All internal consistency checks succeded"
fi
