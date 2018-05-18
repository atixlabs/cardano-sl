#!/bin/bash

# Usage: ./bn_query_node PATH_CARDANO

polling_interval=5
path_cardano=$1

node_request="curl -s -H 'Content-Type: application/json' -X GET --cacert $path_cardano/scripts/tls-files/ca.crt https://localhost:8090/api/settings/sync/progress"

while true; do
	node_response=$(eval $node_request);
	current_date=$(date +"%Y-%m-%d %H:%M:%S");
	echo "$current_date $node_response";
	sleep 5;
done;
