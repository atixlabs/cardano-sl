#!/bin/bash

# Usage: ./bn_query_node

polling_interval=5

importer_request="curl -s -H 'Content-Type: application/json' -X GET http://localhost:8200/api/stats/blocksCount"

while true; do
	importer_response=$(eval $importer_request);
	current_date=$(date +"%Y-%m-%d %H:%M:%S");
	echo "$current_date $importer_response";
	sleep 5;
done;