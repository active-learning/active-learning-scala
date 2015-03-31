#!/bin/bash
threshold=2000          # Minimum amount of memory left when you should start killing, in MB
while true; do
    available=$(free -m | head -2 | tail -1 | awk '{print $4}')
    if [ "$threshold" -ge "$available" ]; then
	echo "memoria quase cheia:$available"
	/home/davi/wcs/als/memorykiller.sh
    fi
    sleep 10
done
