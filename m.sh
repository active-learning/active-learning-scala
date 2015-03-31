#!/bin/bash
while true; do
    available=$(free -m | head -3 | tail -1 | awk '{print $4}')
    if [ "$1" -ge "$available" ]; then
	echo "memoria quase cheia:$available ; matando "
	kill $(ps aux --sort=-%mem | awk 'NR<=10{print $0}' | grep java | grep davi | head -1 | tr -s ' ' | cut -d' ' -f2)
    fi
    sleep 10
done
