#!/bin/sh

if [ $1 = "build" ]; then
    echo "Building"
    stack build
elif [ $1 = "run" ]; then
    echo "Running"
    stack run
else
    echo "Unsupported command: $1"
    exit 1
fi
