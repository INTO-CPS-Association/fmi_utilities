#!/bin/bash

rm -rf target
mkdir target
docker run  \
    -v "$(pwd)":/application \
    docker.sweng.au.dk/haskell865nodeelm:latest \
    /bin/bash -c 'cd application && ./make.sh 'none''
