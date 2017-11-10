#!/bin/bash

plat=$1
if [ -z "$plat" ]; then
    plat="ubuntu"
fi

if [ ! -f "Dockerfile.${plat}_rbase" ]; then
    echo "Dockerfile.${plat}_rbase not found"
    exit 1
fi

for rver in 3.2 3.3 3.4; do 
    echo "Building wlog/rsuite:${plat}_r${rver} ..."
    docker build --build-arg rver=$rver -t wlog/rsuite:${plat}_r${rver} -f Dockerfile.${plat}_rbase .
    if [ "$?" != "0" ]; then echo "Failed to build rbase $rver for $plat"; exit 1; fi

    echo "... pushing wlog/rsuite:${plat}_r${rver} ..."
    docker push wlog/rsuite:${plat}_r${rver}

    echo "... done. (wlog/rsuite:${plat}_r${rver})"
done
