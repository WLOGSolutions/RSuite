#!/bin/bash

#for rver in 3.2 3.3 3.4; do 
#    echo "Building wlog/rsuite:ubuntu_r$rver ..."
#    docker build --build-arg rver=$rver -t wlog/rsuite:ubuntu_r$rver -f Dockerfile.ubuntu_rbase .
#    if [ "$?" != "0" ]; then echo "Failed to build rbase $rver for Ubuntu"; exit 1; fi
    
#    echo "... pushing wlog/rsuite:ubuntu_r$rver ..."
#    docker push wlog/rsuite:ubuntu_r$rver

#    echo "... done. (wlog/rsuite:ubuntu_r$rver)"
#done

for rver in 3.2; do 
    echo "Building wlog/rsuite:centos_r$rver ..."
    docker build --build-arg rver=$rver -t wlog/rsuite:centos_r$rver -f Dockerfile.centos_rbase .
    if [ "$?" != "0" ]; then echo "Failed to build rbase $rver for CentOS"; exit 1; fi

    echo "... pushing wlog/rsuite:centos_r$rver ..."
    #docker push wlog/rsuite:centos_r$rver

    echo "... done. (wlog/rsuite:centos_r$rver)"
done
