#!/bin/bash

dockerfile="$1"
if [ -z "$dockerfile" ]; then echo "No docker file provided. Call: $0 <Dockerfile>."; exit 1; fi

tag=`basename $1 | sed -e "s/^Dockerfile[.]//"`
docker build -t wlog/rsuite:$tag -f $dockerfile .
