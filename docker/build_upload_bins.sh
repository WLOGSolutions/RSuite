#!/bin/bash

plat=$1
rver=$2
pkgs=$3

if [ -z "$plat" -o -z "$rver" -o -z "$pkgs" ]; then
    echo "Not ehouth parameters provided: Call: $0 <platform> <rver> <pkgs>"
    exit 1
fi

# detect image
img_tag=`docker images | grep -e "^wlog/rsuite\\s*${plat}_r${rver}_v" | sort | head -n 1 | sed -e "s/^wlog\/rsuite\\s\+\(${plat}_r${rver}_v[0-9.]\+\).\+$/\\1/"`
if [ -z "$img_tag" ]; then 
    echo "Failed to detect image to build under. Was looking for wlog/rsuite:${plat}_r${rver}_vX.X.XXX"
    exit 1
fi

echo "Will build packages under wlog/rsuite:${img_tag}."

docker rm -f bin_bld_upl 2>&1 > /dev/null
docker run --name bin_bld_upl -d wlog/rsuite:${img_tag}
if [ "$plat" = "ubuntu" ]; then
    docker exec -i bin_bld_upl sh -c "apt-get install -y python-pip"
elif [ "$plat" = "centos" ]; then
    docker exec -i bin_bld_upl sh -c "yum install -y epel-release && yum install -y python-pip"
else
    echo "Unexpected platform $plat. Do not know how to install pip."
    exit 2
fi
docker cp $HOMEPATH/.aws bin_bld_upl:/root \
    && docker exec -i bin_bld_upl sh -c "pip install awscli" \
    && docker exec -i bin_bld_upl sh -c "rsuite proj start -n Uploader -v" \
    && docker exec -i bin_bld_upl sh -c "cat Uploader/PARAMETERS | grep -v 'Repositories:' > PARAMETERS.fix; echo 'Repositories: Url[http://wlog-cran.s3.amazonaws.com], CRAN' >> PARAMETERS.fix; mv -f PARAMETERS.fix Uploader/PARAMETERS" \
    && docker exec -i bin_bld_upl sh -c "cd Uploader; rsuite repo addext -b T -s http://wlog-cran.s3.amazonaws.com --with-deps -v -n $pkgs" 
    #&& docker rm -f bin_bld_upl
