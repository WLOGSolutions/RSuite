#!/bin/bash

ver_base=`cat ../packages/RSuite/DESCRIPTION | grep Version | sed -e "s/^Version:\s*\([0-9\\.]\+\)$/\\1/"  | sed "s/\r$//"`
git_tag=`git tag | sed 's/\r$//' | sort -n | tail -n 1`
if [ -z "$git_tag" ]; then
	echo "No git tag found. Please, tag sources with git tag <NNN>."
	exit 1
fi
rsuite_ver=${ver_base}.${git_tag}

plat=$1
if [ -z "$plat" ]; then
    plat="debian"
fi

if [ ! -f "Dockerfile.${plat}_rsuite" ]; then
    echo "Dockerfile.${plat}_rsuite not found"
    exit 1
fi


for rver in 3.2 3.3 3.4; do 
    echo "Building wlog/rsuite:${plat}_r${rver}_v${rsuite_ver} ..."
    docker rmi -f wlog/rsuite:${plat}_r${rver}_v${rsuite_ver} > /dev/nul 2>&1
    docker build -t wlog/rsuite:${plat}_r${rver}_v${rsuite_ver} -f Dockerfile.${plat}_rsuite . --build-arg rver=$rver --build-arg rsuite_ver=$rsuite_ver --no-cache
    if [ "$?" != "0" ]; then echo "Failed to build centos v${rsuite_ver} (R$rver) for ${plat}"; exit 1; fi
    
    echo "... pushing wlog/rsuite:${plat}_r${rver}_v${rsuite_ver} ..."
    docker push wlog/rsuite:${plat}_r${rver}_v${rsuite_ver}

    echo "... done. (wlog/rsuite:${plat}_r${rver}_v${rsuite_ver})"
done
