#!/bin/bash

ver_base=`cat ../packages/RSuite/DESCRIPTION | grep Version | sed -e "s/^Version:\s*\([0-9\\.]\+\)$/\\1/"  | sed "s/\r$//"`
git_tag=`git tag | sed 's/\r$//' | sort -n | tail -n 1`
if [ -z "$git_tag" ]; then
	echo "No git tag found. Please, tag sources with git tag <NNN>."
	exit 1
fi
rsuite_ver=${ver_base}.${git_tag}

for rver in 3.2 3.3 3.4; do 
    echo "Building wlog/rsuite:ubuntu_r${rver}_v${rsuite_ver} ..."
    docker rmi -f wlog/rsuite:ubuntu_r${rver}_v${rsuite_ver} > /dev/nul 2>&1
    docker build -t wlog/rsuite:ubuntu_r${rver}_v${rsuite_ver} -f Dockerfile.ubuntu_rsuite . --build-arg rver=$rver --build-arg rsuite_ver=$rsuite_ver $*
    if [ "$?" != "0" ]; then echo "Failed to build rsuite v${rsuite_ver} (R$rver) for Ubuntu"; exit 1; fi
    
    echo "... pushing wlog/rsuite:ubuntu_r${rver}_v${rsuite_ver} ..."
    docker push wlog/rsuite:ubuntu_r${rver}_v${rsuite_ver}

    echo "... done. (wlog/rsuite:ubuntu_r${rver}_v${rsuite_ver})"
done

