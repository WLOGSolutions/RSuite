@echo off
echo Creating DEB package for RSuite CLI ...
docker rm -f deb_builder 2> nul

docker run --name deb_builder -d wlog/rsuite:cli_deb_builder 2> nul ^
& (
    (
        docker exec -it deb_builder sh -c "git clone https://github.com/WLOGSolutions/RSuite.git && cd RSuite/cli/deb && /bin/bash build_deb.sh" ^
        & (docker cp deb_builder:/opt/RSuite/cli/deb/debs ./ ^
            & echo Creating DEB package for RSuite CLI ... succeded)
    ) 
)