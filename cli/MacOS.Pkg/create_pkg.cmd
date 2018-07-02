@echo off
echo Creating MacOS Pkg package for RSuite CLI ...
docker rm -f pkg_builder 2> nul

docker run --name pkg_builder -d wlog/rsuite:cli_pkg_builder 2> nul ^
& (
    (
        docker exec -it pkg_builder sh -c "git clone https://github.com/WLOGSolutions/RSuite.git && cd RSuite/cli/MacOS.Pkg && /bin/bash build_pkg.sh" ^
        & (docker cp pkg_builder:/opt/RSuite/cli/MacOS.Pkg/pkgs ./ ^
            & echo Creating MacOS Pkg package for RSuite CLI ... succeded)
    ) 
)