@echo off
echo Creating RMP package for RSuite CLI ...
docker rm -f rpm_builder 2> nul

docker run --name rpm_builder -d wlog/rsuite:cli_rpm_builder 2> nul ^
& (
    (
        docker exec -it rpm_builder sh -c "git clone https://github.com/WLOGSolutions/RSuite.git && cd RSuite/cli/rpm && /bin/bash build_rpm.sh" ^
        & (docker cp rpm_builder:/opt/RSuite/cli/rpm/rpms ./ ^
            & echo Creating RMP package for RSuite CLI ... succeded)
    ) 
)