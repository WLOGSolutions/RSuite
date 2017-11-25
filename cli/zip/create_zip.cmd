@echo off
echo Creating ZIP package for RSuite CLI ...
docker rm -f zip_builder 2> nul

docker run --name zip_builder -d wlog/rsuite:cli_rpm_builder 2> nul ^
& (
    (
        docker exec -it zip_builder sh -c "git clone https://github.com/WLOGSolutions/RSuite.git && cd RSuite/cli/zip && /bin/bash build_zip.sh" ^
        & (docker cp zip_builder:/opt/RSuite/cli/zip/zips ./ ^
            & echo Creating ZIP package for RSuite CLI ... succeded)
    ) 
)