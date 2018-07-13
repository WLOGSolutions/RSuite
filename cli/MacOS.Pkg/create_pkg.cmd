@echo off
echo Creating MacOS Pkg package for RSuite CLI ...
docker rm -f pkg_builder 2> nul

aws s3 cp s3://wlog-rsuite-secrets/macos_signing.tar.gz .
docker run --name pkg_builder -d wlog/rsuite:cli_pkg_builder 2> nul ^
& (
	docker cp macos_signing.tar.gz pkg_builder:/opt/ ^
	& (
		docker exec -it pkg_builder sh -c "git clone https://github.com/WLOGSolutions/RSuite.git && cd RSuite/cli/MacOS.Pkg && mv /opt/macos_signing.tar.gz . && /bin/bash build_pkg.sh" ^
		& (docker cp pkg_builder:/opt/RSuite/cli/MacOS.Pkg/pkgs ./ ^
			& echo Creating MacOS Pkg package for RSuite CLI ... succeded)
	) 
)
del macos_signing.tar.gz