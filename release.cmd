@echo off

FOR /F "tokens=* USEBACKQ" %%F IN (`git tag`) DO (
    SET gitver=%%F
)

IF "%gitver%" == "" (
    echo ERROR: No GIT tag detected.
    goto error
)

set pkg_index=%~dp0\PKG_INDEX
del "%pkg_index%" 2> nul

echo Building/uploading RSuite tag %gitver% onto S3 repository ...
call rsuite proj depsinst -v
if ERRORLEVEL 1 goto error
call rsuite repo addproj -s http://wlog-rsuite.s3.amazonaws.com -b F -v
if ERRORLEVEL 1 goto error
echo Building/uploading RSuite tag %gitver% onto S3 repository ... done


echo Building/uploading RSuite CLI tag %gitver% MSI onto S3 repository ...
pushd cli\WiX 

del *.msi 2> nul 
call build.cmd 

FOR /R %%F IN (RSuiteCLI_v*.%gitver%_x64.msi) DO set msi_x64=%%F
IF "%msi_x64%" == "" goto popd_error
rem aws s3 cp "%msi_x64%" s3://wlog-rsuite/cli/windows/
rem IF ERRORLEVEL 1 goto popd_error
echo win-x64: %msi_x64% >> "%pkg_index"

FOR /R %%F IN (RSuiteCLI_v*.%gitver%_x86.msi) DO set msi_x86=%%F
IF "%msi_x86%" == "" goto popd_error
rem aws s3 cp "%msi_x86%"" s3://wlog-rsuite/cli/windows/
rem IF ERRORLEVEL 1 goto popd_error
echo win-x86: %msi_x86% >> "%pkg_index"

popd
echo Building/uploading RSuite CLI tag %gitver% MSI onto S3 repository ... done


echo Building/uploading RSuite CLI tag %gitver% RPM package onto S3 repository ...
pushd cli\rpm

rmdir /s/q rpms 2> nul
call create_rpm.cmd

FOR /R %%F IN (rpms\rsuitecli-*.%gitver%-1.noarch.rpm) DO set rpm=%%F
IF "%rpm%" == "" goto popd_error
rem aws s3 cp "%rpm%" s3://wlog-rsuite/cli/redhat/
rem IF ERRORLEVEL 1 goto popd_error
echo rpm: %rpm% >> "%pkg_index"

popd
echo Building/uploading RSuite CLI tag %gitver% RPM package onto S3 repository ... done


echo Building/uploading RSuite CLI tag %gitver% DEB package onto S3 repository ...
pushd cli\deb

rmdir /s/q debs 2> nul
call create_deb.cmd

FOR /R %%F IN (debs\rsuitecli_*.%gitver%-1_all.deb) DO set deb=%%F
IF "%deb%" == "" goto popd_error
rem aws s3 cp "%deb%" s3://wlog-rsuite/cli/debian/
rem IF ERRORLEVEL 1 goto popd_error
echo deb: %deb% >> "%pkg_index"

popd
echo Building/uploading RSuite CLI tag %gitver% DEB package onto S3 repository ... done


echo Uploading RSuite CLI %gitver% PKG_INDEX onto S3 repository ...
rem aws s3 cp "%pkg_index%" s3://wlog-rsuite/cli/
rem IF ERRORLEVEL 1 goto error
echo Uploading RSuite CLI %gitver% PKG_INDEX onto S3 repository ... done


echo All done.
exit /B 0

:popd_error
popd

:error
echo Failed.
exit /B 1
