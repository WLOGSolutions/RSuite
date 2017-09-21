@echo off

FOR /F "tokens=* USEBACKQ" %%F IN (`git tag`) DO (
    SET gitver=%%F
)

IF "%gitver%" == "" (
    echo ERROR: No GIT tag detected.
    goto error
)

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
FOR /R %%F IN (RSuiteCLI_v*.%gitver%_x*.msi) DO (
    aws s3 cp %%F s3://wlog-rsuite/cli/windows/
    IF ERRORLEVEL 1 goto popd_error
)
popd
echo Building/uploading RSuite CLI tag %gitver% MSI onto S3 repository ... done

echo Building/uploading RSuite CLI tag %gitver% RPM package onto S3 repository ...
pushd cli\rpm
rmdir /s/q rpms 2> nul
call create_rpm.cmd
FOR /R %%F IN (rpms\rsuitecli-*.%gitver%-1.noarch.rpm) DO set rpm=%%F
IF "%rpm%" == "" goto popd_error
aws s3 cp %rpm% s3://wlog-rsuite/cli/redhat/
IF ERRORLEVEL 1 goto popd_error
popd
echo Building/uploading RSuite CLI tag %gitver% RPM package onto S3 repository ... done

echo Building/uploading RSuite CLI tag %gitver% DEB package onto S3 repository ...
pushd cli\deb
rmdir /s/q debs 2> nul
call create_deb.cmd
FOR /R %%F IN (debs\rsuitecli_*.%gitver%-1_all.deb) DO set deb=%%F
IF "%deb%" == "" goto popd_error
aws s3 cp %deb% s3://wlog-rsuite/cli/debian/
IF ERRORLEVEL 1 goto popd_error
popd
echo Building/uploading RSuite CLI tag %gitver% DEB package onto S3 repository ... done

echo All done.
exit /B 0

:popd_error
popd

:error
echo Failed.
exit /B 1
