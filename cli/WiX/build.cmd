@echo off

SET /P majver=<../version.txt

FOR /F "tokens=* USEBACKQ" %%F IN (`git tag`) DO (
    SET gitver=%%F
)

if "%gitver%" == "" (
    echo ERROR: No GIT tag detected.
    goto error
)

mkdir build 2> nul
echo %majver%-%gitver%> build/version.txt

SET ver=%majver%.%gitver%

echo %ver%| findstr /r /c:"^[0-9][0-9]*[.][0-9][0-9]*[.][0-9][0-9]*$" > nul
if ERRORLEVEL 1 (
    echo Invalid source version detected: '%ver%'
    goto error
)

echo Building for x64 (v%ver%)...
wix311\candle.exe -nologo -wx -arch x64 -dVERSION=%ver% -o build/RSuiteCLI_x64.wixobj src/RSuiteCLI.wxs 
wix311\candle.exe -nologo -wx -arch x64 -dVERSION=%ver% -o build/WixUI_Advanced.wixobj src/WixUI_Advanced.wxs 
if %ERRORLEVEL% GTR 0 goto error

wix311\light.exe -nologo -wx -sw1076 -spdb -ext WixUIExtension -cultures:en-us -out RSuiteCLI_v%ver%_x64.msi build/RSuiteCLI_x64.wixobj build/WixUI_Advanced.wixobj
if %ERRORLEVEL% GTR 0 goto error

echo Building for x32 (v%ver%)...

wix311\candle.exe -nologo -wx -arch x86 -dVERSION=%ver% -o build/RSuiteCLI_x86.wixobj src/RSuiteCLI.wxs
wix311\candle.exe -nologo -wx -arch x86 -dVERSION=%ver% -o build/WixUI_Advanced.wixobj src/WixUI_Advanced.wxs 
if %ERRORLEVEL% GTR 0 goto error

wix311\light.exe -nologo -wx -sw1076 -spdb -ext WixUIExtension -cultures:en-us -out RSuiteCLI_v%ver%_x86.msi build/RSuiteCLI_x86.wixobj build/WixUI_Advanced.wixobj
if %ERRORLEVEL% GTR 0 goto error

rmdir /s/q build

echo All done.
exit /B 0

:error
rmdir /s/q build 2> nul

echo Failed.
exit /B 1