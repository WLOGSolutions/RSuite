## Test environments
* local Windows install, R 3.3.0, R 3.5.0, R-devel
* win-builder (devel and release)
* Travis-CI: os & osx with devel, release, oldrel, R 3.3.0
* AppVeyor: devel, release, oldrel, R 3.3.0
* rhub-builder (win server - devel, ubuntu - release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
No issues detected

## Previous submission comments
  * Detection if vignettes are to build fixed.
  * Fixed building vignettes: use devtools::build_vignettes and place generated
    files for building not just for loading. Also if vignettes index is generated
    use it instead of creating artificial.
  * Sysreqs on linux/osx detects if installation is possible (installation is under 
    root account). It detects also which sysreqs are installed already and do not
    try to install them again. Sysreqs are installed one by one so it is possible
    to find out which caused the problem while installing.
  * Fixed default proj template to run properly on OSX
  * Updated sysreqsdb to handle conda, python and make on OSX
  * Added ability to wrap deploymnet zip into bash installer package.
