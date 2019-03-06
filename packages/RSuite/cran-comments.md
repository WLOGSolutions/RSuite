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
  * Bug fix: detecting declaration of dependencies in scripts with "library (...)".
  * handling of branches in Git RC adapter fixed.
  * local Git repository is created while starting project if not RC detected
  * when checking for changes in Git repository only changes in project folder are
    considered.
  * prj_build have new parameter tag which enforces tagging packages with RC revision
    before build.
