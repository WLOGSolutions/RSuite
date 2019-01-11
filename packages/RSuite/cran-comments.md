## Test environments
* local Windows install, R 3.3.0, R 3.5.0, R-devel
* win-builder (devel and release)
* Travis-CI: os & osx with devel, release, oldrel, R 3.3.0
* AppVeyor: devel, release, oldrel, R 3.3.0

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
No issues detected

## Previous submission comments
  * Fix in dependecy resolving process. Upper requirements enforse subdependencies
    version selection not to present version collisions.
  * Fixed caching of source archive packages. Only needed packages are cached and
    all proper versions are downloaded.
  * Dependency resolution process is reporing early if requirements collision is 
    detected.
  * Proper RC version detection then building packages for repo in prjpack. 
  * Bug fix: packages detected in source archive were not included in pkgzip.
