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
  * Fixed problem with CRAN's Debian R-devel (2018-12-12) check platform:
    "length > 1 in coercion to logical" occured to more restrictive logical
    operations. Verified that && and & are consistently used.
  * Detection of implicit dependencies like <pkg>::<name> or <pkg>:::<name> in
    master scripts (and tests) fixed not to consider occurence in strings or
    comments.
  * Adapted to changes in devtools v2.0.x
  * Proper handling of packages with C/C++ sources and detection if package
    (dependency or in-project) cannot be reinstalled.
  * In-project packages if not changed are not reinstalled: much faster 
    project under development building.
  * Fixed detection of MRAN available snapshot while starting project using
    MRAN.
  * Adapter to CI builders (Jenkins is in-built) are presented: projects 
    built under CI(Jenkins) will be tagged with build number instead of RC tag.
  * Running build/install subprocesses are more durable.
