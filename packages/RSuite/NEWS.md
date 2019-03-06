# RSuite 0.36 (2019-03-06)
  * #184: detecting declaration of dependencies in scripts with "library (...)".
  * handling of branches in Git RC adapter fixed.
  * #183: local Git repository is created while starting project if not RC detected
  * when checking for changes in Git repository only changes in project folder are
    considered.
  * prj_build have new parameter tag which enforces tagging packages with RC revision
    before build.
  
# RSuite 0.35 (2019-01-07)
  * Fix in dependecy resolving process. Upper requirements enforse subdependencies
    version selection not to present version collisions.
  * Fixed caching of source archive packages. Only needed packages are cached and
    all proper versions are downloaded.
  * Dependency resolution process is reporing early if requirements collision is 
    detected.
  * Proper RC version detection then building packages for repo in prjpack. 
  * Bug fix: packages detected in source archive were not included in pkgzip.

# RSuite 0.34 (2018-12-13)
  * Adapters for CI building presented. Deployment package is tagged with CI build 
    number instead of RC version.
  * Building of C++ packages fixed. After building documentation it clears created
    binaries so while building package for real they do not interfer with x64/x86 
    compiler versions.
  * Adapted to devtools 2.0.1. It had fundamental changes: functionalities are 
    delegated to pkgbuild, pkgload and remotes packages. Rcpp attributes are not
    rebuild by default with pkgbuild: enforces rebuilding if step not skiped.
  * Detecting MRAN date while no connection to internet fixed not to fail.

# RSuite 0.33 (2018-09-25)
  * Detection of implicit dependencies like <pkg>::<name> or <pkg>:::<name> in
    master scripts (and tests) added.
  * Added R Suite project templates to RStudio project templates. We can now create
    R Suite projects using "File -> New Project..." in RStudio 
  * Caching of in-src-archive packages list implemented.
  * Got rid of subprocess in favor to processx package.
  * Adding packages from BioConductor into repository implemented together with
    building PKGZIP out of such packages.

# RSuite 0.32 (2018-08-15)
  * Unregistering repo adapters functionality added.
  * Dependency resolving when using multiple repos. Previously if a subdependency
    was present in a preceding repository R Suite was unable to find it.
  * rsuite_get_os_info added to retrieve OS release/version etc.
  * SunOS is supported.
  * Changed logic when passing template as filepath, it caused issues when creating
    a project/package from a registered template and a directory with the same name was
    present in the working directory. If we want to use a template that is present in
    the working directory we have to like this "./<name_of_template>"
  * Merged tmpl_prjadd, tmpl_pkgadd into tmpl_start previously the user had to create both of those
    components separately. Now the user creates a full template using tmpl_start, but it is possible
    to skip project or package template creation by using specific flags: skip_prj, skip_pkg
  * Git RC adapter updates: detection of git2r version and using proper head retrieval function,
    fix handling of s4 classes

# RSuite 0.31 (2018-07-26)
  * Handling of old (not available in currently selected repositories) packages
     (and their versions). They are reinstalled. Versions of packages to install
     in sandbox are detected to conform with packages installed in project env.
  * Fedora platform version detection fixed.
  * New template tag added (__LatestMRAN__) which is expanded to latest available
     MRAN snapshot specification. RSuite searches from NOW 14 days back checking 
     day by day if snapshot is available. This is required due to some days 
     (e.g 2018-07-18) are not available at all.
  * NAMESPACE validation fixed: it supports both import and importFrom clauses.
  * Checking dependencies on locked project fixed. It crached previously if some
     dependencies failed to satisfy lock.

# RSuite 0.30 (2018-07-03)
  * RSuite package supports MacOS from now on: proper repository url handling,
     commands to download/build packages adapted to run on MacOS, handling of
     mac.binary type added.
  * documentation spell-checked

# RSuite 0.29 (2018-06-24)
  * #169: if project package depends on some dependents of devtools and devtools
     is installed into sbox as supportive package not available in the 
     environment (e.g. for R without RSuite) devtools could not be loaded for 
     project package building because only packages from sbox there visible 
     while loading devtools. Now devtools are loaded with sbox & libs 
     visibility.
  * #168: while detecting supportive packages to install checking if available
     external package is loadable in project environment. If not it gets 
     installed.
  * locking/unlocking of project environment functionality added. 
  * template based project/package creation added
  * cache (for repository contents and downloaded packages) is stored in location
     specified by rsuite.cache_path option. If unset caching is totally off.
  * user templates are detected in folder specified by rsuite.user_templ_path
     option. If unset no user templates are in use.
  * minor fixes in sysreqs functionality: removed duplicated logs, support for
     additional tool information added, conda build fixed to install and reinstall
     properly.

# RSuite 0.28 (2018-06-01)
  * Documentation changes: package main documentation added, sections added, 
     documentation reviewed for API functions.
  * vignettes argument added to prj_build. Passing FALSE skips building vignettes.
  * vanilla_sups flag added to prj_install_deps. It causes check/install only 
     basic supportive dependencies required for project package building.
  * check_repos_consistency flag added to prj_install_deps. For R-devel CRAN has 
     packages not rebuilt for R-devel version, so consistency check breaks 
     building project environment.
  * detection of OS version for binary packages repository path fixed. It failed
     on some Debians. If regular detection does not work warning is logged and 
     generic R.version$platform is used.

# RSuite 0.27 (2018-05-29)
  * Handling downloaded packages cache have had error: then multiple packages
     were checks improper repository cache folder was selected. Due to that
     binary packages there installed for improper R version.
  * RSuite processed with goodpractices and most of detected issues fixed. 
     goodpractices unfortunately does not use .lintr in package file, so 
     default lintr configuration (improper for RSuite) is in use while checked.
  * Vignettes are also checked if changed while detecting package changes.
  * While building package its installation folder (in libs) is not removed. The
     folder is cleared only during package removal. Warning that DESCRIPTION is
     not available while building package vanished.
  * RSuite vignette refactored to run step by step basic workflow with presenting
     intermediate results.
  * Authors in DESCRIPTION updated to contain all contributors and copyright 
     holders.

# RSuite 0.26 (2018-05-23)
  * Saving MD5 sums for package also if it gets build for the first time.
  * #138: Fixes related to project R-cross building. Proper detection of 
     installed packages for Rver. devtools is installed into sbox if not 
     found for Rver required by the project.
  * Handling vignettes building while building package.
  * testthat moved to Suggested. While building package it is detected that 
    package requires testthat for testing and it gets installed into sbox.
    Also other packages required to run tests are detected and installed into
    sbox. All packages from Suggests field are also installed into sbox as 
    they are required to properly check package.
  
# RSuite 0.25 (2018-04-23)
  * Functionality related to integration with RStudio moved to separate AddIn
     package. Got rid of dependency to rstudioapi.
  * Removed dependency to knitr and rmarkdown. They should be installed as
     supportive packages.
  * On Linux properly detecting if supportive packages are available. They can
     be present under .Library.site but not under .Library.
  * Default MRAN date is Today - 7days.

# RSuite 0.24 (2018-04-18)
  * .Rprofile loads environment through set_env.R not to load RSuite before
     environment is configured properly.
  * set_env.R default script_path set to R (relative to base project dir).
  * Removing previously build version from intrepo before rebuilding package.
     On windows non removing caused updating package instead of overwriting
     due to that if something was removed it not got removed from build packaged.
  * Fixed zipping of project not to remove data files.

# RSuite 0.23 (2018-03-09)
  * SystemRequirements field in package DESCRIPTION interpretation and handling
     of declared system requirements implemented.

# RSuite 0.22 (2018-01-19)
  * RC revision is checked for validity: Git tags containing non digits prevent
     packages from building when tagged.
  * Project can be packed with dest R version enforcement to support CLI docker
     command.
  * Support packages are detected and installed into sbox while collecting
     dependencies: as for now only libraries required to run roclets on project
     packages.
  * Imports consistency with declared in package DESCRIPTION fixed.

# RSuite 0.21 (2018-01-17)
  * Repository cache handling enhanced. It is recached if older than 1 week.
  * Fixed repository caching for R 3.2
  * Then packing project dest R version is not checked.
  * Then generating DESCRIPTION for package user name is converted to latin1
     to get rid of all locale specific characters.
  * Handling RoxygenExtraRoclets in package DESCRIPTION file.
  * MRAN repository uses https by default

# RSuite 0.20 (2017-12-08)
  * Caching of available packages from each repository to build faster with CLI.
  * While building project packages it detects changes in package sources.
     If no changes detected package will not be rebuilt and package from intrepo
     will be installed.
  * Remote packages downloaded are cached. So next time they are ready which
     reduces dependency collection time.
  * Fix in package version comparison

# RSuite 0.19 (2017-11-23)
  * Detection imports in package NAMESPACE fixed to support comma separated lists.
  * Default set_env.R fixed to create user specific log file if has no access to default.
  * Version restriction handling fixed, tests created.
  * GitHub package building fixed to support Git repositories requiring all Git
     repository structure to build properly (e.g. Microsoft/LightGBM).
  * There is not requirement for package to have same name as package folder.
  * It is possible to skip some preliminary steps while building project packages
     or packages from github (to put into PKGZIP or to add to repository).
  * While building package from github it is possible to keep its sources after
     build finished.
  * fixing package index in repository for R3.4 fixed for case of removing last
     package in repository.

# RSuite 0.18 (2017-11-13)
  * #150: checking if R version is available early while installing deps, building,
     zipping and packing.
  * for R3.4 it does not creates PACKAGES.rds (and removes it if found in repo) as
     it does not support multiple package versions.

# RSuite 0.17 (2017-11-11)
  * prj_pack can be told to pack only selected project packages or not to include
     master scripts in the pack.
  * fixed bug in package version requirements detection. It used to create large
     available packages set with duplicates.
  * fixed error with handling imports with dot in name.
  * debian & redhat OS version detection corrected
  * if NAMESPACE file of source package is built with roxygen, before building
     documentation non present imports from DESCRIPTION are appended. Previously
     they replaced content.
  * while building source packages to binary form neither documentation is
     rebuilt nor imports validity testing is performed.
     e.g. withr source package does not have all requirements to build documentation.
  * while building source packages to binary form Rcpp:compileAttributes is skipped.
     It does not not work properly with newest Rcpp (e.g. for plyr).
  * updating of PACKAGES.rds fixed while uploading to S3 repository.

# RSuite 0.16 (2017-11-08)
  * Project zip can be created for projects without master scripts.
  * prj_pack function added to export project sources. It is useful for building
     project in docker and for giving project sources your peers to play with it.

# RSuite 0.15 (2017-11-04)
  * Creation of PKGZIP with dependencies and filtering against packages existing
     in passed repository.
  * Uploading packages into repository together with dependencies and filtering
     against existing packages present in it.

# RSuite 0.14 (2017-10-30)
  * Handling of multiple Dir repositories in project fixed.

# RSuite 0.13 (2017-10-23)
  * #146: After installing package it is checked if it was built for proper
     R version. MRAN has packages built under 3.3.2 presented as built for 3.4.
  * Failure of building documentation for package is considered major error as
     roxygen also builds NAMESPACE file which is responsible for project exports
     and imports. Failure to properly declare NAMESPACE will make package unusable.
  * Detection if post documentation build imports in package DESCRIPTION file and
     NAMESPACE file are consistent. NAMESPACE is updated if has some packages
     declared in DESCRIPTION missing.

# RSuite 0.12 (2017-10-16)
  * RStudio basic integration implemented. Dependency to rstudioapi added.
  * sbox folder added beside deployment/libs for libraries installed by the user.
  * #147: Building package documentation for unprivileged user fixed.
  * #141: prj_zip does not require project to be under version control. Version
     is detected out of PARAMETERS/ZipVersion and packages version if format does
     not exist.
  * #143: Clean dependencies RSuite addin added.

# RSuite 0.11 (2017-10-12)
  * Handling of .libPath during project package build: it currently contains
     also user library path.
  * #140: Building package documentation then package has dependencies fixed.

# RSuite 0.10 (2017-09-24)
  * Fixed handling of .libPath during prj_deps_inst and prj_build.
  * logging moved to Imports in RSuite and created projects.

# RSuite 0.9 (2017-09-20)
  * Minor fixes related to installation of RSuite

# RSuite 0.8 (2017-09-13)
  * Dependencies are checked both binary and source in an iterative
       manner until they are all satisfied (deep dependency search).
  * Repository order is important: former repositories are checked for
       dependencies before later ones. If proper version is found latter
       repositories will not be checked to satisfy it.

# RSuite 0.7 (2017-09-12)
  * Packages are not detached during build if they are loaded from another
       location then project environment
  * Repository manager is separated out of repository adapter.
  * Packages from github can be uploaded into a repository or packed into PKGZIP.
  * Setting RVersion for package added into API.
  * License changed to MIT + LICENSE
  * Listing and removing packages from repositories added.
  * It does not require optparse any more
  * While building pkg zip from external packages it can include binary versions
       build out of sources.
  * Installing binary packages from repository on Linux fixed.

# RSuite 0.6 (2017-08-10)
  * R version consistency check and enforcement on project level
  * Issues in upgrade of old deployment structure fixed

# RSuite 0.5 (2017-07-22)
  * Added cleaning non required packages from project local environment.
  * PKGZIP creation and upload implemented
  * Proper checking for SVN if update is required

# RSuite 0.4 (2017-07-18)
  * Got rid of checkpoint dependency
  * Support for specific package installing requirements added like configure.args
      and configure.vars and checking for environment variables.

# RSuite 0.3 (2017-07-14)
  * SVN adapter fixed to properly detect if project is under RC then creating
  * SVN adapter fixed to properly remove svn:externals from project directory
  * Fixed deployment scripts to properly reference logging package
  * Documentation fixed not to contain internals
  * Dependencies consistency detection while building project implemented
  * Project packages are built before building zip package. If RC revision is
      detected it is added to versions of project back ups before building.
  * Repository adapters added for CRAN and MRAN. Detection if repository is
      reliable realized. Warning is logged if any of repositories is not.
  * Got rid of miniCRAN dependency.
  * S3 and Dir repository management added. Project packages can be uploaded
      into repository as binary or source packages. User also can upload
      arbitrary package files into repository.
  * RSuite version check and update implemented.

# RSuite 0.2 (2017-07-05)
  * Adapter mechanism implemented: RC adapters for Git and Svn, Local and S3
    repository adapters.
  * Tests reviewed and adapted to RSuite.
  * Logging is consistent: all messages are logged into rsuite logger

# RSuite 0.1 (2017-06-23)
  * Initial version
