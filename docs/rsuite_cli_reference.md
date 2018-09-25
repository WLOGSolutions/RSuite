# R Suite CLI reference manual

R Suite CLI is a command line utility to manage you R projects.

It helps you with number of tasks like

* installing (upgrading to newest version) of R Suite package
* creating projects and packages inside them
* building project internal environment, project packages and packaging them into deployment zip
* managing you own local (Dir) or S3 package repositories
* building PKGZIP to transfer packages and build repositories in connection less environment.

R Suite supports you in developing you R projects in a standardized way and helps with dependencies control and project consistency management while preparing to deploy on production.

# Requirements

R Suite CLI requires R being available on your machine. Any version will do, but we tested it mostly on v3.2+.
While running R Suite CLI checks if R is available in the PATH environment variable. If it's not it will try 
to detect the R base folder from standard locations. On Windows it will also look in the registry to find 
installed R version.

For working with Subversion and/or Git revision control command line clients respectively svn and git are required.

To manage S3 repositories you will need the aws command line client and aws credentials (.aws folder in you home directory).

# Installing R Suite

To install R Suite just open you command line (terminal) and run 

```bash
rsuite install 
```

It accepts some advanced options. If you call 

```bash
rsuite install -v
```

It will log lots of messages to the console which can be used to detect the reason if any problem with installing R Suite will occur.

If you would like to use some specific repository to look for R Suite package instead of default (`http://wlog-rsuite.s3.amazonaws.com`) you can call it like this

```bash
rsuite install -u http://url.to.your.repository
```

You can also see all supported options 'rsuite install' supports please call

```bash
rsuite install -h
```

There is also the `--rstudio-addin` option which will install the `RSuiteRStudio` package. The RSuiteRStudio package is an RStudio addin which provides menu items for R Suite functionalities. Currently the following features are supported:

* project starting
* package starting
* dependencies installation
* packages building
* dependency cleaning
* deployment package building

```bash
rsuite install --rstudio-addin
```

## Updating R Suite CLI

R Suite CLI works with compatible version of R Suite. Compatibility is determined by
two higher numbers in versions of both: they should match. While installing R Suite
R Suite CLI picks the latest available but still compatible version of R Suite to 
install.

You can update R Suite CLI with following command:

```bash
rsuite update
```

The command checks that version of R Suite CLI is installed and if newer version is
available. If so it will download installer and commit installation process. You
will probably need elevated privileges to upgrade R Suite CLI properly.

Pay attention that new version of R Suite CLI will need newer version of R Suite
to work. You will need to issue R Suite installation (update to compatible version)
just after R Suite CLI gets updated.

# Project management

After you have installed R Suite you are ready to start developing your projects.
Command proj gives you access to all related R Suite functionalities.

It accepts `-h` option which prints all accepted sub-commands with brief description.

## Starting project 

To start project, create it's structure and properly put created files under revision control (Git or Subversion)
just call 

```bash
rsuite proj start -n MyProject
```

It will create project MyProject in current directory. Default content of PROPERTIES file will created and all other administrative folders 
to support project development.

It will also add created project under revision control if detects current folder to be under revision control. It will also configure appropriate 
RC ignores so non required files created during development (like installed packages or auto generated man packages) will be omitted while checking 
if project needs to be committed before generating deployment package or uploading project packages into repository.

If you do not want adding project under revision control call it following way

```bash
rsuite proj start -n MyProject --skip_rc
```

As all other commands proj accepts `-v` (short for `--verbose`) to print lots of messages during project creation 
and `-h` (short for `--help`) to print all accepted options with some short description.

## Creating package

After you have created project you can add packages to it. Just call (project folder should be your current directory)

```bash
rsuite proj pkgadd -n MyPackage
```

It will create package MyPackage inside the project in packages folder. The package will contain standard content like package internal logging tools, package imports and some simple argument validation utilities. 

It will also add created package under revision control if detect project to be under revision control. 

If you do not want adding package under revision control for some reason call it following way

```bash
rsuite proj pkgadd -n MyPackage --skip_rc
```

As all other commands proj accepts `-v` (short for `--verbose`) to print lots of messages during project creation 
and `-h` (short for `--help`) to print all accepted options with some short description.

## Project environment locking

R Suite CLI allows the user to lock the project environment. It collects all dependencies' versions and stores them in a lock file to enforce exact dependency versions in the future. To lock the project environment we have to execute the following function:

```bash
rsuite proj lock
```

The lock file is in the 'deployment' directory under the 'env.lock' name. It is a dcf file that stores information about packages in the local environment together with their versions.

When dependencies are being installed using ```rsuite proj depsinst``` the 'env.lock' file will be used to detect whether any package will change versions. If that's the case an appropriate warning message will be displayed. The feature allows to safely deploy packages with specific dependencies' versions. It prevent errors caused by newer versions of packages which might work differently than previous ones used in the project. 

To safely unlock the local project environment execute the following command:

```bash
rsuite proj unlock
```
## Building project local enviroment

When the project is created it already has the logging package installed as master scripts and all packages are supposed to use logging.

If you have any other dependencies (required libraries) you have to build internal project environment to have them available. To achieve it call somewhere inside your project folder

```bash
rsuite proj depsinst 
```

Beside standard `-v` and `-h` options depsinst sub-command accepts also `-c` (short for `--clean`) to clean up internal project environment before installing all required packages and `-r` (short for `--relock`) to allow env.lock file updating in case of removed or updated dependencies. You call them like this:

```bash
rsuite proj depsinst -c
```

```bash
rsuite proj depsinst -r
```


## Building project packages

After building local project environment you can build project packages by calling

```bash
rsuite proj build
```

R Suite detects if project package have been changed and not rebuilds it in the case. If you would like to enforce rebuilding your project packages
use following command:

```bash
rsuite proj build -f
```

It also accept standard `-v` and `-h` options.

## Cleaning unused project dependencies

If some project dependencies are not required any more you can remove them from local project environment by calling

```bash
rsuite proj depsclean
```

It does not accept any special options except standard `-v` and `-h`.

## Building deployment package

To build deployment packages simply call

```bash
rsuite proj zip
```

It checks project consistency if it is under version control to make sure all changes are committed and source revision is inline with repository. Created package will be tagged with ZipVersion taken from project PARAMETERS file with revision number appended. All project packages get rebuilt with revision number appended to their version.

Local project environment together with rebuilt project packages and master scripts is included in deployment package so in production environment (assuming it is binary compatible with development environment) you can just unzip it to have everything required to run your project functionalities.

zip sub-command accepts also `-p` (short for `--path`) which specifies there to put created deployment package:

```bash
rsuite proj zip -p /path/to/put/deployment/package
```

If you do not want project consistency check for some reason (or if project is not under version control) you can enforce deployment package version 
passing `--version` option following way:

```bash
rsuite proj zip --version=1.0
```

Version number should be in form NN.NN.

It also accepts standard options `-v` and `-h`.

## Testing project

R Suite CLI can run testthat tests for you. Just execute following command

```bash
rsuite proj test 
```

It will look for tests in 'tests' folder under project base folder. If your tests
are located elsewhere in project folder you can inform the command on that 
following way

```bash
rsuite proj test -d path/to/test/folder/instide/your/project/folder/tree
```

# Template management
R Suite CLI allows you to customise projects and packages structure by using templates. For example by default R Suite uses MRAN as the main package repository, if your projects require you to use a different repository, you can define your own project template with the adequate settings (in this case the Repositories option in the PARAMETERS file).



## Creating custom project and package templates
To create a new template use the following command 

```bash
rsuite tmpl start -n MyTemplate
```

This command creates a folder called `MyTemplate` in your working directory. All templates have the following file structure
```bash
2018-06-22  07:37    <DIR>          .
2018-06-22  07:37    <DIR>          ..
2018-06-21  07:43    <DIR>          package
2018-06-22  07:38    <DIR>          project
               0 File(s)              0 bytes
               4 Dir(s)  260 126 388 224 bytes free
```
The `project` and `package` directories contain the default R Suite project and package files. You can add, delete and modify those files according to your preference. If you want to create a template containing only a `project` or `package` directory, you can do it by using the `--prj` and `--pkg` options accordingly.

```bash
rsuite tmpl start -n MyTemplate --prj
```

```bash
rsuite tmpl start -n MyTemplate --pkg
```

If you want to create a template in a different location just use the `-p` (short for `--path`) option
```bash
rsuite tmpl start -n MyTemplate -p /path/to/create/template
```

**Important**: Templates have specific requirements in case of projects they have to contain a PARAMETERS file as for packages they have to contain a DESCRIPTION file.

R Suite templates support the usage of markers - special keywords which will be replaced while creating a project/package from a custom template. All markers have the following form: `__<word>__` for example `__ProjectName__`. The following markers are supported:

- `__ProjectName__` - will be replaced with the name of the project
- `__PackageName__` - will be replaced with the name of the package
- `__RSuiteVersion__` - will be replaced with the used R Suite version
- `__RVersion__` - will be replaced with the used R version
- `__Date__` - will be replaced with the current date
- `__User__` - will be replaced with the username
* `__LatestMRAN__` - will be replaced with `MRAN[<Date of latest working MRAN snapshot from the last 2 weeks>]`

## Registering custom templates
To register a template use the following command

```bash
rsuite tmpl register -p /path/to/created/template
```

Linux users can also register templates in the global template directory (`/etc/.rsuite/templates`) using the `-g` (short for `--global`) option

```bash
rsuite tmpl register -p /path/to/created/template -g
```

**Important**: to register a template in the global template directory extended permissions are required.


All registered templates can be listed using the following command

```bash
rsuite tmpl list
```

additional information whether the registered templates have a defined project/package template will be displayed. 

## Starting projects/packages using created templates
To start a project using a custom template the `-t` (short for `--tmpl`) option can be used. If `MyTemplate` is a registered template simply pass the name of the registered template to the `-t` option

```bash
  rsuite proj start -n MyProject -t MyTemplate
```

Unregistered templates can also be provided to the `-t` option. You simply have to provide the path to the template

```bash
  rsuite proj start -n MyProject -t /path/to/template
```

Similarly package templates can be used

```bash
  rsuite proj pkgadd -n MyPackage -t MyTemplate
```

```bash
  rsuite proj pkgadd -n MyPackage -t /path/to/template
```

**Important**: The default R Suite template is called `builtin` and can also be provided to the `-t` option. 

# System requirements management

Project environment can contain packages which have system requirements declared (in SystemRequirements field in their 
DESCRIPTION file). R Suite tries to interpret them and can help you to update your system for packages to build/work properly.
For example xml package on Linuxes requires libxml2 system library on Linuxes. Depending on platform (RedHat like or Debian like)
appropriate package should be installed using appropriate package management utility. R Suite can check your system against these 
system requirements, try to install them (if running as privileged user) and generate script to to check/install them.

## Collecting system requirements

To find out which system requirements are needed for your solution to run properly use following command:

```bash
rsuite sysreqs collect
```

It collects and presents all SystemRequirements fields declared for all project packages and packages the project depends on. 

## Checking system against requirements

To check if your system conforms to requirements declared for project dependencies use following command:

```bash
rsuite sysreqs check
```

It will collect all SystemRequirements fields from project dependencies and will try to match them with internal utilities
database. Matched requirements will enforce platform dependent checks of the system. 

## Installing system requirements

R Suite not only checks system against dependency requirements but also can update your system. Pay attention that to install 
system libraries privilege access is usually required. To update your system use following command:

```bash
rsuite sysreqs install
```

## Create system update script

You can also create script to upgrade your system to meet requirements for your project. Call following command:

```bash
rsuite sysreqs script
```

It will create bash script sysreqs_install.sh (for Linux) or batch script sysreqs_install.cmd (for Windows) which
will check for proper system elements and install them if not found using platform specific package management 
utility.

# Repository management

You can also manage content of local (Dir) and S3 repositories with R Suite CLI. For that purpose repo command should be used.

All repo sub-commands accept beside standard `-v` and `-h` following options 

* `-d` (short for `--dir`) which takes as parameter path to local (in directory) repository
* `-s` (short for `--s3_url`) which takes as parameter url to S3 repository

For local repository it is check if you have permissions to modify it.

For S3 repository it is required to have repository credentials in your user home directory and S3 command line client available in your run environment (PATH environment variable should point to folder containing aws utility).

## Initializing repository

To create repository structure execute following

```bash
rsuite repo init -s http://your-s3-bucket.s3.amazonaws.com/path
```

It will create required subfolders and package indexes for default R version and type of packages. You can specify
that you want (or not) initialize it for binary packages with following command

```bash
rsuite repo init -s http://your-s3-bucket.s3.amazonaws.com/path -b TRUE
```

You can also force repository structure creation for different R version (which is important for binary packages) 
with following command

```bash
rsuite repo init -d /path/to/your/repository --rver 3.4
```

## Adding project packages to repository

During adding project packages to repository project consistency is checked the same way it is done during building deployment package: it is checked if uncommitted changes exists and if project source revision is consistent with repository. Project packages are rebuilt with revision number appended to project version number.

To add project packages to a repository execute following

```bash
rsuite repo addproj -s http://your-s3-bucket.s3.amazonaws.com/path
```

It will add all your project packages to the repository. You can specify which packages should be added following way

```bash
rsuite repo addproj -s http://your-s3-bucket.s3.amazonaws.com/path -n Package1,Package2
```

If for some reason you do not want check project source consistency while rebuilding project packages you can skip RC checks:

```bash
rsuite repo addproj -s http://your-s3-bucket.s3.amazonaws.com/path --skip_rc
```

You can also decide which kind of packages will be built and added to repository (source or binary) with `-b` (short for `--binary`) option:

```bash
rsuite repo addproj -s http://your-s3-bucket.s3.amazonaws.com/path -b FALSE
```

If you want to add also all dependencies which are not currently present in the repository pass `--with-deps` option:

```bash
rsuite repo addproj -s http://your-s3-bucket.s3.amazonaws.com/path -b TRUE --with-deps
```

## Adding in file packages to repository

If you have some specific packages downloaded as files (source or binary) you can upload then following way:

```bash
rsuite repo addfile -d /path/to/your/repository -f /path/to/file1.tar.gz,/path/to/file2.tar.gz
```

## Adding external packages to repository

If you need for some reason add external packages (from CRAN, MRAN or any other repository) you can do it with following command

```bash
rsuite repo addext -d /path/to/your/repository -n package1,package2
```

Packages are searched in repositories project is configured to use (Repositories entry in project PARAMETERS file) for looking for dependencies.

You can specify that you want to add source (or binary) version of packages to repository with `-b` (short for `--binary`) option:

```bash
rsuite repo addext -d /path/to/your/repository -n package1,package2 -b TRUE
```

If you want to add also all dependencies which are not currently present in the repository pass `--with-deps` option:

```bash
rsuite repo addext -d /path/to/your/repository -n package1,package2 -b TRUE --with-deps
```


## Adding content of PKGZIP to repository

If you managed to build PKGZIP containing some packages (see pkgzip command) you can add its content to repository:

```bash
rsuite repo addpkgzip -s http://your-s3-bucket.s3.amazonaws.com/path -z /path/to/pkgzip.zip
```

## Adding package from GitHub to repository

If you want to add package available on GitHub repository you can achieve it calling following command:

```bash
rsuite repo addgithub -d /path/to/your/repository -r github/ProjectName
```

R Suite CLI will download sources, build package and add it to specified repository.

GitHub repository can be specified in format `username/repo[/subdir][@ref|#pull]`. 

You can also specify following options to addgithub:

* `-H` (short for `--host`) which GitHub API host to use. Use it to override with your GitHub enterprise hostname, 
  for example, 'github.hostname.com/api/v3'.
* `-b` (short for `--binary`) which takes as parameter logical value (T/F/TRUE/FALSE). It specifies what 
  kind of package will be added to the repository: system specific binary of source.
* `--rver` which takes R version number to target built package for (important for binary packages).
* `--with-deps` If passed will upload also dependencies which are not currently present in the repository.

## Adding package from BioConductor to repository

If you want to add package available on BioConductor repository you can achieve it calling following command:

```bash
rsuite repo addbioc -d /path/to/your/repository -r PackageName
```

R Suite CLI will download sources, build package and add it to specified repository.

BioConductor repository can be specified in format `[username:password@][release/]repo[#revision]`. 

You can also specify following options to addbioc:

* `-b` (short for `--binary`) which takes as parameter logical value (T/F/TRUE/FALSE). It specifies what 
  kind of package will be added to the repository: system specific binary of source.
* `--rver` which takes R version number to target built package for (important for binary packages).

## List contents of repository

You can list packages available on the repository with following command:

```bash
rsuite repo list -s http://your-s3-bucket.s3.amazonaws.com/path
```

It will print table with all packages and their versions available in repository. 
Specifying `-b` (short for `--binary`) option to can choose to list binary or source packages.

## Remove packages from repository

You can also remove packages from repository with following command:

```bash
rsuite repo remove -s http://your-s3-bucket.s3.amazonaws.com/path -r Package1==Version1,Package2==Version2
```

# Building PKGZIP packages

PKGZIPs can be used to create repository in some connection less environment. To create PKGZIPs you can use pkgzip command. All pkgzip sub-commands beside standard `-v` and `-h` options accept also `-p` (short for `--path`) which takes as parameter folder to put created PKGZIP to.

## Building PKGZIP containing project packages

During creating PKGZIP with project packages included project consistency is checked the same way it is done during building deployment package: it is checked if uncommitted changes exists and if project source revision is consistent with repository. Project packages are rebuilt with revision number appended to project version number and rebuilt versions are included into PKGZIP.

You can create PKGZIP containing project packages with command

```bash
rsuite pkgzip proj
```

It will include all your project packages into PKGZIP. You can specify which packages should be included following way

```bash
rsuite pkgzip proj -n Package1,Package2
```

If for some reason you do not want check project source consistency you can enforce PKGZIP version following way:

```bash
rsuite pkgzip proj --version=1.0
```

Version number should be in form NN.NN.

You can also decide which kind of packages will be built and included in PKGZIP (source or binary) with `-b` (short for `--binary`) option:

```bash
rsuite pkgzip proj -b TRUE
```

If you want to include also all dependencies in PKGZIP pass `--with-deps` option:

```bash
rsuite pkgzip proj -b FALSE --with-deps
```

In case you are building repository of packages (which you have access to while building PKGZIP) you probably would want
to include into PKGZIP only dependencies which could not be satisfied by current content of the repository. In that case
you can filter dependencies against contents of the repository:

```bash
rsuite pkgzip proj -b FALSE --with-deps --filter-repo http://url.to.your.repository
```

## Building PKGZIP containing in file packages

If you have some specific packages downloaded as files (source or binary) you can create PKGZIP containing them following way:

```bash
rsuite pkgzip proj -f /path/to/file1.tar.gz,/path/to/file2.tar.gz
```

## Building PKGZIP containing external packages

If you need to create PKGZIP containing external packages (from CRAN, MRAN or any other repository) you can do it with following command

```bash
rsuite pkgzip ext -n package1,package2
```

Packages are searched in repositories project is configured to use (Repositories entry in project PARAMETERS file) for looking for dependencies.

You can specify that you want to include source (or binary) version of packages in PKGZIP with `-b` (short for `--binary`) option:

```bash
rsuite pkgzip ext -n package1,package2 -b TRUE
```

If you want to include also all dependencies in PKGZIP pass `--with-deps` option:

```bash
rsuite pkgzip ext -n package1,package2 -b FALSE --with-deps
```

In case you are building repository of packages (which you have access to while building PKGZIP) you probably would want
to include into PKGZIP only dependencies which could not be satisfied by current content of the repository. In that case
you can filter dependencies against contents of the repository:

```bash
rsuite pkgzip ext -n package1,package2 --with-deps --filter-repo http://url.to.your.repository
```

## Building PKGZIP containing package from GitHub

If you want to create PKGZIP out of available on GitHub repository call following command:

```bash
rsuite pkgzip github -r github/ProjectName
```

R Suite CLI will download sources, build package and add create PKGZIP out of it.

GitHub repository can be specified in format `username/repo[/subdir][@ref|#pull]`. 

You can also specify following options to github:

* `-H` (short for `--host`) which GitHub API host to use. Use it to override with your GitHub enterprise hostname, 
  for example, 'github.hostname.com/api/v3'.
* `-b` (short for `--binary`) which takes as parameter logical value (T/F/TRUE/FALSE). It specifies what kind of package will 
  be included in PKGZIP: system specific binary of source.
* `--with-deps` If passed will include dependencies into PKGZIP.
* `--filter-repo` which takes as parameter url to repository. If passed will not include dependencies into PKGZIP which satisfying 
  versions are present in the repository. The parameter should be used together with --with-deps.

## Building PKGZIP containing package from GitHub

If you want to create PKGZIP out of available on BioConductor repository call following command:

```bash
rsuite pkgzip bioc -r PackageName
```

R Suite CLI will download sources, build package and add create PKGZIP out of it.

GitHub repository can be specified in format `[username:password@][release/]repo[#revision]`. 

You can also specify following options to github:

* `-b` (short for `--binary`) which takes as parameter logical value (T/F/TRUE/FALSE). It specifies what kind of package will 
  be included in PKGZIP: system specific binary of source.
* `--filter-repo` which takes as parameter url to repository. If passed will not include dependencies into PKGZIP which satisfying 
  versions are present in the repository. The parameter should be used together with --with-deps.

# Docker integration

R Suite CLI can help you with building your solution on some different platform than the one you are working on or build docker images containing solutions
you developed.

All commands described below assume that you have docker command usable/available.

## Building project under different platform in docker container

To build your project in docker container issue following command:

``` bash
rsuite docker zip -p centos
```

It will run docker container for centos platform, build project inside it and create zip deployment package. Generated zip is retrieved from the container and
can be used to deploy solution on your production environment.

R Suite CLI uses standard images for building solution. We support ubuntu and centos platforms for R version 3.2, 3.3 and 3.4. Each image contain installation of
appropriate R version and R Suite with R Suite CLI installed. Image for with latest version of R Suite is always used.

You can provide following options to rsuite docker zip:

* `-p` (short for `--platform`) takes platform name to build project under. centos, ubuntu and debian platforms are supported (ubuntu is default).
* `-d` (short for `--dest`) takes location to put generated zip into. Default is current folder.
* `--sh` accepts additional shell commands to execute inside container before building the project. You can pass commands to install additional system
  packages to required for project to build properly. For example following command will install libglpk-dev on container before building project:
  
``` bash
rsuite docker zip --sh "apt-get install -y libglpk-dev"
```
* `--dont-rm` If passed, container used to build the project will not be removed after command finished. If not passed container used to build project 
   will be removed even if project building fails. Option is useful if you want to detect reasons for failure.
* `--packages` Accepts comma separated list of project packages to include in project build. If not passed all project packages will be included. Option 
   is useful if your solution consists of number of images and only some packages must be present on some of them.
* `--exc-master` If passed, generated deployment zip will not contain any master scripts. It is useful if master scripts depend on some glue packages 
   which you do not want to be present in deployment zip package.
* `--version` Accepts version number to tag deployment zip package with. Version number should be in form DD.DD. If not passed standard algorithm for 
   tag selection is applied (ZipVersion from project PARAMETERS + revision from RC). If version is not enforced R Suite will check for source codes 
   consistency against RC.
* `-i` (short for `--image`) takes name of docker image to use for building zip instead of default rsuite image.

It also accepts standard options `-v` and `-h`.

## Building docker image for solution

To create docker image for the solution issue following command:

``` bash
rsuite docker img -t <my/image:tag>
```

It will build project deployment zip package under appropriate platform (same way as with 'rsuite docker zip' command) and will build docker image containing 
the deployed version of project inside. Image will be tagged with provided docker tag.

If passed docker tag does not have tag part, project tag detected from project deployment zip package name will be added as tag part. 

You can provide following options to rsuite docker img:

* `--tag-latest` if passed image build will also tagged as latest.
* `-p` (short for `--platform`) takes platform name to deploy solution under. centos, ubuntu and debian platforms are supported (ubuntu is default).
* `-f` (short for `--from`) accepts image name to use as base image. If not passed default base images will be used: for appropriate platform and R version.
* `-z` (short for `--zip`) accepts project deployment zip package file (built probably with 'rsuite docker zip' command). If passed project deployment zip
   package will not be built. Passed zip package will be used instead.
* `--templ` accepts Dockerfile template file to use for building image. It is regular Dockerfile which can contain tags to be replaced by R Suite CLI with
   automatically generated commands.
   Following tags are accepted:
    * `<RSuite:From>` will be replaced with base image name
    * `<RSuite:DeployProject>` will be replaced with commands for deploying project out of project deployment zip package.
  If not passed default Dockerfile template will be generated of following content:
``` bash
FROM <RSuite:From>
<RSuite:DeployProject>
``` 
* `--templ-ctx` accepts comma separated list of folders to add to image building context. You will be able to copy files from these folders inside your image
   while building with COPY command in Dockerfile.
* `--sh` accepts additional shell commands to pass to `rsuite docker zip` command then building project deployment zip package.
* `--packages` accepts comma separated list of project packages to pass to `rsuite docker zip` command then building project deployment zip package.
* `--exc-master` option passed to `rsuite docker zip` command then building project deployment zip package.
* `--version` Accepts version number to pass to `rsuite docker zip` command then building project deployment zip package.
* `-i` (short for `--image`) takes name of docker image to use for building zip (if not passed) instead of default rsuite image.

It also accepts standard options `-v` and `-h`.

# Getting help

You can find your which commands do rsuite accept calling 

```bash
rsuite help
```

Each command accepts `-h` (short for `--help`) option as well as help sub-command which will provide you with brief information of command purpose and
sub-commands supported.

Each sub-command accepts `-h` (short for `--help`) which will provide you with brief information on sub-command and all the options sub-command accepts 
with description.
