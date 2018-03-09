# Hints and solutions

If R Suite does not work as expected first of all try to increase verbosity of logs. In most cases 
it will give you some hints how to resolve the issue.

If it does not help do not hesitate to contact us through email (rsuite@wlogsolutions.com) or R Suite site
(http://rsuite.io). We will appreciate if you describe your problem, system you are using and any kind of logs
R Suite generated when the problem occurred. It would be ideal if you could send us zipped project which causes
issues.

## Verbosity of logs

### RSuite logging

To make R Suite log more information set logging level with following command:

```bash
logging::setLevel('DEBUG')
```

Each R Suite command logs quite a log messages and output of sub-commands it runs while performing tasks. It will probably give you some hints on what is wrong.

### RSuite CLI logging

Near all commands of R Suite CLI accept -v (short for --verbose) option. It works the same way as setting DEBUG level while using R Suite in R console. For example following command for project environment building

```bash
rsuite proj depsinst -v
```

will show not only repositories it checks for dependencies but will also show progress of downloading depended packages
commands it uses to build them together with their output. It is specially useful to detect why some of dependencies
failed to be installed in project environment.


## Known issues

We are using R Suite heavily during our daily work. Below you can find couple of issues we stumbled upon. R Suite to work
properly is dependent on environment it works on. Most of cases are resolved by R Suite itself but some environment
specific cases are hard to resolve in generic way.

### Windows: user profile folder with local specific characters
If you (as me one day) selected your full name to use as username you will probably have trouble with building source
packages. 

R (and R Suite) handles folders with local specific characters in good enough way. But we discovered that building
source package in that case can be problematic. R uses user temporary folder to build source packages and executes 
R CMD sub-process for that purpose but 

  * it enforces using full temporary folder name (not 8.3 short name without local specific characters) 
  * does not pass locale context to sub-process properly so sub-process command line arguments get messed up.

As a result sub-command gets temporary folder path to build package in with messed up local symbols and of cause 
cannot install anything there. See my example below.

My name is Walerian Sokołowski (note polish letter ł). So as I used my full name as username, my profile folder path is

```bash
C:\Users\Walerian Sokołowski
```

My default temporary folder is therefore

```bash
C:\Users\Walerian Sokołowski\AppData\Local\Temp
```

So then I try to install source package testpkg1 (the one I created in my project folder) I get an error:

```bash
C:\Checkout\R\wspace\gżegżółka\polish>rsuite proj build -v
INFO:rsuite:Installing testpkg1 (for R 3.3) ...
DEBUG:rsuite:> cmd: C:/PROGRA~1/R/R-33~1.3/bin/x64/Rscript.exe --no-init-file --no-site-file -e "<...>" 2>&1
DEBUG:rsuite:> Updating testpkg1 documentation
DEBUG:rsuite:> cmd: C:/PROGRA~1/R/R-33~1.3/bin/x64/Rscript.exe --no-init-file --no-site-file -e "<...>" 2>&1
DEBUG:rsuite:> "C:/PROGRA~1/R/R-33~1.3/bin/x64/R" --no-site-file --no-environ --no-save  \
DEBUG:rsuite:>   --no-restore --quiet CMD INSTALL  \
DEBUG:rsuite:>   "C:\Checkout\R\wspace\gżegżółka\polish\packages\testpkg1" --build
DEBUG:rsuite:>
DEBUG:rsuite:> * installing to library 'C:/Users/Walerian Sokołowski/AppData/Local/Temp/RtmpWscDRB/temp_libpath5fc2624946'
DEBUG:rsuite:> * installing *source* package 'testpkg1' ...
DEBUG:rsuite:> Warning in file(file, if (append) "a" else "w") :
DEBUG:rsuite:>   cannot open file 'C:/Users/Walerian Soko3owski/AppData/Local/Temp/RtmpWscDRB/temp_libpath5fc2624946/testpkg1/DESCRIPTION': No such file or directory
DEBUG:rsuite:> Error in file(file, if (append) "a" else "w") :
DEBUG:rsuite:>   cannot open the connection
DEBUG:rsuite:> ERROR: installing package DESCRIPTION failed for package 'testpkg1'
DEBUG:rsuite:> * removing 'C:/Users/Walerian Sokołowski/AppData/Local/Temp/RtmpWscDRB/temp_libpath5fc2624946/testpkg1'
DEBUG:rsuite:>
WARNING:rsuite:Building of testpkg1 failed: Error: Command failed (1)
ERROR:rsuite:Failed to install project packages: testpkg1
```

As you can see it tries to build source package into C:/Users/Walerian Sokołowski/AppData/Local/Temp/RtmpWscDRB/temp_libpath5fc2624946 and fails passing the
folder path properly to R CMD sub process. Sub process gets path as C:/Users/Walerian Soko3owski/... and of cause can not find it.

To solve the issue just change TMP environment variable to point to some writable folder which path does not contain locale specific symbols:

```bash
set TMP=c:\temp
```

R Suite CLI will accept environment change at once. If you are using R Suite from R console, R session has to be restarted first: tempdir() is detected on R
session start.

So why don't we handle such a case in R Suite? In general case it is not possible to detect programmatically folder user can use as temporary files storage. 
The only sure place to look for such a folder is user private space which is somewhere under his profile folder. And profile folder is not a good choice as it
contains locale specific symbols. Otherwise problem would not occur.

### Linux: /tmp is mounted with noexec option
On some security conscious systems /tmp is separate volume mounted with noexec option. That option prevents running any files located on the volume even if
execution permission is set for the file.

Why does it cause problem? Some R packages contain C (or C++) codes which must be compiled during package building. Before building such source codes
configuration process is performed to check if system has all components required for source codes to build properly. For that purpose configure script must 
be executed (it is shipped together with R source package). R unpacks source package before building into sub folder under tempdir(). After unpacking R builds
package sources and installs compiled binaries. tempdir() is usually is somewhere under /tmp so configure script although has execution permission cannot be 
executed. Therefore build fails.

Below is example of such a failed build:

``` bash
[lkubiak@osx02330:~/shapes]$ rsuite proj depsinst -v
INFO:rsuite:Detecting repositories (for R 3.4)...
INFO:rsuite:Will look for dependencies in ...
INFO:rsuite:.           Dir#1 = file:////app/CRAN (source, binary)
INFO:rsuite:Collecting project dependencies (for R 3.4)...
INFO:rsuite:Resolving dependencies (for R 3.4)...
INFO:rsuite:Detected 2 dependencies to install. Installing...
DEBUG:rsuite:> cmd: /usr/lib64/R/bin/Rscript --no-init-file --no-site-file -e "<...>" 2>&1
DEBUG:rsuite:> cmd: /usr/lib64/R/bin/Rscript --no-init-file --no-site-file -e "<...>" 2>&1
DEBUG:rsuite:> * installing *source* package ‘rgeos’ ...
DEBUG:rsuite:> ** package ‘rgeos’ successfully unpacked and MD5 sums checked
DEBUG:rsuite:> ERROR: 'configure' exists but is not executable -- see the 'R Installation and Administration Manual'
DEBUG:rsuite:> * removing ‘/home/lkubiak/shapes/deployment/libs/rgeos’
[...]
```

As you can see it tried to install rgeos package but failed to run configure script.

For /tmp folder mount command presented following information:

``` bash
/dev/mapper/vg_sys-lv_tmp on /tmp type xfs (rw,nosuid,nodev,noexec,relatime,attr2,inode64,noquota)
```

As you can see /tmp was mounted with noexec option.

If you decide to change /tmp mounting options you will need root access to the machine. Under root account issue following command:

``` bash
mount -o remount,exec /tmp 
```

It will remount /tmp with exec option. 

If you do not have root access or you don't want to change /tmp mounting options you can change tempdir() location for R. That way for example:

``` bash
mkdir -p ~/tmp
export TMPDIR=~/tmp
```

Add this lines to your .profile, .bash_profile and .bashrc (or .rc) to have them executed next time you open terminal session.

If you are using R Suite from R console, R session has to be restarted first: tempdir() is detected on R session start.

### R Suite CLI complains about lack of optparse

R Suite CLI uses optparse and logging packages to interact with user (optparse is used for parsing command line arguments). Just after installation on fresh
environment R Suite CLI checks if the packages are available. If not it tries to install them from shipped together with R Suite CLI sources. 

Complaining about lack of optparse means that it was not available and R Suite CLI failed to install it from source package. To detect reason it may be useful to run rsuite command with -v (short for --verbose) option. For example:

``` bash
...>rsuite proj install -v
Installing packages into 'C:/Users/Walerian Sokołowski/Documents/R/win-library/3.3'
(as 'lib' is unspecified)
* installing *source* package 'getopt' ...
** package 'getopt' successfully unpacked and MD5 sums checked
Warning in file(file, if (append) "a" else "w") :
  cannot open file 'C:/Users/Walerian Soko3owski/Documents/R/win-library/3.3/getopt/DESCRIPTION': No such file or directory
Error in file(file, if (append) "a" else "w") :
  cannot open the connection
ERROR: installing package DESCRIPTION failed for package 'getopt'
* removing 'C:/Users/Walerian Sokołowski/Documents/R/win-library/3.3/getopt'
ERROR: dependency 'getopt' is not available for package 'optparse'
* removing 'C:/Users/Walerian Sokołowski/Documents/R/win-library/3.3/optparse'
ERROR: there is no package called 'optparse'
ERROR: optparse is required for RSuite CLI to work. Tried to install it but failed.
```

As you can see installing of optparse failed due to user library folder contains locale specific symbols. In the case you can try to install binary optparse and logging packages:

``` bash
RScript -e "install.packages(c('optparse', 'logging'), repos = 'http://cran.r-project.org/')"
```

After packages have been installed you will be able to work with R Suite CLI normally.

