# Basic R Suite usage
In this document basic R Suite usage is presented. It covers:

* creating project
* adding custom packages
* building custom packages
* installing dependencies
* developing custom package with `devtools`
* understanding loggers
* project environment locking

**Important** This tutorial was tested with R Suite version 0.25-234.

## **Got stuck?**

If you are stuck feel free to contact us:

* through the RSuite website (http://rsuite.io#contact) or 
* using Gitter [RSuite room](https://gitter.im/WLOGSolutions/RSuite
  "Gitter RSuite room")
* directly by sending an email with the description of your problem to
  [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).
  
## Table of contents

  * [Step 1 - start a new project](#step-1---start-a-new-project "Step 1 - start a new project")
  * [Step 2 - add first package](#step-2---add-first-package "Step 2 - add first package")
  * [Step 3 - add custom package to master script](#step-3---add-custom-package-to-master-script "Step 3 - add custom package to master script")
  * [Step 4 - building custom packages](#step-4---building-custom-packages "Step 4 - building custom packages")
  * [Step 5 - adding function to a package](#step-5---adding-function-to-a-package "Step 5 - adding function to a package")
  * [Step 6 - rebuild custom packages](#step-6---rebuild-custom-packages "Step 6 - rebuild packages")
  * [Step 7 - adding dependencies ](#step-7---adding-dependencies "Step 7 - adding dependencies ")
  * [Step 8 - install dependencies](#step-8---install-dependencies "Step 8 - install dependencies")
  * [Step 9 - developing custom package using `devtools`](#step-9---developing-custom-package-using-devtools "Step 9 - developing custom package using `devtools`")
  * [Step 11 - loggers in packages](#step-11---loggers-in-packages "Step 11 - loggers in packages")
  * [Step 12 - Project environment locking](#step-12---project-environment-locking "Step 12 - Project environment locking")
  * [Step 13 - prepare deployment package](#step-13---prepare-deployment-package "Step 13 - prepare deployment package")
  * [Step 14 - running deployment package](#step-14---running-deployment-package "Step 14 - running deployment package")


# Step 1 - start a new project

![Start a new project](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_1.png "Start a new R Suite project")

To create a new project (called `my_project`) we have to issue the following command
```bash
rsuite proj start -n my_project
```

If the folder you are working in is not under git/svn control the output should look like this:
```
2017-09-23 16:39:40 INFO:rsuite:Will create project my_project structure for RSuite v0.9.211.
2017-09-23 16:39:40 INFO:rsuite:Project my_project started.
2017-09-23 16:39:40 WARNING:rsuite:Failed to detect RC manager for my_project
```

To avoid warning messages you can add `--skip_rc` when calling `rsuite`.

## Step 1.1 - run master file

![Run master file](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_1_1.png "Run master file")

Every project has a specific structure. Let's change the directory to the project directory
we just created in order to check it.

```bash
cd my_project
dir
```

You should see the following output

```
 Directory of C:\Workplace\Projects\my_project        
                                                      
2017-09-23  16:39    <DIR>          .                 
2017-09-23  16:39    <DIR>          ..                
2017-09-23  16:39                20 .Rprofile         
2017-09-23  16:39                16 config_templ.txt  
2017-09-23  16:39    <DIR>          deployment        
2017-09-23  16:39    <DIR>          logs              
2017-09-23  16:39               371 my_project.Rproj  
2017-09-23  16:39    <DIR>          packages          
2017-09-23  16:39               121 PARAMETERS        
2017-09-23  16:39    <DIR>          R                 
2017-09-23  16:39    <DIR>          tests             
               4 File(s)            528 bytes         
```

In the `R` directory there are master scripts - these are execution scripts in our project. R Suite by default creates an exemplary script `R\master.R`

```R
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
	if (!length(script_path)) { return(".") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()
```

To check if everything is working properly issue the following command

```bash
Rscript R\master.R
```

You should not see any error messages.


# Step 2 - add first package

![Add first package to R Suite project](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_1_1.png
"Add first package to R Suite project")

R Suite forces users to keep logic in packages. To create a package issue the following command

```bash
 rsuite proj pkgadd -n mypackage
```

To check if everything works run the following commands

```bash
  cd packages\mypackage
  dir
```

You should see the following output

```
2017-09-23  16:50    <DIR>          .
2017-09-23  16:50    <DIR>          ..
2017-09-23  16:50                20 .Rprofile
2017-09-23  16:50    <DIR>          data
2017-09-23  16:50               309 DESCRIPTION
2017-09-23  16:50    <DIR>          inst
2017-09-23  16:50    <DIR>          man
2017-09-23  16:50               371 mypackage.Rproj
2017-09-23  16:50                65 NAMESPACE
2017-09-23  16:50                73 NEWS
2017-09-23  16:50    <DIR>          R
2017-09-23  16:50    <DIR>          tests
               5 File(s)            838 bytes
```

# Step 3 - add custom package to master script

![Editing master.r in R Studio](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_3a.png "Editing master.R in R Studio")

Open `R\master.R` in any editor and change it to look like this:

```R
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
	if (!length(script_path)) { return("R") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(mypackage)
```

You can check if your package is visible to your master script by
using the following commands

![Running master.R](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_3b.png "Running master.R")

```bash
Rscript R\master.R
```

You will notice an error saying there is no such package as
`mypackage`. This is fine because in R you have to install package to
have access to it.

```
Loading required package: methods
Error in library(mypackage) : there is no package called 'mypackage'
Execution halted
```

# Step 4 - building custom packages

![Building custom packages](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_4.png "Buidling custom packages")

Adding a package to the project is not enough to use it. You have to build it. You can do this using this command

```bash
rsuite proj build
```

On my computer this command gave the following output

```
2017-09-23 16:55:55 INFO:rsuite:Installing mypackage (for R 3.4) ...
2017-09-23 16:56:00 INFO:rsuite:Successfuly build 1 packages
```

Now you can check if your master script has access to the package `mypackage`

```bash
Rscript R\master.R
```

If everything worked properly you should not see any error messages.

# Step 5 - adding function to a custom package

![Adding function to a custom package](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_4.png "Adding function to a custom package")

Let's add a function `hello_world` to our package `mypackage`. To do
this you have to create a new file in folder
`packages/mypackage/R/hello_world.R`. Edit `hello_world.R` to have the following content

```R
#'@export
hello_world <- function(name) {
    sprintf("Hello %s!", name)
}
```

Please remember to add `#'@export` if you want to see this function in the global namespace.

Now you can change the master script by adding one line to it

```R
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
	if (!length(script_path)) { return("R") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(mypackage)

hello_world("John")
```

![Running master.R](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_5b.png "Running master.R")

Let's check if everything works

```bash
Rscript R\master.R
```

As you can see you got an error message that there is no such function
as `hello_world`.

```
Loading required package: methods
Error in hello_world("John") : could not find function "hello_world"
Execution halted
```

# Step 6 - rebuild custom packages

![Rebuilding custom packages](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_6.png "Rebuilding custom packages")

You have to rebuild packages to have all the functionalities available
to master scripts. You do it with the following command.

```bash
rsuite proj build
```

And check if `master.R` works

```bash
Rscript R\master.R
```

You should see the output with the following line

```
[1] "Hello John!"
```

# Step 7 - adding dependencies 

You can add dependencies to external packages in two ways:

1. **Recommended** - using *imports* in `DESCRIPTION` file in each package
2. **Not recommended** - using `library` or `require` in master scripts.

![Editing DESCRIPTION file](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_7a.png "Editing DESCRIPTION file")

To add dependencies to external packages we will edit the
`packages\mypackage\DESCRIPTION` file like below:

```
Package: mypackage
Type: Package
Title: What the package does (short line)
Version: 0.1
Date: 2018-04-25
Author: WitJakuczun
Maintainer: Who to complain to <yourfault@somewhere.net>
Description: More about what it does (maybe more than one line)
License: What license is it under?
Imports: 
	logging,
	data.table (>= 1.10.1)
RoxygenNote: 6.0.1
```


I have added the line `data.table (>= 1.10.1)` to the Depends section. This
means I declared that `mypackage` depends on `data.table` package in
version `1.10.1` or newer.

![Rebuilding packages](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_7b.png "Rebuilding packages")

Let's rebuild the package to have master scripts see the changes

```bash
rsuite proj build
```

The output is

```
2017-09-23 17:15:21 ERROR:rsuite:Some dependencies are not installed in project env: data.table. Please, install dependencies(Call RSuite::prj_install_deps)
ERROR: Some dependencies are not installed in project env: data.table. Please, install dependencies(Call RSuite::prj_install_deps)
```

You can conclude that you have to install dependencies in order to build your package.

# Step 8 - install dependencies

![Installing R Suite project
dependencies](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/basic_workflow_step_8.png "Installing R Suite
project dependencies")

To install dependencies you have to issue the following command:

```bash
rsuite proj depsinst
```

You should see the following output

```
2017-09-23 17:12:38 INFO:rsuite:Detecting repositories (for R 3.4)...
2017-09-23 17:12:39 INFO:rsuite:Will look for dependencies in ...
2017-09-23 17:12:39 INFO:rsuite:.          MRAN#1 = http://mran.microsoft.com/snapshot/2018-04-18 (win.binary, source)
2017-09-23 17:12:39 INFO:rsuite:Collecting project dependencies (for R 3.4)...
2017-09-23 17:12:39 INFO:rsuite:Resolving dependencies (for R 3.4)...
2017-09-23 17:12:46 INFO:rsuite:Detected 1 dependencies to install. Installing...
2017-09-23 17:12:51 INFO:rsuite:All dependencies successfully installed.
```

From this output you can see that we use `MRAN` as package
repository. Moreover R Suite detected 1 dependency to be installed. 

You can check if the installation was successful by issuing the following command

```bash
rsuite proj build
```

The output you should see if everything worked

```
2017-09-23 17:18:23 INFO:rsuite:Installing mypackage (for R 3.4) ...
2017-09-23 17:18:28 INFO:rsuite:Successfuly build 1 packages
```

Let's check what happens if you run our master script

```bash
Rscript R\master.R
```

The output says that `data.table` was loaded. This is exactly what we wanted.

# Step 9 - developing custom package using `devtools`

If you want to develop a package the dev-build cycle can take too long. This is especially important if the packages are bigger.
You can use `devtools` to speed up this process.

To do this edit `R\master.R` like this

```R
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
	if (!length(script_path)) { return(".") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

#library(mypackage)
devtools::load_all(file.path(script_path, "../packages/mypackage"))

hello_world("John")
```

We commented the line `library(mypackage)` and added a new line
```R
devtools::load_all(file.path(script_path, "../packages/mypackage"))
```

This line uses devtools to dynamically load your package. We are using the variable `script_path` which is
auto-initialized by R Suite and is a path pointing to `master.R`.

# Step 10 - loggers in master scripts

R Suite promotes good programming practices and using loggers is one of them. R Suite is based on  the `logging` package.

Let's update `R/master.R` as follows

```R
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
	if (!length(script_path)) { return(".") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

#library(mypackage)
devtools::load_all(file.path(script_path, "../packages/mypackage"))

loginfo("Master info")
logdebug("Master debug")
logwarn("Master warning")
logerror("Master error")

hello_world("John")
```

Let's check how it works

```bash
Rscript R\master.R
```

The output should look like this

```
Loading mypackage
Loading required package: data.table
2017-09-23 17:27:14 INFO::Master info
2017-09-23 17:27:14 WARNING::Master warning
2017-09-23 17:27:14 ERROR::Master error
[1] "Hello John!"
```

As you can see there are logging messages. You can see that the debug message is missing. 

## Step 10.1 - controlling loggers level

To see the debug logging message you have to edit the `config.txt` file in the project root folder to 
look like this

```
LogLevel: DEBUG
```

Let's check how it works

```bash
Rscript R\master.R
```

The output should look like this

```
Loading mypackage
Loading required package: data.table
2017-09-23 17:31:22 INFO::Master info
2017-09-23 17:31:22 DEBUG::Master debug
2017-09-23 17:31:22 WARNING::Master warning
2017-09-23 17:31:22 ERROR::Master error
[1] "Hello John!"
```

As you can see now the debug logging message is printed.

## Step 10.2 - `logs` folder

Logging messages are stored in the `logs` folder in files named with the current date.
You can check this by issuing the following command

```bash
dir logs
```

The output on my laptop looks like this

```
2017-09-23  17:27    <DIR>          .
2017-09-23  17:27    <DIR>          ..
2017-09-23  17:31               541 2017_09_23.log
```

When you open this log file in an editor you should see content similar to this

```
2017-09-23 17:27:14 INFO::Master info
2017-09-23 17:27:14 WARNING::Master warning
2017-09-23 17:27:14 ERROR::Master error
2017-09-23 17:30:54 INFO::Master info
2017-09-23 17:30:54 WARNING::Master warning
2017-09-23 17:30:54 ERROR::Master error
2017-09-23 17:31:06 INFO::Master info
2017-09-23 17:31:06 WARNING::Master warning
2017-09-23 17:31:06 ERROR::Master error
2017-09-23 17:31:22 INFO::Master info
2017-09-23 17:31:22 DEBUG::Master debug
2017-09-23 17:31:22 WARNING::Master warning
2017-09-23 17:31:22 ERROR::Master error
```

As you can see this is very similar to the output you saw in console.


# Step 11 - loggers in packages

R Suite allows you to use loggers in your custom packages. Let's open `hello_world.R` and change its content to the following one

```R
#'@export
hello_world <- function(name) {
  pkg_loginfo("Package info")
  pkg_logdebug("Package debug")
  pkg_logwarn("Package warning")
  pkg_logerror("Package error")

  print(sprintf("Hello %s!", name))
}
```

You can check how it works by issuing this command

```bash
Rscript R\master.R
```

The output you should see

```
Loading mypackage
Loading required package: data.table
2017-09-23 17:37:52 INFO::Master info
2017-09-23 17:37:52 DEBUG::Master debug
2017-09-23 17:37:52 WARNING::Master warning
2017-09-23 17:37:52 ERROR::Master error
2017-09-23 17:37:52 INFO:mypackage:Package info
2017-09-23 17:37:52 DEBUG:mypackage:Package debug
2017-09-23 17:37:52 WARNING:mypackage:Package warning
2017-09-23 17:37:52 ERROR:mypackage:Package error
[1] "Hello John!"
```

As you can see there are messages from your package. They are marked with the package name `mypackage`. Please also note that
as you used `devtools` in your master script you did not have to build the package to see the changes.

# Step 12 - Project environment locking 
RSuite allows the user to lock the project environment. It collects all dependencies' versions and stores them in a lock file to enforce exact dependency versions in the future. To lock the project environment you have to execute the following command:

```bash
rsuite proj lock
```

The lock file is in the ```deployment``` directory under the ```env.lock``` name. It is a dcf file that stores information about packages in the local environment together with their versions. In order to see the content of the ```env.lock``` file run the following command:

```bash
type my_project\deployment\env.lock
```

When dependencies are being installed using `rsuite proj depsinst` the `env.lock` file will be used to detect whether any package will change versions. If that's the case an appropriate warning message will be displayed. The feature allows to safely deploy packages with specific dependencies' versions. It prevent errors caused by newer versions of packages which might work differently than previous ones used in the project. 

To safely unlock the local project environment use the following command:

```bash
rsuite proj unlock
```
The command deletes an existing `env.lock` file.

# Step 13 - prepare deployment package

We can now prepare a deployment package to ship our project on a production. To do this you have to first remove `devtools` from `master.R`

```R
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
	if (!length(script_path)) { return(".") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(mypackage)

loginfo("Master info")
logdebug("Master debug")
logwarn("Master warning")
logerror("Master error")

hello_world("John")
```

Now you have to install dependencies

```bash
rsuite proj depsinst
```

As you did not add any new dependencies R Suite smartly understands it and the lengthy dependencies installation phase is not repeated.

Let's build our custom packages

```bash
rsuite proj build
```

You should see the following output

```
2017-09-23 17:44:20 INFO:rsuite:Installing mypackage (for R 3.4) ...
2017-09-23 17:44:25 INFO:rsuite:Successfuly build 1 packages
```

To build a deployment package you use the following command

```bash
rsuite proj zip
```

You can see that if the project is not under version control the command returns an error. You have to explicitly give a version number for your deployment package

```bash
rsuite proj zip --version=1.0
```

The output should be like this

```
2017-09-23 17:49:01 INFO:rsuite:Installing mypackage (for R 3.4) ...
2017-09-23 17:49:06 INFO:rsuite:Successfuly build 1 packages
2017-09-23 17:49:06 INFO:rsuite:Preparing files for zipping...
2017-09-23 17:49:07 INFO:rsuite:... done. Creating zip file my_project_1.0x.zip ...
2017-09-23 17:49:08 INFO:rsuite:Zip file created: C:/Workplace/Projects/my_project/my_project_1.0x.zip
```

You have created the `my_project_1.0x.zip` file that contains all information necessary to run your solution on a production environment.

# Step 14 - running deployment package

To test if the deployment package is working you can extract `my_project_1.0x.zip` created in the previous step in a new folder say `prod`.
Now you can run your solution with the command

```bash
Rscript my_project\R\master.R
```

The output you should see

```
2017-09-23 17:53:34 INFO::Master info
2017-09-23 17:53:34 WARNING::Master warning
2017-09-23 17:53:34 ERROR::Master error
2017-09-23 17:53:34 INFO:mypackage:Package info
2017-09-23 17:53:34 WARNING:mypackage:Package warning
2017-09-23 17:53:34 ERROR:mypackage:Package error
[1] "Hello John!"
```

As you can see the output is exactly the same as you would expect.
