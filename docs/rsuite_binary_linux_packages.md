# Binary packages under Linux #

In this document we present a very handy feature of [R Suite](https://rsuite.io) - handling binary packages under Linux.
We will cover the following topics:

* creating local CRAN repos with binary packages
* setting up R Suite project to install from local CRAN

**Important** This tutorial was tested with R Suite version 0.35-251 and Ubuntu 18.04.

## Got stuck? ##

If you are stuck feel free to contact us:

through the R Suite website (https://rsuite.io#contact) or
using Gitter R Suite room
directly by sending an email with the description of your problem to rsuite@wlogsolutions.com.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

## **Table of Contents** ##

- [Motivation](#motivation)
- [Create local CRAN](#create-local-cran)
    - [First step - understand CRAN management under R Suite](#first-step---understand-cran-management-under-r-suite)
- [Create R Suite project for building binary packages](#create-r-suite-project-for-building-binary-packages)
    - [Add dependencies to your project](#add-dependencies-to-your-project)
    - [Install dependencies (long!)](#install-dependencies-long)
- [Add binary packages to CRAN](#add-binary-packages-to-cran)
- [Use binary packages for other project](#use-binary-packages-for-other-project)
    - [Create project `ShinyApp`](#create-project-shinyapp)
    - [Add local CRAN to project's repositories](#add-local-cran-to-projects-repositories)
    - [Code your App](#code-your-app)
- [Installing dependencies (fast!)](#installing-dependencies-fast)

<!-- markdown-toc end -->

# Motivation

Installing R packages under Linux is very time consuming. This is because packages are compiled. This is not the case for Windows OS. It is because packages for Windows are binary and R just have to download them and unzip. 

We wanted to have this functionality for Linux but unfortunately bare R is not handling binary packages. Simply you cannot `install.packages(dplyr)` with binary CRAN. We had to tweak package installation to make it possible with R Suite.

# Create local CRAN #

We will start with creating local CRAN repository in a folder - here we are using `CRAN` in user's home directory. For this we have to run the following command

```bash
rsuite repo init -d CRAN -b TRUE
```

The result should be like this:
```
2019-02-07 19:55:39 WARNING::Repository directory does not exist: CRAN. Will create it
2019-02-07 19:55:39 INFO:rsuite:Repo manager for Dir created.
2019-02-07 19:55:39 INFO:rsuite:Repo manager destroyed.rsuite repo init 
```

Here we have used two options:

- `-d` which gives a folder where you want to create CRAN repo
- `-b TRUE` which says that you want to create CRAN for *binary* packages.

## First step - understand CRAN management under R Suite ##

R Suite helps manage CRAN like repos. Currently we support repos in local folders and at [AWS S3 service](https://aws.amazon.com/s3/). To check list of functionalities run the following command

```bash
rsuite repo
```

You should see similar output as below:

```
The command helps you manage R repositories.

Usage: rsuite repo [subcmd] [subcmd args]

Sub-commands
	    init
                Initialize repository structure.
        addproj
                Upload project packages into repository.

        addfile
                Upload package files into repository.

        addext
                Upload external packages by name into repository. Will look for packages in repositories of project in context.

        addpkgzip
                Upload contents of PKGZIP into repository.

        addgithub
                Upload package build from GitHub into repository.

        addbioc
                Upload package build from BioConductor into repository.

        list
                Retrieve list of packages available in repository.

        remove
                Removes specified packages from repository.

        help
                Show this message and exit.

Call rsuite repo [subcmd] -h to get more information on [subcmd args].
```

As you can see there are many functions to support CRAN repositories with R Suite. In this tutorial we are using: **init** and **addext**. For more details follow this [link](https://rsuite.io/RSuite_Tutorial.php?article=rsuite_cli_reference.md#repository-management).

# Create R Suite project for building binary packages #

R Suite is very rigid as far as reproducibility is concerned. Because of this you cannot just add a package to a repository. You have to declare context for this - you do this by creating a new R Suite project.

Issue the following command

```bash
rsuite proj start -n BinaryCRAN
```

## Add dependencies to your project ##

Lets assume we want to have binary `shiny` package and its dependencies in our local CRAN. Lets edit `R\master.R` file in our `BinaryCRAN` project so it looks like the code below:

```r
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
  if (!length(script_path)) {
    return("R")
  }
  if (grepl("darwin", R.version$os)) {
    base <- gsub("~\\+~", " ", base) # on MacOS ~+~ in path denotes whitespace
  }
  return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(shiny)
```

**Remark** R Suite understands that R Studio is very popular. Each R Suite project is compatible with R Studio - in a project root folder you can find `BinaryCRAN.prj` file - if you click this, R Studio will open the project.

## Install dependencies (long!) ##

Now you are ready to install dependencies. **Beware** that it will take a lot of time because of compilation. You install dependencies with the following command:

```bash
rsuite proj depsinst -v
```

On my laptop it took around 6 minutes to install all dependencies. At the end you should see the following message

```
2019-02-07 21:34:33 INFO:rsuite:All dependencies successfully installed.
2019-02-07 21:34:33 DEBUG:rsuite:Collecting project support(All) packages (for R 3.5)...
2019-02-07 21:34:33 DEBUG:rsuite:> cmd: /usr/local/lib/R/bin/Rscript --no-init-file --no-site-file -e ".Library <- NULL;.libPaths(c(new=c('/home/rstudio/projects/blog/BinaryCRAN/deployment/sbox', '/home/rstudio/projects/blog/BinaryCRAN/deployment/libs'), Sys.getenv('R_LIBS_USER'), .Library.site));cat('Lib paths:\\n');void <- lapply(.libPaths(), function(lp) { cat(paste0('\\t', lp, '\\n')) });tryCatch({  suppressWarnings({ installed <- utils::installed.packages();installed <- as.data.frame(installed, stringsAsFactors = F)[, c('Package', 'Version', 'Built')];load(file='/tmp/RtmptPWlul/filee7c103a09c0.RData');installed <- installed[installed\$Package %in% pkgs, ];installed <- installed[!duplicated(installed\$Package), ];loadable <- unlist(lapply(   X = installed\$Package,   FUN = function(pkg) {     tryCatch({      suppressPackageStartupMessages(library(pkg, character.only = TRUE));      pkg     },     error = function(e) NULL)   }));installed <- installed[installed\$Package %in% loadable, ];save(installed, file='/tmp/RtmptPWlul/filee7c5064b4df.RData') });  cat(sprintf('~ done\\n'))}, error = function(e) {  cat(sprintf('~ error:%s\\n', e))})" 2>&1
2019-02-07 21:34:33 DEBUG:rsuite:> Lib paths:
2019-02-07 21:34:33 DEBUG:rsuite:>      /home/rstudio/projects/blog/BinaryCRAN/deployment/sbox
2019-02-07 21:34:33 DEBUG:rsuite:>      /home/rstudio/projects/blog/BinaryCRAN/deployment/libs
2019-02-07 21:34:33 DEBUG:rsuite:>      /usr/local/lib/R/library
2019-02-07 21:34:33 DEBUG:rsuite:> ~ done
2019-02-07 21:34:33 DEBUG:rsuite:No project support packages required.
```

This message says that you managed to install all dependencies and that there were no supporting packages required.

As a last check you can list all packages with the following command:

```bash
ls -l deployment/libs
```

You should see output similar to the one below

```
total 0
drwxrwxrwx 2 root root 0 Feb  7 21:30 BH
drwxrwxrwx 2 root root 0 Feb  7 21:26 R6
drwxrwxrwx 2 root root 0 Feb  7 21:26 Rcpp
drwxrwxrwx 2 root root 0 Feb  7 21:25 crayon
drwxrwxrwx 2 root root 0 Feb  7 21:25 digest
drwxrwxrwx 2 root root 0 Feb  7 21:31 htmltools
drwxrwxrwx 2 root root 0 Feb  7 21:33 httpuv
drwxrwxrwx 2 root root 0 Feb  7 21:25 jsonlite
drwxrwxrwx 2 root root 0 Feb  7 21:30 later
drwxrwxrwx 2 root root 0 Feb  7 21:25 logging
drwxrwxrwx 2 root root 0 Feb  7 21:25 magrittr
drwxrwxrwx 2 root root 0 Feb  7 21:25 mime
drwxrwxrwx 2 root root 0 Feb  7 21:31 promises
drwxrwxrwx 2 root root 0 Feb  7 21:25 rlang
drwxrwxrwx 2 root root 0 Feb  7 21:34 shiny
drwxrwxrwx 2 root root 0 Feb  7 21:24 sourcetools
drwxrwxrwx 2 root root 0 Feb  7 21:24 xtable
```

# Add binary packages to CRAN #

When installing dependencies jos is finished you are ready to populate your local CRAN with the packages. For this you have to be in `BinaryCRAN` project and run the following command

```bash
rsuite repo addext -n shiny --with-deps -d ~/CRAN -b TRUE
```

Where meaning of the used options is

* `-n shiny` add external package called `shiny`. You can list more using `,` as a separator.
* `-d ~/CRAN` use directory `~/CRAN` for your local CRAN.
* `--with-deps` add also dependencies to the CRAN
* `-b TRUE` add **binary** packages

**Remark** pay attention to option `-d`, you should give valid path to the CRAN folder you have created in this [step](#create-local-cran).

After few minutes of compiling `shiny` and its dependencies you should see packages in your CRAN folder. Check this with the following command

```bash
ls -l CRAN/bin/deb18.04_x86_64-pc-linux-gnu/contrib/3.5/
```
You should see output similar to the one below

```
total 28156
-rwxr-xr-x 1 root root 11394523 Feb  7 21:45 BH_1.69.0-1_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root     4207 Feb  7 21:45 PACKAGES
-rwxr-xr-x 1 root root     1367 Feb  7 21:45 PACKAGES.gz
-rwxr-xr-x 1 root root    55426 Feb  7 21:45 R6_2.3.0_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root  4601885 Feb  7 21:45 Rcpp_1.0.0_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   747629 Feb  7 21:45 crayon_1.3.4_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   203589 Feb  7 21:45 digest_0.6.18_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   396983 Feb  7 21:45 htmltools_0.3.6_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root  2450528 Feb  7 21:45 httpuv_1.4.5.1_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root  1154041 Feb  7 21:45 jsonlite_1.6_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   652289 Feb  7 21:45 later_0.7.5_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   154027 Feb  7 21:45 magrittr_1.5_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root    36994 Feb  7 21:45 mime_0.6_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   400029 Feb  7 21:45 promises_1.0.1_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root  1141843 Feb  7 21:45 rlang_0.3.1_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root  4486926 Feb  7 21:45 shiny_1.2.0_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   160775 Feb  7 21:45 sourcetools_0.1.7_R_x86_64-pc-linux-gnu.tar.gz
-rwxr-xr-x 1 root root   750569 Feb  7 21:45 xtable_1.8-3_R_x86_64-pc-linux-gnu.tar.gz
```

As you can see there is a `bin` folder inside subfolder with packages for Ubuntu.

# Use binary packages for other project #

Now we will use our binary packages for a second project `ShinyApp`.

## Create project `ShinyApp` ##

Lets start with creating R Suite project.

```bash
rsuite proj start -n ShinyApp 
```

## Add local CRAN to project's repositories ##

R Suite project can have more than one CRAN repository. They are defined in `PARAMETERS` file that you can find in project's root folder. Open it and edit to look like the one below

```
RSuiteVersion: 0.35.251
RVersion: 3.5
Project: ShinyApp
Repositories: Dir[../CRAN], MRAN[2019-02-07]
Artifacts: config_templ.txt
```

**Remark** pay attention to the path you put for `Dir` option. It must be either *relative* to project's root folder or *explicit* and it **must** point to your local CRAN folder. In my case I was creating `CRAN` in my user's home directory so adding `../CRAN` is working properly.

## Code your App ##

You can code your App and make it as fancy as you want. For the purpose of this post I will just modify `R\master.R` file to the following code

```r
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
  if (!length(script_path)) {
    return("R")
  }
  if (grepl("darwin", R.version$os)) {
    base <- gsub("~\\+~", " ", base) # on MacOS ~+~ in path denotes whitespace
  }
  return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(shiny)
```

# Installing dependencies (fast!) #

Now you are ready to check if your binary packages speeds up installation process. Go into the `ShinyApp` root and run the command:

```bash
rsuite proj depsinst -v
```

You should see the following output

```
2019-02-07 21:59:23 INFO:rsuite:Detecting repositories (for R 3.5)...
2019-02-07 21:59:23 DEBUG:rsuite:Trying to cache availables from https://mran.microsoft.com/snapshot/2019-02-07/bin/deb18.04_x86_64-pc-linux-gnu/contrib/3.5 ...
2019-02-07 21:59:27 WARNING:rsuite:Project is configured to use non reliable repositories: Dir. You should use only reliable repositories to be sure of project consistency over time.
2019-02-07 21:59:27 INFO:rsuite:Will look for dependencies in ...
2019-02-07 21:59:27 INFO:rsuite:.           Dir#1 = file:////home/rstudio/projects/blog/CRAN (source, binary)
2019-02-07 21:59:27 INFO:rsuite:.          MRAN#2 = https://mran.microsoft.com/snapshot/2019-02-07 (source)
2019-02-07 21:59:27 INFO:rsuite:Collecting project dependencies (for R 3.5)...
2019-02-07 21:59:27 INFO:rsuite:Resolving dependencies (for R 3.5)...
2019-02-07 21:59:30 INFO:rsuite:Detected 16 dependencies to install. Installing...
```

As you can see the first repository is your local CRAN. This is **very important** because R Suite checks repositories in an order they were put in `PARAMETERS` file.

But take a look to other messages. You will see something similar to the one below

```
2019-02-07 21:59:31 DEBUG:rsuite:> * installing *binary* package 'xtable' ...
2019-02-07 21:59:32 DEBUG:rsuite:> * DONE (xtable)
```

As you can see R Suite is using **binary** packages! 

On my laptop it took around 3 minutes to install all packages. **This means I achieved a double reduction of installation time**.
