# How to install RSuite

RSuite is an R package so it is platform independent. We **strongly
recommend** using R Suite CLI to install R Suite package. But there
are other options that could fit your needs better.

## **Got stuck?**

If you are stuck feel free to contact us:

* through RSuite website (http://rsuite.io#contact) or 
* using Gitter [RSuite room](https://gitter.im/WLOGSolutions/RSuite "RSuite room")
* directly by sending an email with your problem description to [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).

# Requirements

RSuite depends on number of other R packages. On Linux systems they require
libxml2-devel, libssl-devel, libcurl-devel and zlib-devel system packages.

On RedHat like systems (RedHat/Fedora/CentOS) you can install them 
with following command:

```bash
$ sudo yum install -y openssl-devel libxml2-devel libcurl-devel zlib-devel
```

On Debian like systems (Debian/Ubuntu) you can install them executing:

```bash
$ sudo apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev zlib1g-dev
```

**Important:** If you are installing using RSuite CLI (see below) the
required dependencies are installed automatically.

On Windows it is necessary to have Rtools installed. Rtools contains number of utilities for building R packages 
(e.g. zip - required to build binary packages on windows).

# Installation with help of RSuite CLI [Recommended]

![Installing R Suite with R Suite CLI](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/rsuite_install_with_cli.png "Installing R Suite with R Suite CLI")

If you have RSuite CLI installed (check [RSuite CLI installation
reference](http://rsuite.io/RSuite_Tutorial.php?article=rsuite_cli_installation.md
"RSuite CLI installation reference.")) already you can use it to
install latest compatible version of RSuite. Just execute the following
command in your shell:

```bash
$ rsuite install
```

If any problems occur try to run this command in verbose mode. It will probably
give you hints on why the problem occurred:

```bash
$ rsuite install -v
```

# Basic installation

You can simply install RSuite from WLOG S3 repository executing the following command
in your R environment:

```bash
> install.packages('RSuite', repo = c(options('repos'), 'http://wlog-rsuite.s3.amazonaws.com'))
```

It will install the latest released version of RSuite in the first folder mentioned in 
.libPaths().

The complicated parameter repo instructs install.packages to use beside standard repositories
also RSuite repository located in S3 bundle.

# Development version installation

RSuite sources are publicly available. You can install the development version of 
RSuite from GitHub by executing the following code in your R environment:

```bash
> devtools::install_github('WLOGSolutions/RSuite/packages/RSuite')
```

# Updating RSuite

You can check which version of RSuite is currently installed in your R environment
with the following code:

```bash
> packageVersion("RSuite")
```

You can check what is the latest released version of RSuite using the following code:

```bash
> RSuite::rsuite_check_version()
```

You can upgrade RSuite any time by executing the following code:

```bash
> RSuite::rsuite_update()
```

It will check if the currently installed version of RSuite is up to date and if it's
not, the latest released version will be installed from the WLOG repository.

**ATTENTION:** RSuite CLI requires compatible version of RSuite to work properly.
Version of RSuite and RSuite CLI has form \<Maj\>.\<Min\>-\<Rel\>. Compatibility is 
detected comparing if \<Maj\>.\<Min\> matches for RSuite and RSuite CLI. Then upgrading
to the latest released version of RSuite compatibility with RSuite CLI can be broken.
In that case you will need to upgrade RSuite CLI also.
