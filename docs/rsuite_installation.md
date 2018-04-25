# How to install RSuite

RSuite is an R package so it is platform independent. We **strongly
recommend** using R Suite CLI to install R Suite package. But there
are other options that could fit better your needs.

## **Got stuck?**

If you are stuck fill free to contact us:

* through RSuite website (http://rsuite.io#contact) or 
* using Gitter [RSuite room](https://gitter.im/WLOGSolutions/RSuite "RSuite room")
* directly by sending email with your problem description to [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).

# Requirements

RSuite depends on number of other R packages. On Linux systems they require
libxml2-devel, libssl-devel, libcurl-devel and zlib-devel system packages.

On RedHat like systems (RedHat/Fedora/CentOS) executing you can install them 
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

# Installation with help of RSuite CLI [Recommended]

If you have RSuite CLI installed (check [RSuite CLI installation
reference](http://rsuite.io/RSuite_Tutorial.php?article=rsuite_cli_installation.md
"RSuite CLI installation reference.") already you can use it to
install latest compatible version of RSuite. Just execute following
command in you shell:

```bash
$ rsuite install
```

If any problems occur try to run this command in verbose mode. It will probably
give you hint on why the problem occurred:

```bash
$ rsuite install -v
```

# Basic installation

You can simply install RSuite from WLOG S3 repository executing following command
in your R environment:

```bash
> install.packages('RSuite', repo = c(options('repos'), 'http://wlog-rsuite.s3.amazonaws.com'))
```

It will install latest released version of RSuite in first folder mentioned in 
.libPaths().

The complicated parameter repo instructs install.packages to use beside standard repositories
also RSuite repository located in S3 bundle.

# Development version installation

RSuite sources are publicly available. You can install development version of 
RSuite from GitHub execute following code in your R environment:

```bash
> devtools::install_github('WLOGSolutions/RSuite/packages/RSuite')
```

# Updating RSuite

You can check which version of RSuite is currently installed in your R environment
with following code:

```bash
> packageVersion("RSuite")
```

You can check which version of RSuite is latest released with following code:

```bash
> RSuite::rsuite_check_version()
```

You can upgrade RSuite any time executing following code:

```bash
> RSuite::rsuite_update()
```

It will check if currently installed version of RSuite is up to date and if it's
not will install latest released version from WLOG repository.

**ATTENTION:** RSuite CLI requires compatible version of RSuite to work properly.
Version of RSuite and RSuite CLI has form <Maj>.<Min>-<Rel>. Compatibility is 
detected comparing if <Maj>.<Min> match for RSuite and RSuite CLI. Then upgrading
to latest released version of RSuite compatibility with RSuite CLI can be broken.
In that case you will need to upgrade RSuite CLI also.

