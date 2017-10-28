# How to install RSuite

RSuite is R package so it is platform independent. 

## Requirements

RSuite depends on number of other R packages. On Linux systems they require
libxml2-devel, libssl-devel and libcurl-devel system packages.

On RedHat like systems (RedHat/Fedora/CentOS) executing you can install them 
with following command:

```bash
$ sudo yum install -y openssl-devel libxml2-devel libcurl-devel
```

On Debian like systems (Debian/Ubuntu) you can install them executing:

```bash
$ sudo apt-get install -y libssl-dev libxml2-dev libcurl4-openssl-dev 
```

## Basic installation

You can simply install RSuite from WLOG S3 repository executing following command
in your R environment:

```bash
> install.packages('RSuite', repo = 'http://wlog-rsuite.s3.amazonaws.com')
```

It will install latest released version of RSuite in first folder mentioned in 
.libPaths().

## Development version installation

RSuite sources are publicly available. You can install development version of 
RSuite from GitHub execute following code in your R environment:

```bash
> devtools::install_github('WLOGSolutions/RSuite/packages/RSuite')
```

## Updating RSuite

You can check which version of RSuite is currently installed in your R environment
with following code:

```bash
> packageVersion("RSuite")
```

You can check which version of RSuite is latest released with following code:

```bash
> RSuite::rsuite_check_version()
```

You can upgrate RSuite any time executing following code:

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

## Installation with help of RSuite CLI

If you have RSuite CLI installed already you can use it to install latest 
compatible version of RSuite. Just execute following command in you shell:

```bash
$ rsuite install
```

If any problems occure try to run this command in verbose mode. It will probably
give you hint on why the problem occured:

```bash
$ rsuite install -v
```

If you are stuck fill free to contact us through RSuite website (http://rsuite.io#contact) 
or directly by sending email with your problem description to 
<!--html_preserve-->
<a href="mailto:rsuite@wlogsolutions.com">rsuite@wlogsolutions.com</a>
<!--/html_preserve-->.

