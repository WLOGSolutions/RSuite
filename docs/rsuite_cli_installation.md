# How to install RSuite CLI

RSuite CLI is command line api to functionalities RSuite package provides. It also provides some integration features (with docker e.g.)
based on RSuite functionalities.

RSuite CLI is just simple command line shell with enhanced arguments parsing and help system (and bash completion configuration for bash).
It internally performs some environment checks (like detection there R is installed on Windows) and calls R to get access to RSuite.

RSuite and RSuite calling code are platform independent but RSuite CLI depends on platform: on Windows it is cmd script on Linuxes it is
bash script.

To make start of journey with RSuite CLI as easier as possible we prepared installation packages for you. Herein we discuss how they can be
installed on each supported platform. You can download them from [R
Suite Download](http://rsuite.io/RSuite_Download.php).

## **Got stuck?**

If you are stuck fill free to contact us:

* through RSuite website (http://rsuite.io#contact) or 
* using Gitter [RSuite room](https://gitter.im/WLOGSolutions/RSuite
  "RSuite room")
* directly by sending email with your problem description to
  [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).
  
## Windows installation

For Windows RSuite CLI is provided as MSI packages for both x86 and x64 platforms. You can just download appropriate MSI and run it.
It will install RSuite CLI and will add reference to it to you PATH so then you open console rsuite command will be available. 

You can check that everything installed properly with following command:

``` bash
> rsuite version
```

RSuite CLI on first run will try to detect if R is available. It not available in PATH it will search for R installation in windows
registry. 

## Debian (Ubuntu) installation

For Debian-like systems RSuite CLI is provided as DEB package. It is architecture independent. It has some dependencies which will be
required to install RSuite package (libssl-dev libxml2-dev libcurl4-openssl-dev zlib1g-dev).

You will need administrative privileges (root access) to install DEP package. It is requirement of Linux package management system.

As the DEP package has some dependencies you cannot install simply with dpkg utility. If some of dependencies are not present on your
system dpkg will complain about them. But you still can use apt-get which will find required packages in apt repositories available and
will install them all together:

``` bash
$ apt-get install -f ./rsuitecli_0.22.231-1_all.deb
```

Pay attention to the way DEB package is specified to apt-get. You must provide path(like ./ in the example) so apt-get will recognize it 
as local package file and will not try to find it in apt repositories.

Option -f provided to apt-get stands for --fix-broken and tells apt-get to install rsuitecli package then fix (install) all dependencies it
requires.

You can check that everything installed properly with following command:

``` bash
$ rsuite version
```

## RedHat (CentOS, Fedora) installation

For RedHat-like systems RSuite CLI is provided as RPM package. It is architecture independent. It has some dependencies which will be
required to install RSuite package (openssl-devel libxml2-devel libcurl-devel zlib-devel).

You will need administrative privileges (root access) to install RPM package. It is requirement of Linux package management system.

As the RPM package has some dependencies you cannot install simply with rpm utility. If some of dependencies are not present on your
system rpm will complain about them. But you still can use yum which will find required packages in package repositories available and
will install them all together:

``` bash
$ yum install rsuitecli-0.22.231-1.noarch.rpm
```

You can check that everything installed properly with following command:

``` bash
$ rsuite version
```

## After you installed RSuite CLI

After RSuite CLI is installed you probably will need to install RSuite package into you R environment:

``` bash
rsuite install
```

For full description of how to use RSuite CLI please refer to [RSuite CLI reference manual](http://rsuite.io/RSuite_Tutorial.php?article=rsuite_cli_reference.md).
