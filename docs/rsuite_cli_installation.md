# How to install R Suite CLI

R Suite CLI is a command line api to functionalities that the R Suite package provides. It also provides some integration features (with docker e.g.)
based on R Suite functionalities.

R Suite CLI is just a simple command line shell with enhanced arguments parsing and help system (and bash completion configuration for bash).
It internally performs some environment checks (like detection there R is installed on Windows) and calls R to get access to R Suite.

R Suite and R Suite calling code are platform independent but R Suite CLI depends on platform: on Windows it is a cmd script on Linuxes it is a
bash script.

To make the start of the journey with R Suite CLI as easy as possible we prepared installation packages for you. Herein we discuss how they can be
installed on each supported platform. You can download them from [R
Suite Download](http://rsuite.io/RSuite_Download.php).

## **Got stuck?**

If you are stuck feel free to contact us:

* through the R Suite website (http://rsuite.io#contact) or 
* using Gitter [R Suite room](https://gitter.im/WLOGSolutions/RSuite
  "R Suite room")
* directly by sending an email with your problem description to
  [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).
  
# Windows installation

For Windows R Suite CLI is provided as MSI packages for both x86 and x64 platforms. You can just download the appropriate MSI and run it.
It will install R Suite CLI and will add a reference to it to your PATH so when you open the console rsuite command will be available. 

You can check that everything installed properly with the following command:

``` bash
> rsuite version
```

R Suite CLI on first run will try to detect if R is available. If it is not available in  the PATH it will search for the R installation in the windows
registry. 

# Debian (Ubuntu) installation

For Debian-like systems R Suite CLI is provided as DEB package. It is architecture independent. It has some dependencies which will be
required to install the R Suite package (libssl-dev libxml2-dev libcurl4-openssl-dev zlib1g-dev).

You will need administrative privileges (root access) to install DEP package. It is a requirement of Linux package management system.

As the DEP package has some dependencies you cannot install simply with dpkg utility. If some of the dependencies are not present on your
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

# RedHat (CentOS, Fedora) installation

For RedHat-like systems R Suite CLI is provided as RPM package. It is architecture independent. It has some dependencies which will be
required to install the R Suite package (openssl-devel libxml2-devel libcurl-devel zlib-devel).

You will need administrative privileges (root access) to install RPM package. It is a requirement of the Linux package management system.

As the RPM package has some dependencies you cannot install simply with rpm utility. If some of the dependencies are not present on your
system rpm will complain about them. But you still can use yum which will find required packages in package repositories available and
will install them all together:

``` bash
$ yum install rsuitecli-0.22.231-1.noarch.rpm
```

You can check that everything installed properly with the following command:

``` bash
$ rsuite version
```

# Mac OS installation

For Mac OS R Suite CLI is provided as PKG packages. You can just download it and let Mac OS package manager do the rest.
It will install R Suite CLI and will link it to /usr/local/bin/rsuite so when you open terminal rsuite command will be available. 

You can check that everything installed properly with the following command:

``` bash
$ rsuite version
```


# After you installed R Suite CLI

After R Suite CLI is installed you probably will need to install the R Suite package into you R environment:

``` bash
> rsuite install
```

For the full description of how to use R Suite CLI please refer to [R Suite CLI reference manual](http://rsuite.io/RSuite_Tutorial.php?article=rsuite_cli_reference.md).
