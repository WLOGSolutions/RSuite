# What is R Suite?
R Suite was developed by [WLOG Solutions](http://wlogsolutions.com) company to make their development and deployment process robust. R Suite gives answers to the following challenges for any R based software solution:

* Isolated and reproducible projects with controlled dependencies and configuration.
* Separation of business, infrastructural and domain logic.
* Package based solution development.
* Management of custom CRAN-alike repositories.
* Automation of deployment package preparation.
* Development process integrated with version control system (currently Git and SVN).
* Working in internet-less environments.

# How to use it
To create project use

``` bash
RSuite::prj_start(name = "myproject")
```

This will create R Suite project folder structure under myproject folder in your getwd. It will also register its contents under SVN or Git
if detects that getwd is under version control. 

Project folder structure is following:

* packages - your project R packages will go here
* R - master scripts (scripts to run your solutions functionalities) will go here. Default master script is created for you: master.R
* test - your integration tests will go here
* deployment - project isolated environment will be created in libs sub folder here. Project packages built will go into intrepo sub folder. 
* logs - your solution logs will go here.
* config_templ.txt - template for your solution configuration. It will be copied onto config.txt (if not exists) on first run 
  of any muster script.
* PARAMETERS - parameters of your project like project name and repositories it should use to collect isolated project environment.

Then you have project created setwd to anywhere inside it.

To create package inside your project simply run:

``` bash
RSuite::prj_start_package(name = "mypackage")
```

Package will be created under packages sub folder of R Suite project folder (myproject/packages/mypackage). It has standard structure except
some commonly used functionalities added for your convenience:

* package_logger.R - initialization of logger for the package
* package_validation.R - some utilities for arguments validation and assertions.
* packages_imports.R - global imports which will be used by roxygen2 to add appropriate imports into package NAMESPACE.

As usual edit package DESCRIPTION file and add some functionalities to your package.

Now you are ready to build you project isolated environment. First you have to collect dependencies of your project:

``` bash
RSuite::prj_install_deps()
```

This will look onto dependencies of your project packages and library (or require) commands in your master scripts to detect all the 
3rdparty dependencies of your project. It will collect also version requirements from DESCRIPTION files of your project packages. After that 
it will detect packages (and their dependencies) in repositories provided in PARAMETERS file of R Suite project (which satisfy version
requirements) and install them into deployment/libs subfolder.

If everything goes well you are ready to build your project packages:

``` bash
RSuite::prj_build()
```

It will build each package in your R Suite project and install it into deployment/libs. If it succeeds your isolated project environment is
ready. 

Now work on your master scripts. We are convinced that business logic of your solution should be enclosed into packages provided by solution.
Master scripts are required for "glue logic" like read configuration parameters, setup connections and realize data flow between levels of
solution functionality stack.

After all you can prepare your solution for deployment:

``` bash
RSuite::prj_zip()
```

This will create zip bundle containing you isolated project environment together with your master scripts and configuration template. Just 
extract it on your production and run your master scripts (no package installation will be required in production environment).

For more information check R Suite's [official webpage](http://rsuite.io)!


# How to install it
Just run 

``` bash
install.packages('RSuite')
```
