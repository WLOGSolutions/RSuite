# What is R Suite?
R Suite was developed by [WLOG Solutions](https://wlogsolutions.com) company to make their development and deployment process robust. R Suite gives answers to the following challenges for any R based software solution:

* Isolated and reproducible projects with controlled dependencies and configuration.
* Separation of business, infrastructural and domain logic.
* Package based solution development.
* Management of custom CRAN-alike repositories.
* Automation of deployment package preparation.
* Development process integrated with version control system (currently Git and SVN).
* Working in internet-less environments.

# How to use it
To create a project use

``` bash
RSuite::prj_start(name = "myproject")
```

This will create an R Suite project folder structure under the myproject folder in your getwd. It will also register its contents under SVN or Git if R Suite detects that getwd is under version control. 

Project folder structure is the following:

* packages - your project R packages will go here
* R - master scripts (scripts to run your solutions functionalities) will go here. Default master script is created for you: master.R
* test - your integration tests will go here
* deployment - project isolated environment will be created in libs sub folder here. Project packages built will go into intrepo sub folder. 
* logs - your solution logs will go here.
* config_templ.txt - template for your solution configuration. It will be copied onto config.txt (if it does not exist) on the first run of any master script.
* PARAMETERS - parameters of your project like project name and repositories it is used to collect the isolated project environment.

After creating the project setwd to anywhere inside it

To create a package inside your project simply run:

``` bash
RSuite::prj_start_package(name = "mypackage")
```

A package will be created under the packages subfolder of the R Suite project folder (myproject/packages/mypackage). It has a standard structure except for some commonly used functionalities added for your convenience:

* package_logger.R - initialization of logger for the package
* package_validation.R - some utilities for arguments validation and assertions.
* packages_imports.R - global imports which will be used by roxygen2 to add appropriate imports into package NAMESPACE.

As usual you can edit the package DESCRIPTION file in order to add some functionalities to your package.

Now you are ready to build your isolated project environment. First you have to collect dependencies of your project:

``` bash
RSuite::prj_install_deps()
```

This will look onto dependencies of your project packages and library (or require) commands in your master scripts to detect all the  3rdparty dependencies of your project. It will also collect version requirements from the DESCRIPTION files of your project packages. After that  it will detect packages (and their dependencies) in repositories provided in the PARAMETERS file of R Suite project (which satisfy version requirements) and install them into the deployment/libs subfolder. If everything goes well you are ready to build your project packages:

``` bash
RSuite::prj_build()
```

It will build all packages in your R Suite project and install them into deployment/libs. If it succeeds your isolated project environment is ready. 

Now work on your master scripts. We are convinced that the business logic of your solution should be enclosed in packages provided by the solution. Master scripts are required for "glue logic" like reading configuration parameters, setup connections and realize data flow between levels of solution functionality stack.

After all you can prepare your solution for deployment:

``` bash
RSuite::prj_zip()
```

This will create zip bundle containing you isolated project environment together with your master scripts and configuration template. Just extract it on your production and run your master scripts (no package installation will be required in the production environment).

For more information check R Suite's [official webpage](https://rsuite.io)!


# How to install it
Just run 

``` bash
install.packages('RSuite')
```
