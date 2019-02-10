# R Suite and Docker
In this document basic R Suite Docker functionalities are presented. It covers:

* creating deployment zips 
* creating Docker images
* Dockerfile templates

**Important** This tutorial was tested with R Suite version 0.30-241.

## Got stuck? ##

If you are stuck feel free to contact us:

through the R Suite website (https://rsuite.io#contact) or
using Gitter R Suite room
directly by sending an email with the description of your problem to rsuite@wlogsolutions.com.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

## **Table of Contents** ##

- [Motivation](#motivation)
- [Project preparation](#project-preparation)
- [Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)](#detect-proper-scriptpath-you-cannot-use-args-yet-as-they-are-build-with-tools-in-setenvr)
- [Creating a deployment zip for Ubuntu](#creating-a-deployment-zip-for-ubuntu)
- [Creating a Docker image containing our R solution](#creating-a-docker-image-containing-our-r-solution)
    - [R Suite Dockerfile templates](#r-suite-dockerfile-templates)

<!-- markdown-toc end -->

# Motivation
The goal of Docker is to separate the application from the infrastructure. It allows developers to deploy their applications on different platforms without worrying about hidden dependencies and system settings. But is it the ultimate solution for software developed in R?

Software written in R is based on R packages. That kind of structure leads to problems regarding dependencies which can't be always resolved using Docker. For example, when installing RStudio Addins or RMarkdown features some packages might install dependencies which will conflict with your currently developed software.

![Shiny deps](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/shiny_deps.png)

Additionally, your solution might require:

* isolation of the application from system R packages
* running multiple apps with different dependencies
* isolation of the R version from system packages

R Suite aims to resolve those issues by isolating project packages from global packages. Therefore, by combining R Suite and Docker together, we have created a tool that makes creating containerized R applications simple and convenient. Currently, R Suite provides solutions for the following scenarios:

* deployment to a production environment not supporting Docker containers
* deployment to a production environment supporting Docker containers  

No Docker knowledge is necessary in basic scenarios. However, if desired, R Suite provides Dockerfile templates which can be used for creating more advanced Docker images.

# Project preparation

Before proceeding to the tutorial, it is necessary to create an R Suite project. Let's create a simply Shiny app using R Suite.

```bash
rsuite proj start -n my_project
```

Next, let's change our working directory to the project directory and add a package to our project

```bash
cd my_project
rsuite proj pkgadd -n mypackage
```

Now let's modify our master script in order to create a Shiny app.

```r
# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- dirname(sub("--file=", "", args[grep("--file=", args)]))
  if (!length(script_path)) {
    return("R")
  }
  return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, "set_env.R"), chdir = T)

config <- load_config()
args <- args_parser()

library(shiny)
runExample(example = "02_text", port = 8888, host = "0.0.0.0")
``` 

More information about project management is presented in the [Basic R Suite usage](https://rsuite.io/RSuite_Tutorial.php?article=basic_workflow.md#) document.

**Important:** From now on Docker has to be running when issuing R Suite commands.

# Creating a deployment zip for Ubuntu
R Suite allows you to build a deployment package for a specific operating system. By using a Docker container running in the background R Suite prepares a deployment zip for the production environment.

![Deployment Scenario 1](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/deployment_scenario1.PNG)

The following command can be used to create a deployment zip for a production environment running on Ubuntu:

```bash
rsuite docker zip -p ubuntu 
``` 
You should see the following output:
```
2018-07-19 10:48:28 INFO::Retrieving RSuite CLI exposed on S3 package index ...
2018-07-19 10:48:29 INFO::... found v0.30.241 (debian/rsuitecli_0.30.241-1_all.deb)
2018-07-19 10:48:29 INFO::Starting container rsbuild-3dd44b2f34a2 based on wlog/rsuite:ubuntu_r3.4_v0.30.241 image ...
2018-07-19 10:48:30 INFO::... done.
2018-07-19 10:48:30 INFO::RSuite build container rsbuild-3dd44b2f34a2 started ...
2018-07-19 10:48:31 WARNING:rsuite:Project environment is not locked!
2018-07-19 10:48:31 INFO:rsuite:Exporting project from C:\Users\SZYMAN~1\DOCUME~1\DOCKER~1\RSuite\MY_PRO~1 ...
2018-07-19 10:48:31 INFO:rsuite:Package R version set to 3.4
2018-07-19 10:48:31 INFO:rsuite:Exporting project from C:\Users\SZYMAN~1\DOCUME~1\DOCKER~1\RSuite\MY_PRO~1 ... done
2018-07-19 10:48:32 INFO::Copying project pack into container rsbuild-3dd44b2f34a2 ...
2018-07-19 10:48:32 INFO::... done.
2018-07-19 10:48:33 INFO::Running command 'unzip prjpack_my_project_0.1x.zip' in rsbuild-3dd44b2f34a2 container ...
2018-07-19 10:48:33 INFO::... done.
2018-07-19 10:48:33 INFO::Running command 'cd my_project && rsuite proj depsinst -v' in rsbuild-3dd44b2f34a2 container ...
2018-07-19 10:52:58 INFO::... done.
2018-07-19 10:52:58 INFO::Running command 'cd my_project && rsuite proj depsclean -v' in rsbuild-3dd44b2f34a2 container ...
2018-07-19 10:53:01 INFO::... done.
2018-07-19 10:53:01 INFO::Running command 'cd my_project && rsuite proj zip -v -p /opt' in rsbuild-3dd44b2f34a2 container ...
2018-07-19 10:53:21 INFO::... done.
2018-07-19 10:53:21 INFO::Copying project deployment zip back from container rsbuild-3dd44b2f34a2 ...
2018-07-19 10:53:22 INFO::... done.
2018-07-19 10:53:22 INFO::Removing container rsbuild-3dd44b2f34a2 ...
2018-07-19 10:53:24 INFO::... done.
```

You have created the `my_project_1.0x.zip` file that contains all information necessary to run your solution on the example production environment. It's a binary version of your package that can be run only under Ubuntu and does not require Docker on the production environment. 

The zip version can be enforced by using the `--version` option. The specified version should have a DD.DD format. By default, the zip version is determined based on the PARAMETERS file while the revision version is taken from Revision Control.

# Creating a Docker image containing our R solution
R Suite provides the user with the possibility of creating Docker images which contain the production version of your project. It allows convenient development and deployment using Docker containers altogether with Docker Swarms and Kubernetes.

![Deployment scenario 2](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/deployment_scenario2.PNG)

Let's assume that we need to create a Docker image containing our project for the Ubuntu platform. Additionally, we want to tag the image in order to provide version control. It can be achieved by using the following command:

```bash
rsuite docker img -t my_image:1.0 -p ubuntu
```

The output should look like this:
```
2018-07-19 10:58:18 INFO::Will use wlog/rsuite:ubuntu_r3.4 as base image!
2018-07-19 10:58:18 INFO::Retrieving RSuite CLI exposed on S3 package index ...
2018-07-19 10:58:18 INFO::... found v0.30.241 (debian/rsuitecli_0.30.241-1_all.deb)
2018-07-19 10:58:18 INFO::Starting container rsbuild-2c50211a6682 based on wlog/rsuite:ubuntu_r3.4_v0.30.241 image ...
2018-07-19 10:58:20 INFO::... done.
2018-07-19 10:58:20 INFO::RSuite build container rsbuild-2c50211a6682 started ...
2018-07-19 10:58:21 WARNING:rsuite:Project environment is not locked!
2018-07-19 10:58:21 INFO:rsuite:Exporting project from C:\Users\SZYMAN~1\DOCUME~1\DOCKER~1\RSuite\MY_PRO~1 ...
2018-07-19 10:58:21 INFO:rsuite:Package R version set to 3.4
2018-07-19 10:58:21 INFO:rsuite:Exporting project from C:\Users\SZYMAN~1\DOCUME~1\DOCKER~1\RSuite\MY_PRO~1 ... done
2018-07-19 10:58:24 INFO::Copying project pack into container rsbuild-2c50211a6682 ...
2018-07-19 10:58:25 INFO::... done.
2018-07-19 10:58:25 INFO::Running command 'unzip prjpack_my_project_0.1x.zip' in rsbuild-2c50211a6682 container ...
2018-07-19 10:58:26 INFO::... done.
2018-07-19 10:58:26 INFO::Running command 'cd my_project && rsuite proj depsinst -v' in rsbuild-2c50211a6682 container ...
2018-07-19 11:02:57 INFO::... done.
2018-07-19 11:02:57 INFO::Running command 'cd my_project && rsuite proj depsclean -v' in rsbuild-2c50211a6682 container ...
2018-07-19 11:03:00 INFO::... done.
2018-07-19 11:03:00 INFO::Running command 'cd my_project && rsuite proj zip -v -p /opt' in rsbuild-2c50211a6682 container ...
2018-07-19 11:03:20 INFO::... done.
2018-07-19 11:03:20 INFO::Copying project deployment zip back from container rsbuild-2c50211a6682 ...
2018-07-19 11:03:21 INFO::... done.
2018-07-19 11:03:21 INFO::Removing container rsbuild-2c50211a6682 ...
2018-07-19 11:03:22 INFO::... done.
2018-07-19 11:03:23 INFO::Building image my_image:1.0 ...
2018-07-19 11:03:35 INFO::... done.
```

Your image `my_image:1.0` should be present when listing Docker images:

```bash
docker images
REPOSITORY            TAG                     IMAGE ID            CREATED             SIZE
my_image              1.0                     2c19f30e41d5        4 minutes ago       1.31GB 
```

Now let's run the created image and start the shiny app
```bash
docker run --name my_container -p 8888:8888 -d --rm my_image:1.0 Rscript my_project/R/master.R
```

The shiny app should be available at http://localhost:8888

![Shiny running](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/shiny_running.PNG)

To stop the container issue the following command
```bash
docker stop my_container
```

## R Suite Dockerfile templates
More complex Docker images can be created by using R Suite's Dockerfile templates. Those are regular Dockerfiles with specific tags which will be automatically replaced by R Suite generated commands. The following tags are supported:

* `<RSuite:From>` will be replaced with the base image name
* `<RSuite:DeployProject>` will be replaced with commands for deploying our project from the deployment zip package

The default Dockerfile template contains the following content:
```
FROM <RSuite:From>
<RSuite:DeployProject>
```

Dockerfile templates provide a convenient way of creating advanced Docker images containing solutions written in R. We are going to create a Docker image that will automatically start our shiny app when running a Docker containr. Let's create a file called `my_template` with the following content
```
FROM <RSuite:From>
<RSuite:DeployProject>
EXPOSE 8888
CMD ["Rscript", "/opt/my_project/R/master.R"]
``` 

To create an image using the freshly created template, execute the following command
```bash
rsuite docker img -t my_image:2.0 --templ <path/to/my_template/file>
```

Now let's run the created image 
```bash
docker run --name my_container -p 8888:8888 -d --rm my_image:2.0
```

The shiny app should start automatically http://localhost:8888

![Shiny running](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/shiny_running.PNG)

To stop the container, issue the following command:
```bash
docker stop my_container
```

R Suite Dockerfile templates are a powerful tool for creating complex Docker images containing reproducible software written in R. R Suite takes care of all dependency related work while we can take care of the remaining configuration.
