# R Suite templates
This document describes examples of R Suite project and package templates.

The following scenarios are presented:

* Specific repository configuration
* Shiny apps development
* R Markdown configuration

**Important:** This tutorial was tested with R Suite version 0.32-244

## **Got stuck?**

If you are stuck feel free to contact us:

* through the R Suite website (https://rsuite.io#contact) or 
* using Gitter [R Suite room](https://gitter.im/WLOGSolutions/RSuite
"Gitter R Suite room")
* directly by sending an email with the description of your problem to
  [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).
  
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

## **Table of Contents** ##

- [Motivation](#motivation)
    - [**Default project template:**](#default-project-template)
    - [**Default package template:**](#default-package-template)
- [Project repo setup](#project-repo-setup)
- [Shiny app development](#shiny-app-development)
- [__PackageName__](#packagename)
- [Global package definitions and imports](#global-package-definitions-and-imports)
- [Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)](#detect-proper-scriptpath-you-cannot-use-args-yet-as-they-are-build-with-tools-in-setenvr)
- [Setting .libPaths() to point to libs folder](#setting-libpaths-to-point-to-libs-folder)
- [R Markdown configuration](#r-markdown-configuration)
- [Sample dataframe](#sample-dataframe)
- [Dataset](#dataset)
- [Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)](#detect-proper-scriptpath-you-cannot-use-args-yet-as-they-are-build-with-tools-in-setenvr-1)
- [Setting .libPaths() to point to libs folder](#setting-libpaths-to-point-to-libs-folder-1)
- [Summary](#summary)

<!-- markdown-toc end -->


# Motivation
The R programming language is a tool used in a large number of different fields and environments. In each of those, users have specific demands. In order to fulfil those needs, R Suite offers the possibility of defining custom project and package templates. It is important to stress that this is an advanced mechanism, which used improperly might result in errors when using other R Suite functionalities. The default built-in templates offered by R Suite have the following file structure:

#### **Default project template:** 
  <pre>
  │   .Rprofile
  │   config_templ.txt
  │   PARAMETERS
  │   __ProjectName__.Rproj
  │   __rc_ignore
  │
  ├───deployment
  │   │   __rc_ignore
  │   │
  │   ├───libs
  │   │       __rc_ignore
  │   │
  │   └───sbox
  │           __rc_ignore
  │
  ├───logs
  │       __rc_ignore
  │
  ├───packages
  │       __rc_ignore
  │
  ├───R
  │       .Rprofile
  │       master.R
  │       set_env.R
  │
  └───tests
          .Rprofile
          __ProjectName___Tests.Rproj
          __rc_ignore
  </pre>

#### **Default package template:**
  <pre>
  │   .Rbuildignore
  │   .Rprofile
  │   DESCRIPTION
  │   NAMESPACE
  │   NEWS
  │   __PackageName__.Rproj
  │   __rc_ignore
  │
  ├───man
  │       __rc_ignore
  │
  └───R
          packages_import.R
          package_logger.R
          package_validation.R
  </pre>
  
Users have the freedom of adding their own sets of directories or they can modify the content of already existing files according to their needs. In order to make templates even more customisable R Suite introduces markers - special keywords which will later be replaced when creating projects/packages from custom templates. The full list of keywords is presented below:

* `__ProjectName__` - will be replaced with the name of the project
* `__PackageName__` - will be replaced with the name of the package
* `__RSuiteVersion__` - will be replaced with the used R Suite version
* `__RVersion__` - will be replaced with the used R version
* `__Date__` - will be replaced with the current date
* `__User__` - will be replaced with the username
* `__LatestMRAN__` - will be replaced with `MRAN[<Date of latest working MRAN snapshot from the last 2 weeks>]`

Those markers can be used in filenames or directly in the files content.

Users can make use of these customisation options depending on their needs. For example, a Shiny developer might have a favourite set of CSS styles and JavaScript functions or perhaps you are working in an environment in which all projects use a specific package repository. The development process in both of those cases can be enhanced by the use of R Suite templates. In the following sections, the procedure of incorporating R Suite templates in the above-mentioned scenarios is going to be presented.

Before proceeding to the tutorial, let's look at the basic template commands that we are going to use

* `rsuite tmpl start -n my_template` - creates a template called `my_template` (creates both project and package templates)
* `rsuite tmpl register -p path_to_template` - registers a template found under the `path_to_template` path
* `rsuite tmpl list` - lists all available registered templates

For a more detailed description of the template commands please consider reading the `Template management` section of  the [R Suite CLI reference manual](https://rsuite.io/RSuite_Tutorial.php?article=rsuite_cli_reference.md) document.

# Project repo setup
R Suite allows you to specify which package repository will be used in your project. By default, R Suite projects use MRAN as it provides the best reproducibility features. However, in some scenarios, it might be necessary to use a different repository like for example using a local repository when working in an internet-less environment.

In this tutorial, we are going to create a project template that will use CRAN as it's default package repository. We start by creating a default project template

```
rsuite tmpl start -n my_repo_template --prj
```

Next, we will change the `Repositories` field in the `my_repo_template/project/PARAMETERS` file:

<pre>
RSuiteVersion: __RSuiteVersion__
RVersion: __RVersion__
Project: __ProjectName__
Repositories: <font color="blue"><b>CRAN</b></font>
Artifacts: config_templ.txt
</pre>

Our template is ready to be registered
```
rsuite tmpl register -p my_repo_template
```

To test if our template is working properly, we will create a project from the freshly created template and install dependencies
```
rsuite proj start -n my_repo_project -t my_repo_template
cd my_repo_project
rsuite proj depsinst
```

The output should look like this

```
2018-08-08 09:16:47 INFO:rsuite:Detecting repositories (for R 3.5)...
2018-08-08 09:16:49 WARNING:rsuite:Project is configured to use non reliable repositories: CRAN. You should use only reliable repositories to be sure of project consistency over time.
2018-08-08 09:16:49 INFO:rsuite:Will look for dependencies in ...
2018-08-08 09:16:49 INFO:rsuite:.          CRAN#1 = https://cloud.r-project.org/ (win.binary, source)
2018-08-08 09:16:49 INFO:rsuite:Collecting project dependencies (for R 3.5)...
2018-08-08 09:16:50 INFO:rsuite:Resolving dependencies (for R 3.5)...
2018-08-08 09:16:50 INFO:rsuite:No dependencies to install.
```

As we can see, R Suite was looking for dependencies in the CRAN repository.
  
# Shiny app development
Shiny is a fantastic package used for creating web applications in R. It supports the use of CSS themes, HTML widgets and JavaScript functions. In order to speed up the development process of Shiny apps, we can create a package template containing our favourite set of CSS styles.

Let's start by generating a default package template in our working directory.
```
rsuite tmpl start -n my_shiny_template --pkg
```

This command creates a package template which has the same file structure as the default built-in package template. Now, we can start modifying our template. First, we will create the directory structure that will contain our Shiny app
```
mkdir my_shiny_template/package/inst/
mkdir my_shiny_template/package/inst/app
```

In the `app` directory, we are going to create our basic components:

* `app.R` with a minimalistic Shiny app:
  ```
  library(shiny)
  
  ui <- fluidPage(
    theme = "bootstrap.css",
    
    titlePanel(title = "Hello from the __PackageName__ application!")
  )
  
  
  server <- function(input, output) {
  }
  
  
  shinyApp(ui, server)
  ```
  
* `www` directory in which we are going to put our themes. I will be using the `slate` theme provided by bootswatch. Just download it from the [bootswatch github repository](https://raw.githubusercontent.com/thomaspark/bootswatch/v3.3.1/slate/bootstrap.css) and save it as `bootstrap.css` in the `www` directory. You can use your own favourite theme, but remember that Shiny uses the bootstrap framework in version `3.3.1`.




As we are using `Shiny` in our package, remember to add `Shiny` to the `Imports` fields in the `DESCRIPTION` file like below.

<pre>
Package: __PackageName__
Type: Package
Title: What the package does (short line)
Version: 0.1
Date: __Date__
Author: __User__
Maintainer: Who to complain to <yourfault@somewhere.net>
Description: More about what it does (maybe more than one line)
License: What license is it under?
Imports: logging, <font color="blue"><b>shiny</b></font>
</pre>

Don't forget to add `Shiny` to the `my_shiny_template/package/R/package_imports.R` file as well!
<pre>
#----------------------------------------------------------------------------
# __PackageName__
#
# Global package definitions and imports
#----------------------------------------------------------------------------

#' @import logging
<font color="blue"><b>#' @import shiny</b></font>
NULL

</pre>


If we want to make our app runnable from the master script, we have to create a new file `my_shiny_template/package/R/run_app.R` with the following content:

```
#'@export
run_app <- function(port = 8888, host = "127.0.0.1") {
  app_dir <- system.file("app", package = "__PackageName__")
  shiny::runApp(app_dir, port = port, host = host)
}
```

The final package template should have the following file structure:
<pre>
│   .Rbuildignore
│   .Rprofile
│   DESCRIPTION
│   NAMESPACE
│   NEWS
│   __PackageName__.Rproj
│   __rc_ignore
<font color="blue"><b>│
├───inst
│   └───app
│       │   app.R
│       │
│       └───www
│               bootstrap.css
│</b></font>
├───man
│       __rc_ignore
│
└───R
        packages_import.R
        package_logger.R
        package_validation.R
       <font color="blue"><b> run_app.R </b></font>
</pre>

In order to make our template as complete as possible, we are going to additionally prepare a project template.
```
rsuite tmpl start -n my_shiny_template --prj
```

Let's start by creating the `config_templ.txt` file in the `my_shiny_template/project` directory. It will contain the default app settings:
<pre>
LogLevel: INFO
<font color="blue"><b>port: 8888
host: 127.0.0.1</b></font>
</pre>

Finally, let's register our Shiny app template
```
rsuite tmpl register -p my_shiny_template
```

If everything worked properly, `my_shiny_template` should be listed when issuing the following command
```
rsuite tmpl list
```

Now, we are going to make use of our freshly registered template and create a simple Shiny application. We will begin by creating a new project.

```
rsuite proj start -n my_shiny_project -t my_shiny_template
```

Next, let's create a package based on `my_shiny_template`
```
cd my_shiny_project
rsuite proj pkgadd -n myShinyApp -t my_shiny_template
```

Now, we only need to add two lines to our master script `my_shiny_project/R/master.R` in order to run our application.

<pre>
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

<font color="blue"><b>library(myShinyApp)
run_app(port = as.numeric(config$port), host = config$host) </b></font>

</pre>

Finally, let's install dependencies, build the project and run our app

```
rsuite proj depsinst
rsuite proj build
Rscript R/master.R
```

If our Shiny app is running, it should be available at http://localhost:8888/.

![Shiny app](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/templates_shiny.PNG)

We have created a template for developing Shiny applications using our favourite CSS theme. It allows us to  keep the reproducibility and ready to deploy features that R Suite provides and we can focus solely on the application related development.

# R Markdown configuration
R Markdown is a wonderful tool for writing reports and preparing presentations. We will create a project template that will have a separate directory for reports and presentations. Additionally, we will define a common YAML configuration that will ensure that all of our files will be rendered using our custom settings.

We will start by creating a default project template
```
rsuite tmpl start -n my_rmarkdown_template --prj
```

Now, let's create a directory for our documents
```
mkdir my_rmarkdown_template/project/docs
```

Let's assume that we have a favourite powerpoint presentation template, in this example we will use the Austin template available at https://templates.office.com/en-us/Austin-TM00001203. Just save it as `Austin.pptx` in the `docs` directory.

Next, we will create a YAML file called `_output.yaml` (for more information visit https://rmarkdown.rstudio.com/markdown_document_format) with the following content:

```yaml
html_document:
  toc: true
  number_sections: true
  df_print: kable
  
powerpoint_presentation:
  toc: true
  slide_level: 1
  reference_doc: Austin.pptx
```

The configuration will allow us to render HTML documents and create powerpoint presentations using our preferred options. Additionally, we are going to add files containing our report and presentation. Let's create two files `__ProjectName__-report.Rmd` and `__ProjectName__-presentation.Rmd` in the `docs` directory.

Put the following content `__ProjectName__-report.Rmd`
<pre>
---
title: "__ProjectName__ - report"
author: __User__
date: __Date__
output:
  html_document
---

# Sample dataframe
```{r echo = FALSE}
head(iris, 5)
```
</pre>

whereas `__ProjectName__-presentation.Rmd` will contain the following
<pre>
---
title: "__ProjectName__ - presentation"
author: __User__
date: __Date__
output:
  powerpoint_presentation
---

# Dataset
```{r echo=FALSE}
knitr::kable(head(ToothGrowth, 7))
```
</pre>

Let's modify the master script (`R/master.R`) in order to speed up the rendering of our documents
<pre>
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

<font color="blue"><b>library(rmarkdown)

render("docs/__ProjectName__-report.Rmd", output_format = "html_document")
render("docs/__ProjectName__-presentation.Rmd", output_format = "powerpoint_presentation")</b></font>
</pre>

We should end up with the following file structure:
<pre>
│   .Rprofile
│   config_templ.txt
│   PARAMETERS
│   __ProjectName__.Rproj
│   __rc_ignore
│
├───deployment
│   │   __rc_ignore
│   │
│   ├───libs
│   │       __rc_ignore
│   │
│   └───sbox
│           __rc_ignore
<font color="blue"><b>│
├───docs
│       Austin.pptx
│       _output.yaml
│       __ProjectName__-presentation.Rmd
│       __ProjectName__-report.Rmd
│</b></font>
├───logs
│       __rc_ignore
│
├───packages
│       __rc_ignore
│
├───R
│       .Rprofile
│       master.R
│       set_env.R
│
└───tests
        .Rprofile
        __ProjectName___Tests.Rproj
        __rc_ignore
</pre>

Finally, we can register our template
```
rsuite tmpl register -p my_rmarkdown_template
```

Now, let's use our template and render our documents

```
rsuite proj start -n MyDocument -t my_rmarkdown_template
cd MyDocument
rsuite proj depsinst
Rscript R/master.R
```

![R Markdown example](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/templates_rmarkdown.PNG)

# Summary
R Suite templates provide a very flexible way of tailoring projects and packages to fit specific needs. By creating templates we can automate repetitive tasks that are associated with project and package configuration. The examples presented in this document are only a sample of potentially unlimited capabilities. However, it is important to remember that it is an advanced functionality that used improperly might cause unexpected errors when using other R Suite functionalities.

| Templates used in this tutorial | All available R Suite templates |
| :----------------------------:  | :-------------------------------------------------------------------------------------------: |
| [tutorial_templates.zip](http://wlog-rsuite.s3.amazonaws.com/templates/templates_tutorial.zip) | [templates.zip](http://wlog-rsuite.s3.amazonaws.com/templates/templates.zip)                    |

