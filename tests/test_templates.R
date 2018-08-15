#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if project/package templates API functions work properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/template_managment.R")
source("R/project_management.R")


test_that_template("Project template creation in user local path", {
  # create project template
  create_test_template(name = "TestTemplate", path = get_wspace_template_dir(), add_prj = TRUE)

  # check if template was created
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "project")
  expect_true(dir.exists(tmpl_path))

  # check if template contains files from default template
  tmpl_files <- list.files(tmpl_path, all.files = TRUE, recursive = TRUE,
                           include.dirs = TRUE, no.. = TRUE)
  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "__ProjectName__.Rproj",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/__ProjectName___Tests.Rproj",
    "tests/.Rprofile"
  )
})



test_that_template("Project template creation in specified path", {
  # create project template
  create_test_template(name = "TestTemplate", path = get_wspace_dir(), add_prj = TRUE)

  # check if template was created
  tmpl_path <- file.path(get_wspace_dir(), "TestTemplate", "project")
  expect_true(dir.exists(tmpl_path))

  # check if template contains files from default template
  tmpl_files <- list.files(tmpl_path, all.files = TRUE, recursive = TRUE,
                           include.dirs = TRUE, no.. = TRUE)
  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "__ProjectName__.Rproj",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/__ProjectName___Tests.Rproj",
    "tests/.Rprofile"
  )

  expect_true(all(expected_files %in% tmpl_files))
})



test_that_template("Package template creation in local user default path", {
  # create package template
  create_test_template(name = "TestTemplate", path = get_wspace_template_dir(), add_pkg = TRUE)

  # check if template was created
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  expect_true(dir.exists(tmpl_path))

  # check if template contains files from default template
  tmpl_files <- list.files(tmpl_path, all.files = TRUE, recursive = TRUE,
                           include.dirs = TRUE, no.. = TRUE)
  expected_files <- c(
    "__PackageName__.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R"
  )

  expect_true(all(expected_files %in% tmpl_files))
})



test_that_template("Package template creation in specified path", {
  # create package template
  create_test_template(name = "TestTemplate", path = get_wspace_dir(), add_pkg = TRUE)

  # check if template was created
  tmpl_path <- file.path(get_wspace_dir(), "TestTemplate", "package")
  expect_true(dir.exists(tmpl_path))

  # check if template contains files from default template
  tmpl_files <- list.files(tmpl_path, all.files = TRUE, recursive = TRUE,
                           include.dirs = TRUE, no.. = TRUE)
  expected_files <- c(
    "__PackageName__.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R"
  )

  expect_true(all(expected_files %in% tmpl_files))
})


test_that_template("Template getting", {
  # create templates
  create_test_template(name = "TestTemplateOnlyPackage", path = get_wspace_template_dir(), add_pkg = TRUE)
  create_test_template(name = "TestTemplateOnlyProject", path = get_wspace_template_dir(), add_prj = TRUE)

  expected_templates <- data.frame(
    Name = c("TestTemplateOnlyPackage",
             "TestTemplateOnlyProject"),
    HasProjectTemplate = c(FALSE, TRUE),
    HasPackageTemplate = c(TRUE, FALSE)
  )

  expect_templates(expected_templates)
})


test_that_template("Template registering", {
  # create templates
  wspace_dir <- get_wspace_dir()
  create_test_template(name = "TestTemplateOnlyPackage", path = wspace_dir, add_pkg = TRUE)
  create_test_template(name = "TestTemplateOnlyProject", path = wspace_dir, add_prj = TRUE)

  RSuite::tmpl_register(path = file.path(wspace_dir, "TestTemplateOnlyPackage"))
  RSuite::tmpl_register(path = file.path(wspace_dir, "TestTemplateOnlyProject"))

  expected_templates <- data.frame(
    Name = c("TestTemplateOnlyPackage",
             "TestTemplateOnlyProject"),
    HasProjectTemplate = c(FALSE, TRUE),
    HasPackageTemplate = c(TRUE, FALSE)
  )

  expect_templates(expected_templates)
})


test_that_template("Template register using current directory", {
  # create templates
  wspace_dir <- get_wspace_dir()
  create_test_template("TestTemplate", path = wspace_dir, add_prj = TRUE,
                       add_pkg = TRUE)

  oldwd <- getwd()
  setwd(file.path(wspace_dir, "TestTemplate"))
  on_test_exit(function() {
    setwd(oldwd)
  })

  RSuite::tmpl_register(path = ".")

  expected_templates <- data.frame(
    Name = c("TestTemplate"),
    HasProjectTemplate = c(TRUE),
    HasPackageTemplate = c(TRUE)
  )

  expect_templates(expected_templates)
})


test_that_template("Template register using previous directory", {
  # create templates
  wspace_dir <- get_wspace_dir()
  create_test_template(name = "TestTemplate", path = wspace_dir, add_prj = TRUE, add_pkg = TRUE)

  oldwd <- getwd()
  setwd(file.path(wspace_dir, "TestTemplate", "project"))
  on_test_exit(function() {
    setwd(oldwd)
  })

  RSuite::tmpl_register(path = "..")

  expected_templates <- data.frame(
    Name = c("TestTemplate"),
    HasProjectTemplate = c(TRUE),
    HasPackageTemplate = c(TRUE)
  )

  expect_templates(expected_templates)
})

test_that_template("Template register overwriting", {
  # create templates
  wspace_dir <- get_wspace_dir()
  create_test_template(name = "TestTemplate", path = wspace_dir, add_prj = TRUE, add_pkg = TRUE)

  test_template_path <- file.path(wspace_dir, "TestTemplate")
  RSuite::tmpl_register(path = test_template_path)
  expect_log_message(RSuite::tmpl_register(path = test_template_path),
                     "Overwriting existing template: TestTemplate")

  expected_templates <- data.frame(
    Name = c("TestTemplate"),
    HasProjectTemplate = c(TRUE),
    HasPackageTemplate = c(TRUE)
  )

  expect_templates(expected_templates)
})
