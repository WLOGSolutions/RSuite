#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/template_managment.R")
source("R/project_management.R")

context("Testing if project/package templates API functions work properly")

test_that_managed("Project template creation in user local path", {
  # create project template
  create_prj_test_template(name = "TestTemplate")

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



test_that_managed("Project template creation in specified path", {
  # create project template
  create_prj_test_template(name = "TestTemplate", path = get_wspace_dir())

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



test_that_managed("Package template creation in local user default path", {
  # create package template
  create_pkg_test_template(name = "TestTemplate")

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



test_that_managed("Package template creation in specified path", {
  # create package template
  create_pkg_test_template(name = "TestTemplate", path = get_wspace_dir())

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


test_that_managed("Template getting",{
  # create templates
  create_pkg_test_template(name = "TestTemplateOnlyPackage")
  create_prj_test_template(name = "TestTemplateOnlyProject")
  
  expected_templates <- data.frame(
    Name = c("TestTemplateOnlyPackage",
             "TestTemplateOnlyProject"),
    Type = c("package",
             "project")
  )
  
  expect_templates(expected_templates)
})


test_that_managed("Template registering", {
  # create templates
  wspace_dir <- get_wspace_dir()
  create_pkg_test_template(name = "TestTemplateOnlyPackage", path = wspace_dir)
  create_prj_test_template(name = "TestTemplateOnlyProject", path = wspace_dir)
  
  RSuite::rsuite_register_template(path = file.path(wspace_dir, "TestTemplateOnlyPackage"))
  RSuite::rsuite_register_template(path = file.path(wspace_dir, "TestTemplateOnlyProject"))
  
  expected_templates <- data.frame(
    Name = c("TestTemplateOnlyPackage",
             "TestTemplateOnlyProject"),
    Type = c("package",
             "project")
  )
  
  expect_templates(expected_templates)
})