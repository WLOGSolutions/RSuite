#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/template_managment.R")
source("R/project_management.R")

context("Testing if project/templates work properly")


test_that_managed("Project creation from builtin template", {
  # create test project using the builtin template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin")
  params <- prj$load_params()

  # create vector containing all files from the builtin project template
  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "deployment/libs",
    "deployment/sbox",
    "logs",
    "TestProject.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/TestProject_Tests.Rproj",
    "tests/.Rprofile"
  )

  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% prj_files))
})


test_that_managed("Package creation from builtin template", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin")
  params <- prj$load_params()

  # create test package using the builtin template
  create_test_package("TestPackage", prj = prj, tmpl = "builtin")

  # create vector containing all files from the builtin package template
  expected_files <- c(
    "TestPackage.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "data",
    "inst",
    "man",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R",
    "tests"
  )

  # retrieve all files from the created package
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"),
                          all.files = TRUE, recursive = TRUE,
                          include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% pkg_files))
})


test_that_managed("Project template creation", {
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
    "logs",
    "__ProjectName__.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/__ProjectName___Tests.Rproj",
    "tests/.Rprofile"
  )

  expect_true(all(expected_files %in% tmpl_files))
})


test_that_managed("Package template creation", {
  # create project template
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
    "data",
    "inst",
    "man",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R",
    "tests"
  )

  expect_true(all(expected_files %in% tmpl_files))
})


test_that_managed("Project creation using custom template", {
  # create project template not
  create_prj_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "project")

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "__ProjectName__.txt"))

  # create project using custom template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = tmpl_path)

  params <- prj$load_params()

  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "deployment/libs",
    "deployment/sbox",
    "logs",
    "TestProject.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/TestProject_Tests.Rproj",
    "tests/.Rprofile",
    "TestProject.txt"
  )

  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% prj_files))
})


test_that_managed("Package creation using custom template", {
  # create project template not
  create_pkg_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "__PackageName__.txt"))

  # create package using custom template
  prj <- init_test_project(repo_adapters = c("Dir"))
  create_test_package("TestPackage", prj = prj, tmpl = tmpl_path)


  expected_files <- c(
    "TestPackage.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "data",
    "inst",
    "man",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R",
    "tests",
    "TestPackage.txt"
  )

  # retrieve all files from the created package
  params <- prj$load_params()
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"),
                          all.files = TRUE, recursive = TRUE,
                          include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% pkg_files))
})



test_that_managed("Project creation using template
                  not containing the PARAMETERS file", {
  # create project template
  create_prj_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "project")
  unlink(file.path(tmpl_path, "PARAMETERS"))

  expect_error(init_test_project(name = "TestProject", tmpl = tmpl_path),
               regexp = "does not satisfy project template requirements")
})


test_that_managed("Package creation using template
                  not containing the NAMESPACE file", {
  # create package template
  create_pkg_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  unlink(file.path(tmpl_path, "NAMESPACE"))

  prj <- init_test_project(name = "TestProject")

  expect_error(create_test_package(name = "TestPackage",
                                   prj = prj, tmpl = tmpl_path),
               regexp = "does not satisfy package template requirements")
})


test_that_managed("Package creation using template
                  not containing the DESCRIPTION file", {
  # create package template
  create_pkg_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  unlink(file.path(tmpl_path, "DESCRIPTION"))

  prj <- init_test_project(name = "TestProject")

  expect_error(create_test_package(name = "TestPackage",
                                   prj = prj, tmpl = tmpl_path),
               regexp = "does not satisfy package template requirements")
})


test_that_managed("Package creation using template
                  not containing the NEWS file", {
  # create package template
  create_pkg_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  unlink(file.path(tmpl_path, "NEWS"))

  prj <- init_test_project(name = "TestProject")

  expect_error(create_test_package(name = "TestPackage",
                                   prj = prj, tmpl = tmpl_path),
               regexp = "does not satisfy package template requirements")
})