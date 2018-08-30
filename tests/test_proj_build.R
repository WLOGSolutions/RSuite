#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if project building works properly [test_prj_build]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")


test_that_managed("Handling project with package without NAMESPACE", {
  prj <- init_test_project(repo_adapters = c("Dir"))  # uses BaseTestProjectTemplate with logging 0.7-103
  params <- prj$load_params()

  create_test_package("TestPackage", prj)

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("logging"), prj)

  # build source types
  RSuite::prj_build(prj, type = "source")
  expect_that_packages_installed(c("logging", "TestPackage"), prj)

  # build binary types
  RSuite::prj_build(prj, type = params$bin_pkgs_type)
  expect_that_packages_installed(c("logging", "TestPackage"), prj)

  # TestPackage of source type was not removed
  expect_that_packages_available("TestPackage", "source", get_intrepo_manager(prj))
})
