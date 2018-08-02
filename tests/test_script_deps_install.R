#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if master script dependencies are handled properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Install with package requiring 'logging'", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "logging")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})
