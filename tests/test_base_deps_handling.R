#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if base dependencies are handled properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Instaling package which requires methods", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})

test_that_managed("Build deps which contains grid", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_master_script(code = "library(grid)", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})
