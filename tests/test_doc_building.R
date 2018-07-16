#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

context("Testing if project packages documentation builds properly")

test_that_managed("Simple documentation creation", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage1", prj)

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("logging"), prj)

  RSuite::prj_build(prj)
  expect_that_packages_installed(c("TestPackage1", "logging"), prj)

  expect_that_has_docs(c("assert"), "TestPackage1", prj)
})

test_that_managed("Documentation creation for package with depends", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestPackage1", prj, type = "source")
  create_test_package("TestPackage2", prj, deps = "TestPackage1")

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestPackage1", "logging"), prj)

  RSuite::prj_build(prj)
  expect_that_packages_installed(c("TestPackage2", "TestPackage1", "logging"), prj)

  expect_that_has_docs(c("assert"), "TestPackage2", prj)
})
