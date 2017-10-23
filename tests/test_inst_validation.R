#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

context("Testing if package install validations works properly")

test_that_managed("Post-install R version check works", {
  skip_if_not(.Platform$OS.type != "source")

  prj <- init_test_project(repo_adapters = c("Dir", "MRAN[2017-01-08]"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  prj_config_set_rversion("3.4", prj = prj)
  create_test_master_script(prj = prj, code = "library(colorspace)")

  expect_error(prj_install_deps(prj = prj),
               "Failed to install some dependencies: colorspace")

  # colorspace is not installed because it was built for R 3.3.2
  expect_that_packages_installed(c("logging"), prj)
})

test_that_managed("Post building docs imports in NAMESPACE shoud get fixed", {
  prj <- init_test_project(repo_adapters = "Dir")
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestPackage1", prj = prj)

  create_test_package("TestPackage2", prj, imps = "logging, TestPackage1")

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("logging", "TestPackage1"), prj)

  prj_build(prj = prj)
  expect_that_packages_installed(c("logging", "TestPackage1", "TestPackage2"), prj)
})

test_that_managed("Post building docs declared imports should confirm to NAMESPACE", {
  prj <- init_test_project(repo_adapters = "Dir")
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  create_test_package("TestPackage1", prj)
  set_test_package_ns_imports("TestPackage1", prj, c("logging", "TestPackage"))

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("logging"), prj) # dependency to TestPackage1 is detected base on DESCRIPTION

  expect_error(prj_build(prj = prj), "Failed to install .*: TestPackage1")
})
