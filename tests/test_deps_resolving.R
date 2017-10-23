#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

context("Testing if project packages are built regarding dependencies")

test_that_managed("Installs dependency sequence TestPackage1 imports TestPackage2", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage1", prj, imps = "logging, TestPackage2")
  set_test_package_ns_imports("TestPackage1", prj, c("logging", "TestPackage2"))
  create_test_package("TestPackage2", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)

  RSuite::prj_build(prj)

  expect_that_packages_installed(c("TestPackage1", "TestPackage2", "logging"), prj)
})


test_that_managed("Installs dependency sequence src -> bin -> src -> Package", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  bin_type <- ifelse(.Platform$pkgType != "source", .Platform$pkgType, "binary")

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestPackage1", prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestPackage2", prj, type = bin_type, deps = "TestPackage1")
  create_package_deploy_to_lrepo(name = "TestPackage3", prj, type = "source", deps = "TestPackage2")
  create_test_package("TestPackage4", prj, deps = "TestPackage3")

  RSuite::prj_install_deps(prj, clean = T)

  expect_that_packages_installed(c("logging", "TestPackage1", "TestPackage2", "TestPackage3"), prj)

  RSuite::prj_build(prj)

  expect_that_packages_installed(c("logging", "TestPackage1", "TestPackage2", "TestPackage3", "TestPackage4"), prj)
})

