#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if uploading external packages into directory works properly")

test_that_managed("Uploading external packages (basic)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  RSuite::repo_upload_ext_packages(mgr$repo_mgr, pkgs = "logging", prj = prj, pkg_type = "source")

  expect_that_packages_available("logging", "source", mgr)
})

test_that_managed("Uploading external packages (with deps)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo("TestPackage", prj, type = "source")

  RSuite::repo_upload_ext_packages(mgr$repo_mgr, pkgs = "TestPackage", prj = prj, pkg_type = "source",
                                   with_deps = TRUE)

  expect_that_packages_available(c("logging", "TestPackage"), "source", mgr)
})

test_that_managed("Uploading external packages (withr - building source cannot rebuild docs)", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  bin_type <- prj$load_params()$bin_pkgs_type

  mgr <- init_test_manager(prj)

  RSuite::repo_upload_ext_packages(mgr$repo_mgr, pkgs = "withr", prj = prj, pkg_type = bin_type)

  expect_that_packages_available("withr", bin_type, mgr)
})
