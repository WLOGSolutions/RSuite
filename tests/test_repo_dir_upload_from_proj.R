#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if uploading project packages into directory works properly")

test_that_managed("Uploading project packages (basic)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")
  RSuite::prj_install_deps(prj)

  RSuite::repo_upload_prj_packages(mgr$repo_mgr, prj = prj, skip_rc = T)

  expect_that_packages_available("TestPackage", .Platform$pkgType, mgr)
})

test_that_managed("Uploading project packages (with deps)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")
  RSuite::prj_install_deps(prj)

  RSuite::repo_upload_prj_packages(mgr$repo_mgr, prj = prj, skip_rc = T, with_deps = T)

  expect_that_packages_available(c("TestPackage", "logging"), .Platform$pkgType, mgr)
})

