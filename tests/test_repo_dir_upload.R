#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if uploading into directory works properly")

test_that_managed("Uploading project packages", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")
  RSuite::prj_install_deps(prj)

  RSuite::repo_upload_prj_packages(mgr$repo_mgr, prj = prj, skip_rc = T)

  expect_that_packages_available("TestPackage", .Platform$pkgType, mgr)
})

test_that_managed("Uploading file packages", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  RSuite::repo_upload_package_files(mgr$repo_mgr, files = file.path("data", "logging_0.7-103.tar.gz"))

  expect_that_packages_available("logging", "source", mgr)
})

test_that_managed("Uploading external packages", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  RSuite::repo_upload_ext_packages(mgr$repo_mgr, pkgs = "logging", prj = prj, pkg_type = "source")

  expect_that_packages_available("logging", "source", mgr)
})

test_that_managed("Uploading PKGZIP", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  wspace_dir <- get_wspace_dir()
  pkgzip <- file.path(wspace_dir, sprintf("%s_pkgzip_logging.zip", Sys.Date()))
  on_test_exit(function() {
    unlink(pkgzip, force = T)
  })
  
  RSuite::pkgzip_build_package_files(files = file.path("data", "logging_0.7-103.tar.gz"),
                                     path = wspace_dir)
  RSuite::repo_upload_pkgzip(mgr$repo_mgr, pkgzip = pkgzip)

  expect_that_packages_available("logging", "source", mgr)
})

test_that_managed("Uploading GitHub", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  mgr <- init_test_manager(prj)
  RSuite::prj_config_set_repo_adapters(c("CRAN", sprintf("Dir[%s]", mgr$path)), prj = prj)
  
  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/rAzureBatch", prj = prj, pkg_type = "source")
  expect_that_packages_available("rAzureBatch", "source", mgr)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/doAzureParallel", prj = prj, pkg_type = "source")
  expect_that_packages_available(c("rAzureBatch", "doAzureParallel"), "source", mgr)
})
