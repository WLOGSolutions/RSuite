#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if uploading into directory works properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")


test_that_managed("Uploading file packages", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  mgr <- init_test_manager(prj)

  RSuite::repo_upload_package_files(mgr$repo_mgr, files = file.path("data", "logging_0.7-103.tar.gz"))

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
