#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if uploading GitHub packages into directory works properly")

test_that_managed("Uploading GitHub (basic)", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  mgr <- init_test_manager(prj)
  RSuite::prj_config_set_repo_adapters(c("CRAN", sprintf("Dir[%s]", mgr$path)), prj = prj)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/rAzureBatch", prj = prj, pkg_type = "source")
  expect_that_packages_available("rAzureBatch", "source", mgr)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/doAzureParallel", prj = prj, pkg_type = "source")
  expect_that_packages_available(c("rAzureBatch", "doAzureParallel"), "source", mgr)
})

test_that_managed("Uploading GitHub (with deps)", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  mgr <- init_test_manager(prj)
  RSuite::prj_config_set_repo_adapters(c("CRAN", sprintf("Dir[%s]", mgr$path)), prj = prj)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/rAzureBatch", prj = prj, pkg_type = "source",
                                     with_deps = T)
  expect_that_packages_available(c("rAzureBatch",
                                   "bitops", "curl", "digest", "httr", "jsonlite", "mime",
                                   "openssl", "R6", "RCurl", "rjson"),
                                 "source", mgr)
})
