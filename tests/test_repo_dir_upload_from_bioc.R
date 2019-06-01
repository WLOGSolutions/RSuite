#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if uploading Bioconductor packages into directory works properly [test_repo_dir_upload_from_bioc]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")


test_that_managed("Uploading Bioc (basic)", {
  skip_if_not(RSuite:::current_rver() == "3.6") # BiocGenerics needs R3.6 and remotes does not properly handles Bioc branches

  prj <- init_test_project(repo_adapters = c("CRAN"))
  mgr <- init_test_manager(prj)
  RSuite::prj_config_set_repo_adapters(c("CRAN", sprintf("Dir[%s]", mgr$path)), prj = prj)

  RSuite::repo_upload_bioc_package(mgr$repo_mgr, repo = "BiocGenerics", prj = prj, pkg_type = "source")
  expect_that_packages_available("BiocGenerics", "source", mgr)
})
