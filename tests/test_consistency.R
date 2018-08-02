#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if prj & zip building consistency is controlled")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Basic building consistency is controlled", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  expect_error(RSuite::prj_build(prj), "logging.*prj_install_deps")
})
