#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if specific packages are installing properly (ROracle, H2O) [test_specific_pkgs_install]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/install_oracle_instantclient.R")
source("R/project_management.R")

installOracleInstantClient()


test_that_managed("Installing ROracle", {
  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))
  create_test_master_script("library(ROracle)", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("ROracle", "DBI", "logging"), prj)
})

test_that_managed("Installing H2O", {
  prj <- init_test_project(repo_adapters =
                             c("S3[http://h2o-release.s3.amazonaws.com/h2o-classic/cliffc-bulkremove/2/R/]",
                               "CRAN",
                               "Dir"))
  create_test_master_script("library(h2o)", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(
    c("h2o", "bitops", "lattice", "logging", "Matrix", "RCurl", "rjson", "statmod", "survival"),
    prj,
    versions = c("2.9.0.2", NA, NA, NA, NA, NA, NA, NA, NA))
})
