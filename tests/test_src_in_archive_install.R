#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing installation from source archive")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

test_that_managed("glue in archive version is installed", {
  skip_if(is.null(Sys.which("gcc")), "No src package build tools available (e.g. Rtools)")

  # CRAN contains in main repository glue 1.3.0 for R3.5 and 1.2.0 for R3.3 & R 3.4
  #  glue 1.1.0 is available only in source archive

  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage1", prj, imps = "glue (== 1.1)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging", "glue"), prj, c("0.7-103", "1.1.0")) # logging is always installed
})

