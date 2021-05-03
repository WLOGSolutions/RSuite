#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing simple CRAN installation (dependencies detection, basic deployment scenarios) [test_simple_cran_install]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("No deps install", {
  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj) # logging is always installed
})

test_that_managed("Installing colorspace(no sub dependencies)", {
  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))
  create_test_master_script("library(colorspace)", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("colorspace", "logging"), prj)
})

test_that_managed("Installing sub dependencies of from CRAN", {
  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))
  create_test_master_script("library(munsell)", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("munsell", "colorspace", "logging"), prj)
})

test_that_managed("Installing sub sub dependencies of from CRAN", {
  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))
  create_test_package("TestPackage1", prj, imps = "logging, lubridate (>= 1.7), stringr (>= 1.3)")
  create_test_master_script("library(lubridate)", prj)

  RSuite::prj_install_deps(prj)

  expected_pkgs <- c("lubridate", "stringr", "stringi", "magrittr", "logging", "Rcpp", "glue")
  if (RSuite:::current_rver() != "3.2") {
    expected_pkgs <- c(expected_pkgs, "generics")
  }
  expect_that_packages_installed(expected_pkgs, prj)
})
