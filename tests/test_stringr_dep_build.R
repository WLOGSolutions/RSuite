#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if building succeeds for package dependant on stringr (roxygen issue)")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Build package which dependents on stringr", {
  prj <- init_test_project(repo_adapters = c("CRAN", "Dir"))
  create_test_package("TestPackage", prj, deps = "stringr (>= 1.3)")

  RSuite::prj_install_deps(prj, clean = T)

  expect_that_packages_installed(
    c("stringr", "magrittr", "stringi", "logging", "glue"), # glue presented in 1.3
    prj)

  RSuite::prj_build(prj)

  expect_that_packages_installed(
    c("TestPackage", "stringr", "magrittr", "stringi", "logging", "glue"), # glue presented in 1.3
    prj)
})
