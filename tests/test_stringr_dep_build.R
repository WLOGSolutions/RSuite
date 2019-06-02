#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if building succeeds for package dependant on stringr (roxygen issue) [test_stringr_dep_build]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Build package which dependents on stringr", {
  rver_33up_for_macos <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.3.0") >= 0
                          || !grepl("darwin", R.version$os))
  skip_if_not(rver_33up_for_macos, "R < 3.3 is too old for MacOS")


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
