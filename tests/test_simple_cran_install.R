#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

context("Testing simple CRAN installation (dependencies detection, basic deployment scenarios)")

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
  create_test_master_script("library(lubridate)", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(
    c("lubridate", "stringr", "stringi", "magrittr", "logging"),
    prj)
})
