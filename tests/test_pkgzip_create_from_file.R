#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if creation of PKGZIP from package file works properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/pkgzip_management.R")


test_that_managed("Create PKGZIP out of package files", {
  pkgzip <- init_test_pkgzip()

  RSuite::pkgzip_build_package_files(files = file.path("data", "logging_0.7-103.tar.gz"),
                                     path = pkgzip$path)

  expect_that_pkgzip_contains("logging", type = "source", pkgzip = pkgzip)
})

