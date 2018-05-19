#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/pkgzip_management.R")

context("Testing if creation of PKGZIP from GitHub works properly")

test_that_managed("Create PKGZIP out of sources on GitHub (basic)", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  pkgzip <- init_test_pkgzip()

  RSuite::pkgzip_build_github_package("Azure/rAzureBatch@v0.5.7",
                                      prj = prj, pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("rAzureBatch", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of sources on GitHub (with deps)", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  pkgzip <- init_test_pkgzip()

  RSuite::pkgzip_build_github_package("Azure/rAzureBatch@v0.5.7",
                                      prj = prj, pkg_type = "source", path = pkgzip$path,
                                      with_deps = T)

  expect_that_pkgzip_contains(c("rAzureBatch",
                                "bitops", "curl", "digest", "httr", "jsonlite", "mime",
                                "openssl", "R6", "RCurl", "rjson"),
                              type = "source", pkgzip = pkgzip)
})
