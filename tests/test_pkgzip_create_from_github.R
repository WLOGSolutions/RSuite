#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if creation of PKGZIP from GitHub works properly [test_pkgzip_create_from_github]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/pkgzip_management.R")


test_that_managed("Create PKGZIP out of sources on GitHub (basic)", {
  roxygen2_v610_available <- (compareVersion(as.character(packageVersion("roxygen2")), "6.1.0") >= 0)
  skip_if_not(roxygen2_v610_available, "roxygen2 >= 6.1.0 required to build rAzureBatch")

  rver_33up_for_macos <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.3.0") >= 0
                          || !grepl("darwin", R.version$os))
  skip_if_not(rver_33up_for_macos, "R >= 3.3 required to build rAzureBatch on MacOS")

  prj <- init_test_project(repo_adapters = c("CRAN"))
  pkgzip <- init_test_pkgzip()

  RSuite::pkgzip_build_github_package("Azure/rAzureBatch@v0.5.7",
                                      prj = prj, pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("rAzureBatch", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of sources on GitHub (with deps)", {
  roxygen2_v610_available <- (compareVersion(as.character(packageVersion("roxygen2")), "6.1.0") >= 0)
  skip_if_not(roxygen2_v610_available, "roxygen2 >= 6.1.0 required to build rAzureBatch")

  rver_33up_for_macos <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.3.0") >= 0
                          || !grepl("darwin", R.version$os))
  skip_if_not(rver_33up_for_macos, "R >= 3.3 required to build rAzureBatch on MacOS")

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
