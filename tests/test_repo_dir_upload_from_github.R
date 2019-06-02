#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if uploading GitHub packages into directory works properly [test_repo_dir_upload_from_github]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")


test_that_managed("Uploading GitHub (basic)", {
  roxygen2_v610_available <- (compareVersion(as.character(packageVersion("roxygen2")), "6.1.0") >= 0)
  skip_if_not(roxygen2_v610_available, "roxygen2 >= 6.1.0 required to build rAzureBatch")

  rver_33up_for_macos <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.3.0") >= 0
                          || !grepl("darwin", R.version$os))
  skip_if_not(rver_33up_for_macos, "R >= 3.3 required to build rAzureBatch on MacOS")

  rver_34up_for_win <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.4.0") >= 0
                        || .Platform$OS.type != "windows")
  skip_if_not(rver_34up_for_win, "R before 3.4 has problems with SSL on Windows")


  prj <- init_test_project(repo_adapters = c("CRAN"))
  mgr <- init_test_manager(prj)
  RSuite::prj_config_set_repo_adapters(c("CRAN", sprintf("Dir[%s]", mgr$path)), prj = prj)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/rAzureBatch@v0.5.7", prj = prj, pkg_type = "source")
  expect_that_packages_available("rAzureBatch", "source", mgr)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/doAzureParallel@v0.6.3", prj = prj, pkg_type = "source")
  expect_that_packages_available(c("rAzureBatch", "doAzureParallel"), "source", mgr)
})

test_that_managed("Uploading GitHub (with deps)", {
  roxygen2_v610_available <- (compareVersion(as.character(packageVersion("roxygen2")), "6.1.0") >= 0)
  skip_if_not(roxygen2_v610_available, "roxygen2 >= 6.1.0 required to build rAzureBatch")

  rver_33up_for_macos <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.3.0") >= 0
                          || !grepl("darwin", R.version$os))
  skip_if_not(rver_33up_for_macos, "R >= 3.3 required to build rAzureBatch on MacOS")

  rver_34up_for_win <- (compareVersion(paste0(R.version$major, ".", R.version$minor), "3.4.0") >= 0
                        || .Platform$OS.type != "windows")
  skip_if_not(rver_34up_for_win, "R before 3.4 has problems with SSL on Windows")


  prj <- init_test_project(repo_adapters = c("CRAN"))
  mgr <- init_test_manager(prj)
  RSuite::prj_config_set_repo_adapters(c("CRAN", sprintf("Dir[%s]", mgr$path)), prj = prj)

  RSuite::repo_upload_github_package(mgr$repo_mgr, repo = "Azure/rAzureBatch@v0.5.7", prj = prj, pkg_type = "source",
                                     with_deps = T)
  expect_that_packages_available(c("rAzureBatch",
                                   "bitops", "curl", "digest", "httr", "jsonlite", "mime",
                                   "openssl", "R6", "RCurl", "rjson"),
                                 "source", mgr,
                                 optional_names = c("askpass", "sys")) # openssl > 1.2.1 imports askpass -> sys
})
