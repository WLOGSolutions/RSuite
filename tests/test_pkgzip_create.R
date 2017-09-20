#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/pkgzip_management.R")

context("Testing if creation of PKGZIP works properly")

test_that_managed("Create PKGZIP out of project packages", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  pkgzip <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")
  RSuite::prj_install_deps(prj)

  RSuite::pkgzip_build_prj_packages(prj = prj, zip_ver = "1.0", pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("TestPackage", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of package files", {
  pkgzip <- init_test_pkgzip()

  RSuite::pkgzip_build_package_files(files = file.path("data", "logging_0.7-103.tar.gz"),
                                     path = pkgzip$path)

  expect_that_pkgzip_contains("logging", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of external package", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  pkgzip <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  RSuite::pkgzip_build_ext_packages("logging", prj = prj, pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("logging", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of sources on GitHub", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  pkgzip <- init_test_pkgzip()
  
  RSuite::pkgzip_build_github_package("Azure/rAzureBatch", 
                                      prj = prj, pkg_type = "source", path = pkgzip$path)
  
  expect_that_pkgzip_contains("rAzureBatch", type = "source", pkgzip = pkgzip)
})
