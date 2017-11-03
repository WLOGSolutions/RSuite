#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/pkgzip_management.R")
source("R/repo_management.R")

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
  create_package_deploy_to_lrepo("TestPackage1", prj, type = "source")

  RSuite::pkgzip_build_ext_packages("TestPackage1", prj = prj, pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("TestPackage1", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of external package with dependencies", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  pkgzip <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj, type = "source")
  create_package_deploy_to_lrepo("TestPackage2", prj, deps = "TestPackage1", type = "source")

  RSuite::pkgzip_build_ext_packages("TestPackage2", prj = prj, pkg_type = "source", path = pkgzip$path,
                                    with_deps = T)

  expect_that_pkgzip_contains(c("logging", "TestPackage1", "TestPackage2"), type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of external package with filtering", {
  # first build repository containing TestPackage1 v1.0
  prj1 <- init_test_project(repo_adapters = c("Dir"), name = "TestPackage1")
  pkgzip1 <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj1, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj1, type = "source", ver = "1.0")
  RSuite::pkgzip_build_ext_packages("TestPackage1", prj = prj1, pkg_type = "source", path = pkgzip1$path)
  expect_that_pkgzip_contains("TestPackage1", type = "source", pkgzip = pkgzip1)

  mgr <- init_test_manager(prj1)
  RSuite::repo_upload_pkgzip(repo_manager = mgr$repo_mgr, pkgzip = pkgzip1$get_pkgzip_fpath())

  # next build project with repository containing TestPackage1 v2.0 and TestPackage2
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj, type = "source", ver = "2.0")
  create_package_deploy_to_lrepo("TestPackage2", prj, deps = "TestPackage1", type = "source")

  pkgzip <- init_test_pkgzip()
  RSuite::pkgzip_build_ext_packages("TestPackage2", prj = prj, pkg_type = "source", path = pkgzip$path,
                                    with_deps = T, filter_repo = mgr$url)

  # it will not contain TestPackage1 as v1.0 (which is sufficient) is available in mgr$url repository
  expect_that_pkgzip_contains(c("logging", "TestPackage2"), type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of external package with filtering and version select", {
  # first build repository containing TestPackage1 v1.0
  prj1 <- init_test_project(repo_adapters = c("Dir"), name = "TestPackage1")
  pkgzip1 <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj1, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj1, type = "source", ver = "1.0")
  RSuite::pkgzip_build_ext_packages("TestPackage1", prj = prj1, pkg_type = "source", path = pkgzip1$path)
  expect_that_pkgzip_contains("TestPackage1", type = "source", pkgzip = pkgzip1)

  mgr <- init_test_manager(prj1)
  RSuite::repo_upload_pkgzip(repo_manager = mgr$repo_mgr, pkgzip = pkgzip1$get_pkgzip_fpath())

  # next build project with repository containing TestPackage1 v2.0 and TestPackage2
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj, type = "source", ver = "2.0")
  create_package_deploy_to_lrepo("TestPackage2", prj, deps = "TestPackage1 (>= 2.0)", type = "source")

  pkgzip <- init_test_pkgzip()
  RSuite::pkgzip_build_ext_packages("TestPackage2", prj = prj, pkg_type = "source", path = pkgzip$path,
                                    with_deps = T, filter_repo = mgr$url)

  # TestPackage1 v1.0 is available in mgr$url repository, but TestPackage2 requires v2.0
  expect_that_pkgzip_contains(c("logging", "TestPackage2", "TestPackage1"), type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of sources on GitHub", {
  prj <- init_test_project(repo_adapters = c("CRAN"))
  pkgzip <- init_test_pkgzip()

  RSuite::pkgzip_build_github_package("Azure/rAzureBatch",
                                      prj = prj, pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("rAzureBatch", type = "source", pkgzip = pkgzip)
})
