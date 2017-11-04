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

context("Testing if creation of PKGZIP from project packages works properly")

test_that_managed("Create PKGZIP out of project packages (basic)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  pkgzip <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")
  RSuite::prj_install_deps(prj)

  RSuite::pkgzip_build_prj_packages(prj = prj, zip_ver = "1.0",
                                    pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("TestPackage", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of project packages (pkg selection)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  pkgzip <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage1", prj, deps = "R (>= 3.1.0), methods")
  create_test_package("TestPackage2", prj, deps = "TestPackage1")
  RSuite::prj_install_deps(prj)

  RSuite::pkgzip_build_prj_packages(pkgs = "TestPackage2", prj = prj, zip_ver = "1.0",
                                    pkg_type = "source", path = pkgzip$path)

  expect_that_pkgzip_contains("TestPackage2", type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of project packages (with deps)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  pkgzip <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")
  RSuite::prj_install_deps(prj)

  RSuite::pkgzip_build_prj_packages(prj = prj, zip_ver = "1.0",
                                    pkg_type = params$bin_pkgs_type, path = pkgzip$path,
                                    with_deps = TRUE)

  expect_that_pkgzip_contains(c("TestPackage", "logging"), type = params$bin_pkgs_type, pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of project packages (with filtering)", {
  # first build repository containing TestPackage1 v1.0
  prj1 <- init_test_project(repo_adapters = c("Dir"), name = "TestPackage1")
  pkgzip1 <- init_test_pkgzip()

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj1, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj1, type = "source", ver = "1.0")
  RSuite::pkgzip_build_ext_packages("TestPackage1", prj = prj1, pkg_type = "source", path = pkgzip1$path)
  expect_that_pkgzip_contains("TestPackage1", type = "source", pkgzip = pkgzip1)

  mgr <- init_test_manager(prj1)
  RSuite::repo_upload_pkgzip(repo_manager = mgr$repo_mgr, pkgzip = pkgzip1$get_pkgzip_fpath())

  # next build project with repository containing TestPackage1 v2.0
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo("TestPackage1", prj, type = "source", ver = "2.0")
  create_test_package("TestPackage2", prj, deps = "TestPackage1")
  RSuite::prj_install_deps(prj)

  pkgzip <- init_test_pkgzip()
  RSuite::pkgzip_build_prj_packages(pkgs = "TestPackage2", prj = prj, zip_ver = "1.0",
                                    pkg_type = "source", path = pkgzip$path,
                                    with_deps = T, filter_repo = mgr$url)

  # it will not contain TestPackage1 as v1.0 (which is sufficient) is available in mgr$url repository
  expect_that_pkgzip_contains(c("TestPackage2", "logging"), type = "source", pkgzip = pkgzip)
})

test_that_managed("Create PKGZIP out of project packages (with proj deps)", {
  # first build repository containing TestPackage1 v1.0
  prj <- init_test_project(repo_adapters = c("Dir"))
  pkgzip <- init_test_pkgzip()

  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage1", prj)
  create_test_package("TestPackage2", prj, deps = "TestPackage1")
  RSuite::prj_install_deps(prj)

  pkgzip <- init_test_pkgzip()
  RSuite::pkgzip_build_prj_packages(pkgs = "TestPackage2", prj = prj, zip_ver = "1.0",
                                    pkg_type = "source", path = pkgzip$path, with_deps = T)

  expect_that_pkgzip_contains(c("TestPackage2", "TestPackage1", "logging"), type = "source", pkgzip = pkgzip)
})
