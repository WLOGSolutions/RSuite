#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if project structure handling works properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Updating old project deployment switch", {
  wspace_dir <- get_wspace_dir()
  unzip(zipfile = file.path(get_data_dir(), .Platform$OS.type, "TestRepo_rsuite_vNone.zip"),
        exdir = wspace_dir)

  proj_dir <- file.path(wspace_dir, "TestRepo")
  on_test_exit(function() {
    unlink(proj_dir, recursive = T, force = T)
  })
  exp_path <- get_repo_path(proj_dir) # upgrades repository so prj_start can work

  RSuite::prj_start(name = "TestRepo", path = wspace_dir)

  cur_path <- suppressWarnings({
    get_repo_path(file.path(proj_dir, "deployment"))
  })
  expect_equal(cur_path, exp_path)
})

test_that_managed("Handling project with package without NAMESPACE", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  create_test_package("TestPackage", prj)
  # remove NAMESPACE
  unlink(file.path(prj$path, "packages", "TestPackage", "NAMESPACE"), recursive = T, force = T)

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("logging"), prj)

  RSuite::prj_build(prj)
  expect_that_packages_installed(c("logging", "TestPackage"), prj)
})
