#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if project environment locking works properly")

test_that_managed("Project environment lock file creation", {
   # Arrange
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
   create_test_package("TestPackage", prj, deps = c("logging"))

   RSuite::prj_install_deps(prj)
   expected_lock_data <- utils::installed.packages(lib.loc = params$lib_path)

   # Act
   RSuite::prj_lock_env(prj)
   lock_data <- read.dcf(params$lock_path)

   # Assert
   expect_true(file.exists(params$lock_path))
   expect_equal(lock_data[, c("Package", "Version")], expected_lock_data[, c("Package", "Version")])
 })

test_that_managed("Locked environment, no unfeasibles", {
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   # Prepare repo
   pkg_deps <- "TestDependency"
   deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
   create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.0")

   # Create package and install deps
   create_test_package("TestPackage", prj, deps = pkg_deps)
   RSuite::prj_install_deps(prj)

   # Lock environment
   RSuite::prj_lock_env(prj)

   # Add newer version and rebuild
   create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.1")
   RSuite::prj_install_deps(prj, clean = TRUE)

   expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.0", "0.7-103"))
 })

test_that_managed("Locked environment, unfeasibles", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  pkg_deps <- "TestDependency"
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.0")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = pkg_deps)
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add newer version and rebuild
  create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.1")
  create_test_package("TestPackage2", prj, deps = "TestDependency(>= 1.1)")
  
  
  warn_msg <- paste("Lock made the following package unfeasible:", pkg_deps, sep = " ")
  expect_warning(RSuite::prj_install_deps(prj, clean = TRUE), regexp = "unfeasible") 
})
