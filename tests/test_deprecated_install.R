#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if deprecated packages handling works properly")


test_that_managed("Handling unavailable deprecated package", {
  # Create project
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  
  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependency")
  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.0", "0.7-103"))
  
  # "Create" deprecated dependency
  remove_package_from_lrepo(pkg_file = "TestDependency_1.0.zip", prj = prj)
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.1", "0.7-103"))
})
  

test_that_managed("Handling available \"deprecated\" package", {
  # Create project
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependency (== 1.0)") # force to install 1.0
  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.0", "0.7-103"))
  
  # update dependency (to ensure that if 1.0 is still available 1.1 will not be installed)
  set_test_package_deps("TestPackage", prj = prj, deps = "TestDependency")
  

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.0", "0.7-103"))
})
