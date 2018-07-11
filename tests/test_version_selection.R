#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

context("Testing proper version selection")

test_that_managed("Installs last available if no requirements", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.1", NA))
})

test_that_managed("Installs proper version if max version requirement passed", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency(<= 1.0)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.0", NA))
})

test_that_managed("Installs proper version if exact version requirement passed", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency(== 1.0)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.0", NA))
})


test_that_managed("Installs newer version if required", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency(>= 1.0)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.0", NA))

  remove_test_packages(prj)
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency(>= 1.1)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.1", NA))
})

test_that_managed("Test if version dependecies are checked numerically (0.9 < 0.12)", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.12")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.9")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency(>= 0.9)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("0.12", NA))
})

test_that_managed("Test if failes if old version only available", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency1", prj = prj, ver = "0.1", type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency2", prj = prj, ver = "0.1", deps = "TestDependency1", type = "source")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency1(>= 0.2), TestDependency2")

  expect_error(RSuite::prj_install_deps(prj),
               "Required dependencies are not available: TestDependency")
})


test_that_managed("Test if failes if new version only available", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency1", prj = prj, ver = "0.2", type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency2", prj = prj, ver = "0.1", deps = "TestDependency1", type = "source")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency1(<= 0.1), TestDependency2")

  expect_error(RSuite::prj_install_deps(prj),
               "Required dependencies are not available: TestDependency")
})


test_that_managed("Test if strict inequalities are handled properly", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.1")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.2")
  
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency (> 0.1)")
  RSuite::prj_install_deps(prj = prj) 
  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("0.2", NA))
  
  set_test_package_deps("TestPackage", prj = prj, deps = "TestDependency (< 0.2)")
  RSuite::prj_install_deps(prj = prj, clean = TRUE) 
  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("0.1", NA))
})
