#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if deprecated packages handling works properly [test_deprecated_install]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

register_project_templ("TestDeprecatedInstall", function(prj) {
  params <- prj$load_params()
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0", type = params$bin_pkgs_type)
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1", type = params$bin_pkgs_type)
})

test_that_managed("Handling unavailable deprecated package", {
  # Create project
  prj <- init_test_project(name = "TestProject", repo_adapters = c("Dir"),
                           tmpl = get_project_templ("TestDeprecatedInstall"))
  params <- prj$load_params()

  # Prepare repo
  remove_package_from_lrepo(pkg_file = "TestDependency_1.1.*", prj = prj, type = params$bin_pkgs_type)

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependency")
  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.0", "0.7-103"))

  # "Create" deprecated dependency
  prj <- init_test_project(name = "TestProject", repo_adapters = c("Dir"), # reinitialize from template
                           tmpl = get_project_templ("TestDeprecatedInstall"))
  remove_package_from_lrepo(pkg_file = "TestDependency_1.0.*", prj = prj, type = params$bin_pkgs_type)

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.1", "0.7-103"))
})


test_that_managed("Handling available \"deprecated\" package", {
  # Create project
  prj <- init_test_project(repo_adapters = c("Dir"),
                           tmpl = get_project_templ("TestDeprecatedInstall"))
  params <- prj$load_params()

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependency (== 1.0)") # force to install 1.0
  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.0", "0.7-103"))

  # update dependency (to ensure that if 1.0 is still available 1.1 will not be installed)
  set_test_package_deps("TestPackage", prj = prj, deps = "TestDependency")

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(c("TestDependency", "logging"), prj = prj, versions = c("1.0", "0.7-103"))
})
