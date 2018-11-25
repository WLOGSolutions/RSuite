#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if base dependencies are handled properly [test_base_deps_handling]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")


test_that_managed("Instaling package which requires methods", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  create_test_package("TestPackage", prj, deps = "R (>= 3.1.0), methods")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})

test_that_managed("Build deps which contains library(grid) in master script which is base package", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  create_test_master_script(code = "library(grid)", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})

register_project_templ("TestBaseDepsInstall", function(prj) {
  params <- prj$load_params()
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, type = params$bin_pkgs_type)
})

test_that_managed("Build deps which contains library(TestDependency) in master script", {
  prj <- init_test_project(repo_adapters = c("Dir"),
                           tmpl = get_project_templ("TestBaseDepsInstall"))

  create_test_master_script(code = "library(TestDependency)", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj)
})

test_that_managed("Build deps which contains library(\"TestDependency\") in master script", {
  prj <- init_test_project(repo_adapters = c("Dir"),
                           tmpl = get_project_templ("TestBaseDepsInstall"))

  create_test_master_script(code = "library(\"TestDependency\")", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj)
})

test_that_managed("Build deps which contains TestDependency::something in master script", {
  prj <- init_test_project(repo_adapters = c("Dir"),
                           tmpl = get_project_templ("TestBaseDepsInstall"))

  create_test_master_script(code = "TestDependency::something", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj)
})

test_that_managed("Build deps which contains TestDependency:::something in master script", {
  prj <- init_test_project(repo_adapters = c("Dir"),
                           tmpl = get_project_templ("TestBaseDepsInstall"))

  create_test_master_script(code = "TestDependency:::something", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj)
})

test_that_managed("Build deps which contains package.with.dot::some_fun() in master script", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103
  create_package_deploy_to_lrepo(name = "package.with.dot", prj = prj,
                                 ver = "1.0", type = prj$load_params()$bin_pkgs_type)

  create_test_master_script(code = "package.with.dot::some_fun()", prj = prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("package.with.dot", "logging"), prj)
})

test_that_managed("Build deps which contains commented implicit deps in master script", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  create_test_master_script(code = paste("# library(colorspace)",
                                         "something at the beginning of line   # colorspace::coords",
                                         "# colorspace::coords",
                                         "something # package.with.dot:::some_fun",
                                         sep = "\n"),
                            prj = prj)


  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})

test_that_managed("Build deps which contains dep references in strings", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  create_test_master_script(code = paste(" some code \" some test with \\\" inside library(colorspace)\"",
                                         "'colorspace::coords'",
                                         "and some more code 'some text \" colorspace::coords'",
                                         " \" probably some Rcpp code package.with.dot:::some_fun \" ",
                                         sep = "\n"),
                            prj = prj)


  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)
})
