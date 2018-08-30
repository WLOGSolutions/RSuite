#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing proper version selection [test_version_selection]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

register_project_templ("TestVersionSelection", function(prj) {
  params <- prj$load_params()
  create_package_deploy_to_lrepo(name = "TestDep1", prj = prj, ver = "1.0", type = params$bin_pkgs_type)
  create_package_deploy_to_lrepo(name = "TestDep2", prj = prj, ver = "1.0", type = params$bin_pkgs_type,
                                 deps = "TestDep1 (>= 1.0)")

  create_package_deploy_to_lrepo(name = "TestDep1", prj = prj, ver = "1.1", type = params$bin_pkgs_type)
  create_package_deploy_to_lrepo(name = "TestDep2", prj = prj, ver = "1.1", type = params$bin_pkgs_type,
                                 deps = "TestDep1 (>= 1.1)")
})

test_that_managed("Installs last available if no requirements", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.1", NA))
})

test_that_managed("Installs proper version if max version requirement passed", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1(<= 1.0)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.0", NA))
})

test_that_managed("Installs proper version if exact version requirement passed", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1(== 1.0)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.0", NA))
})


test_that_managed("Installs newer version if required", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))
  params <- prj$load_params()

  rmgr1 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr1$repo_mgr, c("TestDep1", "logging"),
                                   prj = prj, pkg_type = params$bin_pkgs_type)
  # rmgr1 repo contains only TestDep1 v1.1

  rmgr <- RSuite::repo_mng_start("Dir", path = file.path(prj$path, "repository"), rver = params$r_ver,
                                 types = c(params$pkgs_type, params$aux_pkgs_type))
  RSuite::repo_mng_remove(rmgr,
                          toremove = data.frame(Package = "TestDep1", Version = "1.1", stringsAsFactors = FALSE),
                          pkg_type = params$bin_pkgs_type)
  # Dir repo does not contain TestDep1 v1.1

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1(>= 1.0)")


  RSuite::prj_install_deps(prj)


  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.0", NA))


  remove_test_packages(prj)
  RSuite::prj_config_set_repo_adapters(repos = sprintf("Dir[%s]", rmgr1$path), prj = prj)

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1(>= 1.1)")


  RSuite::prj_install_deps(prj)


  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.1", NA))
})

test_that_managed("Test if failes if old version only available", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1(>= 1.2), TestDep2")

  expect_error(RSuite::prj_install_deps(prj),
               "Required dependencies are not available: TestDep1$")
})


test_that_managed("Test if failes if new version only available", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1(<= 0.9), TestDep2")

  expect_error(RSuite::prj_install_deps(prj),
               "Required dependencies are not available: TestDep1, TestDep2$")
  # TestDep2 is also not available as requirement TestDep1 >= 1.0 not satisfied
})


test_that_managed("Test if strict inequalities are handled properly", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1 (> 1.0)")
  RSuite::prj_install_deps(prj = prj)
  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.1", NA))

  set_test_package_deps(name = "TestPackage", prj = prj, deps = "TestDep1 (< 1.1)")
  RSuite::prj_install_deps(prj = prj, clean = TRUE)
  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.0", NA))
})


test_that_managed("Test if suggests versions are chosen properly", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestVersionSelection"))

  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.1", type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.2", type = "source")
  create_package_deploy_to_lrepo(name = "TestSuggest", prj = prj, ver = "0.1", deps = "TestDependency (>= 0.1)",
                                 type = "source")
  create_package_deploy_to_lrepo(name = "TestSuggest", prj = prj, ver = "0.2", deps = "TestDependency (>= 0.2)",
                                 type = "source")

  create_test_package(name = "TestPackage", prj = prj, deps = "TestDep1 (== 1.0)")
  RSuite::prj_install_deps(prj = prj)
  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.0", NA))


  set_test_package_deps(name = "TestPackage", prj = prj, deps = "TestDep1", sugs = "TestDep2")
  test_file_path <- file.path(prj$load_params()$pkgs_path, "TestPackage", "tests")
  dir.create(test_file_path)
  writeLines(c("library(TestDep2)"), con = file.path(test_file_path, "test_case.R"))

  RSuite::prj_install_deps(prj = prj)
  expect_that_packages_installed(c("TestDep1", "logging"), prj, versions = c("1.0", NA))
  # although TestDep2 1.1 exists, but
  #  installed TestDep1 is in version 1.0 and TestDep2 1.1 <- TestDep1 (>= 1.1)
  #  so TestDep2 1.0 should be in sbox (TestDep2 1.0 <- TestDep1 (>= 1.0))
  expect_that_packages_installed(c("TestDep2"), prj, versions = c("1.0"), supports = TRUE)
})


test_that_managed("Test if version dependecies are checked numerically (0.9 < 0.12)", {
  prj <- init_test_project(repo_adapters = c("Dir"))  # uses BaseTestProjectTemplate with logging 0.7-103

  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.12")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "0.9")
  create_test_package(name = "TestPackage", prj = prj, deps = "TestDependency(>= 0.9)")

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("0.12", NA))
})

