#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if project packages are built regarding dependencies [test_deps_resolving]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

register_project_templ("TestDepsResolving", function(prj) {
  params <- prj$load_params()
  create_package_deploy_to_lrepo(name = "TestDep1", prj = prj, ver = "1.0", type = params$bin_pkgs_type)
  create_package_deploy_to_lrepo(name = "TestDep2", prj = prj, ver = "1.0", type = params$bin_pkgs_type,
                                 deps = "TestDep1 (>= 1.0)")

  create_package_deploy_to_lrepo(name = "TestDep1", prj = prj, ver = "1.1", type = params$bin_pkgs_type)
  create_package_deploy_to_lrepo(name = "TestDep2", prj = prj, ver = "1.1", type = params$bin_pkgs_type,
                                 deps = "TestDep1 (>= 1.1)")

  create_package_deploy_to_lrepo(name = "TestDep3", prj = prj, ver = "1.0", type = params$bin_pkgs_type,
                                 deps = "TestDep2")
})



test_that_managed("Installs dependency sequence TestPackage1 imports TestPackage2", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  create_test_package("TestPackage1", prj, imps = "logging, TestPackage2")
  set_test_package_ns_imports("TestPackage1", prj, c("logging", "TestPackage2"))
  create_test_package("TestPackage2", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)

  RSuite::prj_build(prj)

  expect_that_packages_installed(c("TestPackage1", "TestPackage2", "logging"), prj)
})


test_that_managed("Installs dependency sequence src -> bin -> src -> Package", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103
  bin_type <- ifelse(.Platform$pkgType != "source", .Platform$pkgType, "binary")

  create_package_deploy_to_lrepo(name = "TestPackage1", prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestPackage2", prj, type = bin_type, deps = "TestPackage1")
  create_package_deploy_to_lrepo(name = "TestPackage3", prj, type = "source", deps = "TestPackage2")
  create_test_package("TestPackage4", prj, deps = "TestPackage3")

  RSuite::prj_install_deps(prj, clean = T)

  expect_that_packages_installed(c("logging", "TestPackage1", "TestPackage2", "TestPackage3"), prj)

  RSuite::prj_build(prj)

  expect_that_packages_installed(c("logging", "TestPackage1", "TestPackage2", "TestPackage3", "TestPackage4"), prj)
})


test_that_managed("Test if specific subdependencies versions are handled properly", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestDepsResolving"))

  create_test_package("TestPackage", prj, deps = c("TestDep1 (== 1.0)", # should TestDep2 == 1.0
                                                   "TestDep2"))

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(names =  c("logging", "TestDep2", "TestDep1"),
                                 prj = prj,
                                 versions = c(NA, "1.0", "1.0"))
})

test_that_managed("Multiple repositories subdependency in previous repo.", {
  # setup repositories and test project
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestDepsResolving"))
  params <- prj$load_params()

  rmgr1 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr1$repo_mgr, c("TestDep1", "logging"),
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  rmgr2 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr2$repo_mgr, "TestDep2",
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  RSuite::prj_config_set_repo_adapters(repos = sprintf("Dir[%s]", c(rmgr1$path, rmgr2$path)),
                                       prj = prj)

  create_test_package("TestPackage", prj, deps = "TestDep2") # TestDep2 <- TestDep1


  RSuite::prj_install_deps(prj)


  expect_that_packages_installed(names =  c("logging", "TestDep2", "TestDep1"),
                                 prj = prj)
})


test_that_managed("Multiple repositories subsubdependency -> subdependency -> dependency  in previous repo.", {
  # setup repositories and test project
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestDepsResolving"))
  params <- prj$load_params()

  rmgr1 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr1$repo_mgr, c("TestDep1", "logging"),
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  rmgr2 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr2$repo_mgr, "TestDep2",
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  rmgr3 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr3$repo_mgr, "TestDep3",
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  RSuite::prj_config_set_repo_adapters(repos = sprintf("Dir[%s]", c(rmgr1$path, rmgr2$path, rmgr3$path)),
                                       prj = prj)

  create_test_package("TestPackage", prj, deps = c("TestDep3")) # TestDep3 <- TestDep2 <- TestDep1


  RSuite::prj_install_deps(prj)


  expect_that_packages_installed(names =  c("logging", "TestDep1", "TestDep2", "TestDep3"),
                                 prj = prj)
})


test_that_managed("Multiple repositories subdependency in next repo.", {
  # setup repositories and test project
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestDepsResolving"))
  params <- prj$load_params()

  rmgr1 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr1$repo_mgr, c("TestDep1", "logging"),
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  rmgr2 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr2$repo_mgr, "TestDep2",
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  RSuite::prj_config_set_repo_adapters(repos = sprintf("Dir[%s]", c(rmgr1$path, rmgr2$path)),
                                       prj = prj)

  create_test_package("TestPackage", prj, deps = c("TestDep2 (>= 1.0)", # TestDep2 <- TestDep1
                                                   "TestDep2"))

  RSuite::prj_install_deps(prj)


  expect_that_packages_installed(names =  c("logging", "TestDep2", "TestDep1"),
                                 prj = prj)
})


test_that_managed("Multiple repositories unavailable dependency.", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = get_project_templ("TestDepsResolving"))
  params <- prj$load_params()

  rmgr1 <- init_test_manager(prj = prj)
  RSuite::repo_upload_ext_packages(rmgr1$repo_mgr, "logging",
                                   prj = prj, pkg_type = params$bin_pkgs_type)

  rmgr2 <- init_test_manager(prj = prj)

  RSuite::prj_config_set_repo_adapters(repos = sprintf("Dir[%s]", c(rmgr1$path, rmgr2$path)),
                                       prj = prj)


  create_test_package("TestPackage", prj, deps = c("TestDep1"))


  expect_error(RSuite::prj_install_deps(prj), "Required dependencies are not available: TestDep1")
})
