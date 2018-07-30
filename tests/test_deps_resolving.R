#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if project packages are built regarding dependencies")

test_that_managed("Installs dependency sequence TestPackage1 imports TestPackage2", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage1", prj, imps = "logging, TestPackage2")
  set_test_package_ns_imports("TestPackage1", prj, c("logging", "TestPackage2"))
  create_test_package("TestPackage2", prj)

  RSuite::prj_install_deps(prj)

  expect_that_packages_installed(c("logging"), prj)

  RSuite::prj_build(prj)

  expect_that_packages_installed(c("TestPackage1", "TestPackage2", "logging"), prj)
})


test_that_managed("Installs dependency sequence src -> bin -> src -> Package", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  bin_type <- ifelse(.Platform$pkgType != "source", .Platform$pkgType, "binary")

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
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
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj, ver = "1.0", type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj, ver = "1.1", type = "source")
  create_package_deploy_to_lrepo(name = "TestParentDependency", prj, ver = "1.0",
                                 deps = "TestDependency (>= 1.0)", type = "source")
  create_package_deploy_to_lrepo(name = "TestParentDependency", prj, ver = "1.1",
                                 deps = "TestDependency (>= 1.1)", type = "source")

  create_test_package("TestPackage", prj, deps = c("TestDependency (== 1.0)",
                                                   "TestParentDependency"))

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(names =  c("logging", "TestDependency", "TestParentDependency"),
                                 prj = prj,
                                 versions = c(NA, "1.0", "1.0"))
})

test_that_managed("Multiple repositories subdependency in previous repo.", {
  # setup repositories and test project
  repo_adapter <- init_test_dir_adapter("TestDir")
  prj <- init_test_project(repo_adapters = c("Dir"))
  repo_manager <- init_test_manager(prj = prj,
                                    ra_name = "TestDir")
  prj <- init_test_project(repo_adapters = c("Dir", sprintf("TestDir[%s]", repo_manager$path)))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  # upload subdependency to first repo
  create_package_deploy_to_lrepo(name = "TestSubdependency", prj = prj, type = "source")

  # upload dependency to second repo
  create_package_deploy_to_repo("TestDependency", prj = prj,
                                repo_manager = repo_manager$repo_mgr,
                                deps = "TestSubdependency",
                                type = "source")
  
  create_test_package("TestPackage", prj, deps = c("TestDependency (== 1.0)",
                                                   "TestDependency"))

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(names =  c("logging",
                                            "TestDependency",
                                            "TestSubdependency"),
                                 prj = prj)
})


test_that_managed("Multiple repositories subsubdependency -> subdependency -> dependency  in previous repo.", {
  # setup repositories and test project
  init_test_dir_adapter("Dir2")
  init_test_dir_adapter("Dir3")
  prj <- init_test_project(repo_adapters = c("Dir"))
  dir2_repo_manager <- init_test_manager(prj = prj,
                                    ra_name = "Dir2")
  dir3_repo_manager <- init_test_manager(prj = prj,
                                    ra_name = "Dir3")
  prj <- init_test_project(repo_adapters = c("Dir",
                                             sprintf("Dir2[%s]", dir2_repo_manager$path),
                                             sprintf("Dir3[%s]", dir3_repo_manager$path)))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  
  # upload subdependency to first repo
  create_package_deploy_to_lrepo(name = "TestDependency1", prj = prj, type = "source")
  
  # upload dependency to second repo
  create_package_deploy_to_repo("TestDependency2", prj = prj,
                                repo_manager = dir2_repo_manager$repo_mgr,
                                deps = "TestDependency1",
                                type = "source")
  
  # upload dependency to third repo
  create_package_deploy_to_repo("TestDependency3", prj = prj,
                                repo_manager = dir3_repo_manager$repo_mgr,
                                deps = "TestDependency2",
                                type = "source")
  
  create_test_package("TestPackage", prj, deps = c("TestDependency3"))

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(names =  c("logging",
                                            "TestDependency1",
                                            "TestDependency2",
                                            "TestDependency3"),
                                 prj = prj)
})


test_that_managed("Multiple repositories subdependency in next repo.", {
  # setup repositories and test project
  repo_adapter <- init_test_dir_adapter("TestDir")
  prj <- init_test_project(repo_adapters = c("Dir"))
  repo_manager <- init_test_manager(prj = prj,
                                    ra_name = "TestDir")
  prj <- init_test_project(repo_adapters = c("Dir", sprintf("TestDir[%s]", repo_manager$path)))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  
  # upload subdependency to first repo
  create_package_deploy_to_repo(name = "TestSubdependency",
                                repo_manager = repo_manager$repo_mgr,
                                prj = prj,
                                type = "source")
  
  # upload dependency to second repo
  create_package_deploy_to_lrepo("TestDependency", prj = prj,
                                deps = "TestSubdependency",
                                type = "source")
  create_test_package("TestPackage", prj, deps = c("TestDependency (== 1.0)",
                                                   "TestDependency"))

  RSuite::prj_install_deps(prj)
  expect_that_packages_installed(names =  c("logging",
                                            "TestDependency",
                                            "TestSubdependency"),
                                 prj = prj)
})


test_that_managed("Multiple repositories unavailable dependency.", {
  repo_adapter <- init_test_dir_adapter("TestDir")
  prj <- init_test_project(repo_adapters = c("Dir"))
  repo_manager <- init_test_manager(prj = prj,
                                    ra_name = "TestDir")
  prj <- init_test_project(repo_adapters = c("Dir", sprintf("TestDir[%s]", repo_manager$path)))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj, deps = c("TestDependency"))

  expect_error(RSuite::prj_install_deps(prj), "Required dependencies are not available: TestDependency")
}) 