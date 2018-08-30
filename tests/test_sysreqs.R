#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if sysreqs functions work properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

test_that_managed("Collecting package system requirements", {
  prj <- init_test_project(repo_adapters = c("Dir")) 
  params <- prj$load_params()
  
  # create package with system requirements
  sysreqs <- "This package has some system requirements."
  sysreqs_pkg_name <- "PackageWithSystemRequirements"
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(sysreqs_pkg_name,
                                 prj = prj,
                                 sysreqs = sysreqs)
  
  create_test_package("TestPackage", prj = prj, deps = sysreqs_pkg_name)
  
  result <- RSuite::sysreqs_collect(prj = prj)
  
  expect_length(result, 1)
  expect_true(sysreqs_pkg_name %in% names(result))
  expect_match(result[[sysreqs_pkg_name]], sysreqs)
})


test_that_managed("Checking system requirements", {
  travis_ci_flag <- as.logical(Sys.getenv("TravisCI"))
  appveyor_ci_flag <- as.logical(Sys.getenv("APPVEYOR_CI"))
  is_windows <- .Platform$OS.type == "windows"
  skip_if(is.na(travis_ci_flag) || is.na(appveyor_ci_flag) || is_windows)
  skip_if_not(travis_ci_flag || appveyor_ci_flag)
  
  prj <- init_test_project(repo_adapters = c("Dir")) 
  params <- prj$load_params()
  
  # create package with system requirements
  sysreqs <- "This package has some system requirements."
  sysreqs_pkg_name <- "PackageWithSystemRequirements"
  
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  deploy_package_to_lrepo(pkg_file = "png_0.1-7.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj = prj, deps = "png")
  
  expect_error(RSuite::sysreqs_check(prj = prj))
  
  RSuite::sysreqs_install(prj = prj)
  
  expect_silent(RSuite::sysreqs_check(prj = prj))
})


test_that_managed("Checking script creation", {
  travis_ci_flag <- as.logical(Sys.getenv("TravisCI"))
  appveyor_ci_flag <- as.logical(Sys.getenv("APPVEYOR_CI"))
  skip_if(is.na(travis_ci_flag) || is.na(appveyor_ci_flag))
  skip_if_not(travis_ci_flag || appveyor_ci_flag)
  
  prj <- init_test_project(repo_adapters = c("Dir")) 
  params <- prj$load_params()
  
  # create package with system requirements
  sysreqs <- "This package has some system requirements."
  sysreqs_pkg_name <- "PackageWithSystemRequirements"
  
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  deploy_package_to_lrepo(pkg_file = "png_0.1-7.tar.gz", prj = prj, type = "source")
  create_test_package("TestPackage", prj = prj, deps = "png")
  
  RSuite::sysreqs_script(prj = prj)
  
  install_script <- ifelse(.Platform$OS.type == "windows", "sysreqs_install.cmd", "sysreqs_install.sh")
  shell(file.path(get_wspace_dir(), "TestProject", install_script))
  
  expect_silent(RSuite::sysreqs_check(prj = prj))
})
