#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if .libPaths is preserved after project/env build")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/libpaths_management.R")


test_that_managed("Testing if .libPaths is preserved after env build", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  expected_libPaths <- add_extra_to_libpath()

  prj_install_deps(prj)

  current_libPaths <- .libPaths()
  expect(setequal(current_libPaths, expected_libPaths),
         sprintf(".libPaths changed after proj_install_deps\nwas: %s\nis:  %s",
                 paste(expected_libPaths, collapse = ", "),
                 paste(current_libPaths, collapse = ", ")))

})

test_that_managed("Testing if .libPaths is preserved after env build", {
  prj <- init_test_project(repo_adapters = c("Dir"))

  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  prj_install_deps(prj)

  expected_libPaths <- add_extra_to_libpath()

  prj_build(prj)

  current_libPaths <- .libPaths()
  expect(setequal(current_libPaths, expected_libPaths),
         sprintf(".libPaths changed after proj_build\nwas: %s\nis:  %s",
                 paste(expected_libPaths, collapse = ", "),
                 paste(current_libPaths, collapse = ", ")))

})
