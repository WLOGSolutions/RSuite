#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if .libPaths is preserved after project/env build [test_libpaths_preserved]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/libpaths_management.R")


test_that_managed("Testing if .libPaths is preserved after env build", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  expected_libPaths <- add_extra_to_libpath()

  prj_install_deps(prj)

  current_libPaths <- .libPaths()
  expect(setequal(current_libPaths, expected_libPaths),
         sprintf(".libPaths changed after proj_install_deps\nwas: %s\nis:  %s",
                 paste(expected_libPaths, collapse = ", "),
                 paste(current_libPaths, collapse = ", ")))

})

test_that_managed("Testing if .libPaths is preserved after env build", {
  prj <- init_test_project(repo_adapters = c("Dir")) # uses BaseTestProjectTemplate with logging 0.7-103

  prj_install_deps(prj)

  expected_libPaths <- add_extra_to_libpath()

  prj_build(prj)

  current_libPaths <- .libPaths()
  expect(setequal(current_libPaths, expected_libPaths),
         sprintf(".libPaths changed after proj_build\nwas: %s\nis:  %s",
                 paste(expected_libPaths, collapse = ", "),
                 paste(current_libPaths, collapse = ", ")))

})
