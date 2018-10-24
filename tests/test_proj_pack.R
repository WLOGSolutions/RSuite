#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2018, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if project source pack creation works properly [test_proj_pack]")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

test_that_managed("Project source pack creation under Jenkins CI", {
  # create test project
  prj_path <- file.path(get_wspace_dir(), "TestProject")
  prj <- init_test_project(skip_rc = TRUE)
  create_test_package("TestPackage", prj = prj, ver = "0.1")
  RSuite::prj_install_deps(prj = prj)

  # emulate Jenkins job: set JOB_NAME and BUILD_NUMBER environment variables
  Sys.setenv(JOB_NAME = "TestProjectJob", BUILD_NUMBER = "7")
  on_test_exit(function() {
    Sys.unsetenv(c("JOB_NAME", "BUILD_NUMBER"))
  })

  # create source pack (it should detect BUILD_NUMBER from environment variable)
  RSuite::prj_pack(prj = prj, path = prj_path)

  expect_true(file.exists(file.path(prj_path, "prjpack_TestProject_0.1_7.zip")))
})
