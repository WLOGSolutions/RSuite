#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")

context("Testing if project structure update works properly")

test_that_managed("Updating old project deployment switch", {
  wspace_dir <- get_wspace_dir()
  unzip(zipfile = file.path(get_data_dir(), .Platform$OS.type, "TestRepo_rsuite_vNone.zip"),
        exdir = wspace_dir)
  
  proj_dir <- file.path(wspace_dir, "TestRepo")
  on_test_exit(function() {
    unlink(proj_dir, recursive = T, force = T)
  })
  exp_path <- get_repo_path(proj_dir) # upgrades repository so prj_start can work

  RSuite::prj_start(name = "TestRepo", path = wspace_dir)
    
  cur_path <- get_repo_path(file.path(proj_dir, "deployment"))
  expect_equal(cur_path, exp_path)
})
