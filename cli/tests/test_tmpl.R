#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
library(testthat)

context("Testing if repo command works properly")

source("R/test_utils.R")

test_that_template("Test template creation - various options", {
  wspace_dir <- get_wspace_dir()
  
  # create complete template without specyfing --pkg and --prj options
  retcode <- rsuite_run(args = c("tmpl", "start", "-n", "TestTemplate1"), wd = wspace_dir)
  expect(retcode == 0, sprintf("Failed to create template in wspace directory(errcode %s)"),
         retcode)
  expect_true(dir.exists(file.path(wspace_dir, "TestTemplate1", "project"))) 
  expect_true(dir.exists(file.path(wspace_dir, "TestTemplate1", "package"))) 
  
  # create complete template when specyfing --pkg and --prj options
  retcode <- rsuite_run(args = c("tmpl", "start", "-n", "TestTemplate2", "--prj", "--pkg"), wd = wspace_dir)
  expect(retcode == 0, sprintf("Failed to create template in wspace directory(errcode %s)"),
         retcode)
  expect_true(dir.exists(file.path(wspace_dir, "TestTemplate2", "project"))) 
  expect_true(dir.exists(file.path(wspace_dir, "TestTemplate2", "package"))) 
  
  # create template containing only the project directory
  retcode <- rsuite_run(args = c("tmpl", "start", "-n", "TestTemplate3", "--prj"), wd = wspace_dir)
  expect(retcode == 0, sprintf("Failed to create template in wspace directory(errcode %s)"),
         retcode)
  expect_true(dir.exists(file.path(wspace_dir, "TestTemplate3", "project"))) 
  expect_false(dir.exists(file.path(wspace_dir, "TestTemplate3", "package"))) 
  
  # create template containing only the project directory
  retcode <- rsuite_run(args = c("tmpl", "start", "-n", "TestTemplate4", "--pkg"), wd = wspace_dir)
  expect(retcode == 0, sprintf("Failed to create template in wspace directory(errcode %s)"),
         retcode)
  expect_false(dir.exists(file.path(wspace_dir, "TestTemplate4", "project"))) 
  expect_true(dir.exists(file.path(wspace_dir, "TestTemplate4", "package"))) 
})
