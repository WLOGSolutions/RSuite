#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if deployment zip creation works properly [test_proj_zip]")

library(RSuite)
library(testthat)
library(git2r)

source("R/test_utils.R")
source("R/project_management.R")

test_that_managed("Deployment zip of project under Git control", {
  # initialize repository
  prj_path <- file.path(get_wspace_dir(), "TestProject")
  dir.create(prj_path)
  repo <- git2r::init(prj_path)
  
  # create project
  prj <- init_test_project()
  create_test_package("TestPackage", prj = prj, ver = "0.1")
  RSuite::prj_install_deps(prj = prj)
  
  oldwd <- getwd()
  setwd(prj_path)
  on_test_exit(function() {
    setwd(oldwd)
  })
  
  # commit all changes
  git2r::config(user.name = "Alice", user.email = "alice@example.org")
  git2r::add(path = "*")
  git2r::commit(message = "init")
  git2r::tag(name = "001", message = "test tag")
  
  # create zip file (it should detect the tag)
  RSuite::prj_zip(prj = prj, get_wspace_dir())
  
  expect_true(file.exists(file.path(get_wspace_dir(), "TestProject_0.1_001.zip")))
})