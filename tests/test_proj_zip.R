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
  wspace_dir <- get_wspace_dir()
  prj_path <- file.path(wspace_dir, "TestProject")
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
  RSuite::prj_zip(prj = prj, prj_path)

  expect_true(file.exists(file.path(prj_path, "TestProject_0.1_001.zip")))
})

test_that_managed("Deployment zip of project under SVN control", {
  # prepare svn repository
  wspace_dir <- get_wspace_dir()
  repo_path <- file.path(wspace_dir, "repo")
  RSuite:::get_cmd_outlines("SVN: Creating repository",
                            sprintf("svnadmin create %s", repo_path),
                            log_debug = TRUE)
  on_test_exit(function() {
    unlink(repo_path, force = TRUE, recursive = TRUE)
  })
  repo_url <- sprintf("file:///%s", RSuite:::rsuite_fullUnifiedPath(repo_path))

  # create project directory and put it under SVN
  prj_path <- file.path(wspace_dir, "TestProject")
  dir.create(prj_path)
  RSuite:::get_cmd_outlines("SVN: Checkout",
                            sprintf("svn checkout %s %s", repo_url, prj_path),
                            log_debug = TRUE)

  # create project
  prj <- init_test_project(skip_rc = FALSE)
  create_test_package("TestPackage", prj = prj, ver = "0.1", skip_rc = F)
  RSuite::prj_install_deps(prj = prj)

  oldwd <- getwd()
  setwd(prj_path)
  on_test_exit(function() {
    setwd(oldwd)
  })

  # commit all changes
  RSuite:::get_cmd_outlines("SVN: Commit",
                            sprintf("svn ci -m 'Init'"),
                            log_debug = TRUE)

  RSuite:::get_cmd_outlines("SVN: Update",
                            sprintf("svn update"),
                            log_debug = TRUE)

  # create zip file (it should detect the tag)
  RSuite::prj_zip(prj = prj, prj_path)

  expect_true(file.exists(file.path(prj_path, "TestProject_0.1_1.zip")))
})
