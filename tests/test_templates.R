#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")

context("Testing if project/templates work properly")


test_that_managed("Project creation from builtin template", {
  # create test project using the builtin template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin") 
  params <- prj$load_params()
  
  # retrieve all files from the builtin project template and replace markers 
  prj_tmpl_dir <- system.file(file.path("extdata", "prj_template"), package = "RSuite")
  expected_files <- list.files(prj_tmpl_dir, all.files = TRUE,
                      recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  expected_files <- gsub("__ProjectName__", "TestProject", expected_files)
  
  # retrieve all files from the created package 
  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  
  expect_true(all(expected_files %in% prj_files))
})


test_that_managed("Package creation from builtin template", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin") 
  params <- prj$load_params()
  
  # create test package using the builtin template
  create_test_package("TestPackage", prj = prj, tmpl = "builtin")
  
  # retrieve all files from the builtin package template and replace markers 
  pkg_tmpl_dir <- system.file(file.path("extdata", "pkg_template"), package = "RSuite")
  expected_files <- list.files(pkg_tmpl_dir, all.files = TRUE,
                      recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  expected_files <- gsub("__PackageName__", "TestPackage", expected_files)
  
  # retrieve all files from the created package 
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"), all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  
  expect_true(all(expected_files %in% pkg_files))
})


test_that_managed("Project creation using template defined in default dir", {
    
})