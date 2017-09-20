#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(testthat)

source("R/test_utils.R")

context("Testing if repo command works properly")

test_that_managed("Test repo project pkg upload", {
  pkg_path <- create_test_package("TestProject", "TestPackage")
  prj_path <- dirname(dirname(pkg_path))
  
  retcode <- rsuite_run(args = c("proj", "depsinst", "-v"), wd = prj_path)
  expect(retcode == 0, "Project deps installing failed")
  expect_that_packages_installed(c("logging"), prj_path)
  
  repo_path <- create_test_repo("repo")
  retcode <- rsuite_run(args = c("repo", "addproj", "-v", "-d", repo_path, "-b", "FALSE", "--skip_rc"), 
                        wd = prj_path)
  expect(retcode == 0, sprintf("Project uploading to %s failed", repo_path))
  expect_that_repo_contains(c("TestPackage"), repo_path, "source")
})

test_that_managed("Test repo package file upload", {
  repo_path <- create_test_repo("repo")
  retcode <- rsuite_run(args = c("repo", "addfile", "-v", "-d", repo_path, 
                                 "-f", file.path(get_data_dir(), "logging_0.7-103.tar.gz")), 
                        wd = get_wspace_dir())
  expect(retcode == 0, sprintf("Project uploading to %s failed", repo_path))
  expect_that_repo_contains(c("logging"), repo_path, "source")
})

test_that_managed("Test repo package file upload", {
  repo_path <- create_test_repo("repo")
  retcode <- rsuite_run(args = c("repo", "addext", "-v", "-d", repo_path, "-n", "colorspace"),
                        wd = get_wspace_dir())
  expect(retcode == 0, sprintf("Project uploading to %s failed", repo_path))
  expect_that_repo_contains(c("colorspace"), repo_path, .Platform$pkgType)
})

test_that_managed("Test repo PKGZIP upload", {
  wspace_dir <- get_wspace_dir()
  retcode <- rsuite_run(args = c("pkgzip", "file", "-v",
                                 "-p", wspace_dir,
                                 "-f", file.path(get_data_dir(), "logging_0.7-103.tar.gz")),
                        wd = wspace_dir)
  expect(retcode == 0, "Failed to create PKGZIP")
  
  pkgzip_file <- file.path(wspace_dir, sprintf("%s_pkgzip_logging.zip", Sys.Date()))
  expect(file.exists(pkgzip_file), 
         sprintf("PKGZIP builded under unexpected name. (expected: %s)", pkgzip_file))
  
  repo_path <- create_test_repo("repo")
  retcode <- rsuite_run(args = c("repo", "addpkgzip", "-v", "-d", repo_path, 
                                 "-z", pkgzip_file),
                        wd = wspace_dir)
  expect(retcode == 0, sprintf("PKGZIP uploading to %s failed", repo_path))
  expect_that_repo_contains(c("logging"), repo_path, "source")
})

test_that_managed("Test repo GitHub upload", {
  wspace_dir <- get_wspace_dir()

  repo_path <- create_test_repo("repo")
  retcode <- rsuite_run(args = c("repo", "addgithub", "-v", "-d", repo_path, 
                                 "-r", "twitter/AnomalyDetection"),
                        wd = wspace_dir)
  expect(retcode == 0, sprintf("PKGZIP uploading to %s failed", repo_path))
  expect_that_repo_contains(c("AnomalyDetection"), repo_path, .Platform$pkgType)
})
