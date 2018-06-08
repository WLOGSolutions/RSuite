#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(testthat)

source("R/test_utils.R")

context("Testing if proj command works properly")

test_that_managed("Test project creation", {
  retcode <- create_test_project('TestProject')
  expect(retcode == 0, sprintf("Failed to create TestProject(errcode: %s)", retcode))

  prj_path <- file.path(get_wspace_dir(), "TestProject")
  retcode <- rsuite_run(args = c("proj", "pkgadd", "-v", "-n", "TestPackage", "--skip_rc"),
                        wd = prj_path)
  expect(retcode == 0, sprintf("Failed to create package in TestProject(errcode: %s)", retcode))
})

test_that_managed("Test project cleaning", {
  pkg_path <- create_test_package("TestProject", "TestPackage")
  prj_path <- dirname(dirname(pkg_path))
  setup_package(pkg_path, Depends = "logging, colorspace")

  retcode <- rsuite_run(args = c("proj", "depsinst", "-v", "-c"), wd = prj_path)
  expect(retcode == 0, "Project deps installing failed")
  expect_that_packages_installed(c("logging", "colorspace"), prj_path)

  setup_package(pkg_path, Depends = "logging")

  retcode <- rsuite_run(args = c("proj", "depsclean", "-v"), wd = prj_path)
  expect(retcode == 0, "Project deps cleaning failed")
  expect_that_packages_installed("logging", prj_path)

})

test_that_managed("Test project building", {
  pkg_path <- create_test_package("TestProject", "TestPackage")
  prj_path <- dirname(dirname(pkg_path))

  retcode <- rsuite_run(args = c("proj", "depsinst", "-v", "-c"), wd = prj_path)
  expect(retcode == 0, "Project deps installing failed")
  expect_that_packages_installed(c("logging"), prj_path)

  retcode <- rsuite_run(args = c("proj", "build", "-v"), wd = prj_path)
  expect(retcode == 0, "Project deps installing failed")

  expect_that_packages_installed(c("logging", "TestPackage"), prj_path)
})

test_that_managed("Test project zipping", {
  pkg_path <- create_test_package("TestProject", "TestPackage")
  prj_path <- dirname(dirname(pkg_path))

  retcode <- rsuite_run(args = c("proj", "depsinst", "-v", "-c"), wd = prj_path)
  expect(retcode == 0, "Project deps installing failed")
  expect_that_packages_installed(c("logging"), prj_path)

  retcode <- rsuite_run(args = c("proj", "zip", "-v", "-p", prj_path, "--version=1.0"), wd = prj_path)
  expect(retcode == 0, "Project deps installing failed")
  expect_that_packages_installed(c("logging", "TestPackage"), prj_path)

  expect_true(file.exists(file.path(prj_path, "TestProject_1.0x.zip")))
})


test_that_managed("Test project locking", {
  pkg_path <- create_test_package("TestProject", "TestPackage")
  prj_path <- dirname(dirname(pkg_path))

  retcode <- rsuite_run(args = c("proj", "lock"), wd = prj_path)
  expect(retcode == 0, "Project lock failed")
  expect_true(file.exists(file.path(prj_path, "deployment/env.lock")))
})


test_that_managed("Test project unlocking", {
  pkg_path <- create_test_package("TestProject", "TestPackage")
  prj_path <- dirname(dirname(pkg_path))

  retcode <- rsuite_run(args = c("proj", "lock"), wd = prj_path)
  expect(retcode == 0, "Project lock failed")
  expect_true(file.exists(file.path(prj_path, "deployment/env.lock")))

  retcode <- rsuite_run(args = c("proj", "unlock"), wd = prj_path)
  expect(retcode == 0, "Project unlock failed")
  expect_false(file.exists(file.path(prj_path, "deployment/env.lock")))
})
