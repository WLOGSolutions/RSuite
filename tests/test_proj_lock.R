#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/project_management.R")
source("R/repo_management.R")

context("Testing if project environment locking works properly")

# create test project with prepared repo
invisible(capture.output(create_lock_test_prj()))

test_that_managed("Project environment lock file creation", {
   # Prepare project
   prj <- init_lock_test_prj()
   params <- prj$load_params()

   create_test_package(name = "TestPackage", prj = prj, deps = c("logging"))

   # install dependencies
   RSuite::prj_install_deps(prj)

   # Lock project environment
   RSuite::prj_lock_env(prj)
   lock_data <- read.dcf(params$lock_path)

   # Check if all installed packages where locked
   expect_true(file.exists(params$lock_path))
   expect_that_packages_locked(c(logging = "0.7-103"), params)
 })


test_that_managed("Locking environment with uninstalled direct dependencies", {
   # Prepare project
   prj <- init_lock_test_prj()
   params <- prj$load_params()

   create_test_package("TestPackage", prj, deps = c("logging"))

   # Try locking the project environment with uninstalled dependencies
   expect_error(RSuite::prj_lock_env(prj))
})


test_that_managed("Locked environment, no unfeasibles", {
   prj <- init_lock_test_prj()
   params <- prj$load_params()


   # Create package and install deps
   create_test_package("TestPackage", prj, deps = "TestDependencyToUpdate (== 1.0)")
   RSuite::prj_install_deps(prj)

   # Lock environment
   RSuite::prj_lock_env(prj)

   # Set dependency to make newer version available
   set_test_package_deps("TestPackage", prj = prj, deps = "TestDependencyToUpdate")
   RSuite::prj_install_deps(prj, clean = TRUE)

   expect_that_packages_installed(c("TestDependencyToUpdate", "logging"), prj, versions = c("1.0", "0.7-103"))
 })


test_that_managed("Locked environment, unfeasibles", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependencyToUpdate (== 1.0)")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Update dependency
  set_test_package_deps("TestPackage", prj = prj, deps = "TestDependencyToUpdate (>= 1.1)")

  # Expect warning message
  expect_log_message(RSuite::prj_install_deps(prj = prj, clean = TRUE, relock = TRUE),
                     regexp = "Following packages will be updated in lock requirements: TestDependency")
})


test_that_managed("Unlocking locked environment", {
   # Prepare project
   prj <- init_lock_test_prj()
   params <- prj$load_params()

   # install dependencies
   RSuite::prj_install_deps(prj)

   # Lock project environment
   RSuite::prj_lock_env(prj)

   # Unlock project environment
   RSuite::prj_unlock_env(prj)

   # Check if lock file was removed
   expect_false(file.exists(params$lock_path))
})


test_that_managed("Unlocking not unlock environment", {
   # Prepare project
   prj <- init_lock_test_prj()
   params <- prj$load_params()

   # Unlock project environment
   expect_log_message(RSuite::prj_unlock_env(prj),
                      regexp = "The project environment is not locked")
})


test_that_managed("Add new dependency, lock updating", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package and install deps
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)
  expect_that_packages_locked(c(logging = "0.7-103"), params)

  # Add new dependency
  create_test_package("TestPackage", prj, deps = "AddedTestDependency")

  # Install new dependencies (lock should be automatically updated)
  RSuite::prj_install_deps(prj)

  expect_that_packages_locked(c(logging = "0.7-103", AddedTestDependency = "1.0"),
                              params)
})


test_that_managed("Update dependency, no relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependencyToUpdate (== 1.0)")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Update dependency
  set_test_package_deps("TestPackage", prj = prj, deps = "TestDependencyToUpdate (>= 1.1)")

  # Expect error
  expect_error(prj_install_deps(prj))
})


test_that_managed("Update dependency, relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package and install deps
  create_test_package(name = "TestPackage", prj, deps = "TestDependencyToUpdate (== 1.0)")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Update dependency
  set_test_package_deps(name = "TestPackage", prj = prj, deps = "TestDependencyToUpdate (>= 1.1)")

  RSuite::prj_install_deps(prj = prj, relock = TRUE)

  # Check if lock was updated
  expect_that_packages_locked(c(logging = "0.7-103", TestDependencyToUpdate = "1.1"),
                              params)
})


test_that_managed("Remove dependency, no relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependencyToRemove")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Remove dependency
  remove_test_packages(prj)

  # Expect error
  expect_error(prj_install_deps(prj),
               regexp = "Following packages to be removed from lock requirements: TestDependency.")
})


test_that_managed("Remove dependency, relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependencyToRemove")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Remove dependency
  set_test_package_deps(name = "TestPackage", prj = prj, deps = "logging")

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103"), params)
})


test_that_managed("Add and Remove dependency, relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package with dependency to remove and install deps
  create_test_package("TestPackageWithDependencyToRemove", prj, deps = "TestDependencyToRemove")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add package with new dependency and remove old one
  set_test_package_deps("TestPackageWithDependencyToRemove", prj = prj, deps = "AddedTestDependency")

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)
  
  # Check updated lock
  expect_that_packages_locked(c(logging = "0.7-103", AddedTestDependency = "1.0"),
                              params)
})


test_that_managed("Add and Update dependency, relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package with dependency to update and install deps
  create_test_package("TestPackage", prj, deps = "TestDependencyToUpdate (== 1.0)")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add new dependency and update old one
  set_test_package_deps(name = "TestPackage", prj = prj,
                        deps = c("TestDependencyToUpdate (>= 1.1)",
                                 "AddedTestDependency"))

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103",
                                AddedTestDependency = "1.0",
                                TestDependencyToUpdate = "1.1"),
                              params)
})


test_that_managed("Remove and Update dependency, relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package with dependency to update
  create_test_package("TestPackage", prj, deps = c("TestDependencyToUpdate",
                                                   "TestDependencyToRemove"))

  # Intall dependencies
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Update/remove dependencies
  set_test_package_deps(name = "TestPackage", prj = prj, deps = c("TestDependencyToUpdate (>= 1.1)"))

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103", TestDependencyToUpdate = "1.1"),
                              params)
})


test_that_managed("Add, Remove and Update dependency, relocking", {
  prj <- init_lock_test_prj()
  params <- prj$load_params()

  # Create package with dependency to update
  create_test_package(name = "TestPackage", prj = prj,
                      deps = c("TestDependencyToUpdate (== 1.0)",
                               "TestDependencyToRemove"))

  # Intall dependencies
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add/Remove/Update dependencies
  set_test_package_deps(name = "TestPackage", prj = prj,
                        c("TestDependencyToUpdate (>= 1.1)",
                          "AddedTestDependency"))

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103",
                                AddedTestDependency = "1.0",
                                TestDependencyToUpdate = "1.1"),
                              params)
})
