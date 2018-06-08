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


test_that_managed("Project environment lock file creation", {
   # Prepare project
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   # Prepare repo
   deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
   create_test_package("TestPackage", prj, deps = c("logging"))

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
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   # Prepare repo
   deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
   create_test_package("TestPackage", prj, deps = c("logging"))

   # Try locking the project environment with uninstalled dependencies
   expect_error(RSuite::prj_lock_env(prj))
})


test_that_managed("Locked environment, no unfeasibles", {
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   # Prepare repo
   pkg_deps <- "TestDependency"
   deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
   create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.0")

   # Create package and install deps
   create_test_package("TestPackage", prj, deps = pkg_deps)
   RSuite::prj_install_deps(prj)

   # Lock environment
   RSuite::prj_lock_env(prj)

   # Add newer version and rebuild
   create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.1")
   RSuite::prj_install_deps(prj, clean = TRUE)

   expect_that_packages_installed(c("TestDependency", "logging"), prj, versions = c("1.0", "0.7-103"))
 })


test_that_managed("Locked environment, unfeasibles", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependency")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add newer version and rebuild
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")
  create_test_package("TestPackage2", prj, deps = "TestDependency (>= 1.1)")

  # Expect warning message
  expect_log_message(RSuite::prj_install_deps(prj = prj, clean = TRUE, relock = TRUE),
                     regexp = "Following packages will be updated in lock requirements: TestDependency")
})


test_that_managed("Unlocking locked environment", {
   # Prepare project
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   # Prepare repo
   deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
   create_test_package("TestPackage", prj, deps = c("logging"))

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
   prj <- init_test_project(repo_adapters = c("Dir"))
   params <- prj$load_params()

   # Unlock project environment
   expect_log_message(RSuite::prj_unlock_env(prj),
                      regexp = "The project environment is not locked")
})


test_that_managed("Add new dependency, lock updating", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")

  # Create package and install deps
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)
  expect_that_packages_locked(c(logging = "0.7-103"), params)

  # Add new dependency
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")
  create_test_package("TestPackage", prj, deps = "TestDependency")

  # Install new dependencies (lock should be automatically updated)
  RSuite::prj_install_deps(prj)

  expect_that_packages_locked(c(logging = "0.7-103", TestDependency = "1.0"),
                              params)
})


test_that_managed("Udpate dependency, no relocking", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  pkg_deps <- "TestDependency"
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.0")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = pkg_deps)
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Update dependency
  create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.1")
  create_test_package("TestPackage2", prj, deps = "TestDependency (>= 1.1)")

  # Expect error
  expect_error(prj_install_deps(prj))
})


test_that_managed("Udpate dependency, relocking", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.0")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = "TestDependency")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Update dependency
  create_package_deploy_to_lrepo(name = "TestDependency", prj = prj, ver = "1.1")
  create_test_package("TestPackage2", prj, deps = "TestDependency (>= 1.1)")

  RSuite::prj_install_deps(prj = prj, relock = TRUE)

  # Check if lock was updated
  expect_that_packages_locked(c(logging = "0.7-103", TestDependency = "1.1"),
                              params)
})


test_that_managed("Remove dependency, no relocking", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  pkg_deps <- "TestDependency"
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.0")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = pkg_deps)
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
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  pkg_deps <- "TestDependency"
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = pkg_deps, prj = prj, ver = "1.0")

  # Create package and install deps
  create_test_package("TestPackage", prj, deps = pkg_deps)
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Remove dependency
  remove_test_packages(prj)
  create_test_package("TestPackage", prj, deps = "logging")

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103"), params)
})


test_that_managed("Add and Remove dependency, relocking", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  added_pkg_deps <- "AddedTestDependency"
  pkg_deps_to_remove <- "TestDependencyToRemove"
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = added_pkg_deps, prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = pkg_deps_to_remove, prj = prj, ver = "1.0")

  # Create package with dependency to remove and install deps
  create_test_package("TestPackageWithDependencyToRemove", prj, deps = pkg_deps_to_remove)
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Remove dependency
  remove_test_packages(prj)

  # Add package with new dependency
  create_test_package("TestPackageWithNewDependency", prj, deps = added_pkg_deps)

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)
  # Check updated lock
  expect_that_packages_locked(c(logging = "0.7-103", AddedTestDependency = "1.0"),
                              params)
})


test_that_managed("Add and Update dependency, relocking", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "AddedTestDependency", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependencyToUpdate", prj = prj, ver = "1.0")

  # Create package with dependency to update and install deps
  create_test_package("TestPackageWithBaseDependency", prj, deps = "TestDependencyToUpdate")
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add package with new dependency
  create_test_package("TestPackageWithNewDependency", prj, deps = "AddedTestDependency")

  # Add package with updated dependency
  create_package_deploy_to_lrepo(name = "TestDependencyToUpdate", prj = prj, ver = "1.1")
  create_test_package("TestPackageWithUpdatedDependency", prj, deps = "TestDependencyToUpdate (>= 1.1)")

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
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "TestDependencyToRemove", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependencyToUpdate", prj = prj, ver = "1.0")

  # Create package with dependency to update
  create_test_package("TestPackageWithBaseDependency", prj, deps = "TestDependencyToUpdate")
  create_test_package("TestPackageWithDependencyToRemove", prj, deps = "TestDependencyToRemove")

  # Intall dependencies
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add package with updated dependency
  create_package_deploy_to_lrepo(name = "TestDependencyToUpdate", prj = prj, ver = "1.1")

  # Remove dependencies
  remove_test_packages(prj)

  create_test_package("TestPackageWithUpdatedDependency", prj, deps = "TestDependencyToUpdate (>= 1.1)")

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103", TestDependencyToUpdate = "1.1"),
                              params)
})


test_that_managed("Add, Remove and Update dependency, relocking", {
  prj <- init_test_project(repo_adapters = c("Dir"))
  params <- prj$load_params()

  # Prepare repo
  deploy_package_to_lrepo(pkg_file = "logging_0.7-103.tar.gz", prj = prj, type = "source")
  create_package_deploy_to_lrepo(name = "AddedTestDependency", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependencyToRemove", prj = prj, ver = "1.0")
  create_package_deploy_to_lrepo(name = "TestDependencyToUpdate", prj = prj, ver = "1.0")

  # Create package with dependency to update
  create_test_package("TestPackageWithBaseDependency", prj, deps = "TestDependencyToUpdate")
  create_test_package("TestPackageWithDependencyToRemove", prj, deps = "TestDependencyToRemove")

  # Intall dependencies
  RSuite::prj_install_deps(prj)

  # Lock environment
  RSuite::prj_lock_env(prj)

  # Add package with updated dependency
  create_package_deploy_to_lrepo(name = "TestDependencyToUpdate", prj = prj, ver = "1.1")

  # Remove dependencies
  remove_test_packages(prj)

  create_test_package("TestPackageWithUpdatedDependency", prj, deps = "TestDependencyToUpdate (>= 1.1)")

  # Add package with new dependency
  create_test_package("TestPackageWithNewDependency", prj, deps = "AddedTestDependency")

  # Update lock
  RSuite::prj_install_deps(prj, relock = TRUE)

  # Check updated lock
  lock_data <- as.data.frame(read.dcf(params$lock_path))
  expect_that_packages_locked(c(logging = "0.7-103",
                                AddedTestDependency = "1.0",
                                TestDependencyToUpdate = "1.1"),
                              params)
})
