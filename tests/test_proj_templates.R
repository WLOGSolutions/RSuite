#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/template_managment.R")
source("R/project_management.R")

context("Testing if creation of project/package using templates works properly")

test_that_managed("Project creation from builtin template", {
  # create test project using the builtin template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin")
  params <- prj$load_params()

  # create vector containing all files from the builtin project template
  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "deployment/libs",
    "deployment/sbox",
    "logs",
    "TestProject.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/TestProject_Tests.Rproj",
    "tests/.Rprofile"
  )

  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% prj_files))
})

test_that_managed("Package creation from builtin template", {
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin")
  params <- prj$load_params()

  # create test package using the builtin template
  create_test_package("TestPackage", prj = prj, tmpl = "builtin")

  # create vector containing all files from the builtin package template
  expected_files <- c(
    "TestPackage.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R"
  )

  # retrieve all files from the created package
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"),
                          all.files = TRUE, recursive = TRUE,
                          include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% pkg_files))
})



test_that_managed("Project creation using custom template defined in user's local path", {
  # create project template not
  create_prj_test_template(name = "TestTemplate")

  # create project using custom template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "TestTemplate")

  params <- prj$load_params()

  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "deployment/libs",
    "deployment/sbox",
    "logs",
    "TestProject.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/TestProject_Tests.Rproj",
    "tests/.Rprofile"
  )

  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% prj_files))
})



test_that_managed("Project creation using custom template (path as argument)", {
  # create project template not
  create_prj_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "project")

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "__ProjectName__.txt"))

  # create project using custom template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = tmpl_path)

  params <- prj$load_params()

  expected_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "deployment/libs",
    "deployment/sbox",
    "logs",
    "TestProject.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/TestProject_Tests.Rproj",
    "tests/.Rprofile",
    "TestProject.txt"
  )

  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% prj_files))
})



test_that_managed("Package creation using custom template defined in user's local directory", {
  # create project template not
  create_pkg_test_template(name = "TestTemplate")
  
  # create package using custom template
  prj <- init_test_project(repo_adapters = c("Dir"))
  create_test_package("TestPackage", prj = prj, tmpl = "TestTemplate")


  expected_files <- c(
    "TestPackage.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R"
  )

  # retrieve all files from the created package
  params <- prj$load_params()
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"),
                          all.files = TRUE, recursive = TRUE,
                          include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% pkg_files))
})



test_that_managed("Package creation using custom template (path as argument)", {
  # create project template not
  create_pkg_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "__PackageName__.txt"))

  # create package using custom template
  prj <- init_test_project(repo_adapters = c("Dir"))
  create_test_package("TestPackage", prj = prj, tmpl = tmpl_path)


  expected_files <- c(
    "TestPackage.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R",
    "TestPackage.txt"
  )

  # retrieve all files from the created package
  params <- prj$load_params()
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"),
                          all.files = TRUE, recursive = TRUE,
                          include.dirs = TRUE, no.. = TRUE)

  expect_true(all(expected_files %in% pkg_files))
})



test_that_managed("Project creation using template not containing the PARAMETERS file", {
  # create project template
  create_prj_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "project")
  unlink(file.path(tmpl_path, "PARAMETERS"))

  expect_log_message(init_test_project(name = "TestProject", tmpl = tmpl_path),
               regexp = "does not contain required files: PARAMETERS")
})


test_that_managed("Package creation using template not containing the DESCRIPTION file", {
  # create package template
  create_pkg_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate", "package")
  unlink(file.path(tmpl_path, "DESCRIPTION"))

  prj <- init_test_project(name = "TestProject")

  expect_log_message(create_test_package(name = "TestPackage",
                                   prj = prj, tmpl = tmpl_path),
               regexp = "does not contain required files: DESCRIPTION")
})



test_that_managed("Template priority during project/package creation", {
  # create custom builtin template
  wspace_dir <- get_wspace_dir()
  create_prj_test_template(name = "builtin", path = wspace_dir)
  success <- file.create(file.path(wspace_dir, "builtin", "project", "prj_builtin.txt"))
  assert(success, "%s failed to create file in project template")
  
  create_pkg_test_template(name = "builtin", path = wspace_dir)
  success <- file.create(file.path(wspace_dir, "builtin", "package", "pkg_builtin.txt"))
  assert(success, "%s failed to create file in project template")
  
  # register created custom builtin template
  RSuite::tmpl_register(path = file.path(wspace_dir, "builtin"))
  
  # create project using custom template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = "builtin")
  create_test_package("TestPackage", prj = prj, tmpl = "builtin")
  params <- prj$load_params()

  # create vector containing all files from the builtin project template
  expected_prj_files <- c(
    ".Rprofile",
    "config_templ.txt",
    "deployment/libs",
    "deployment/sbox",
    "logs",
    "TestProject.Rproj",
    "packages",
    "PARAMETERS",
    "R/.Rprofile",
    "R/master.R",
    "R/set_env.R",
    "tests/TestProject_Tests.Rproj",
    "tests/.Rprofile",
    "prj_builtin.txt"
  )
  
  prj_files <- list.files(params$prj_path, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE, no.. = TRUE)
  pkg_files <- list.files(file.path(params$pkgs_path, "TestPackage"),
                          all.files = TRUE, recursive = TRUE,
                          include.dirs = TRUE, no.. = TRUE)
  expected_pkg_files <- c(
    "TestPackage.Rproj",
    ".Rprofile",
    "DESCRIPTION",
    "NAMESPACE",
    "NEWS",
    "R",
    "R/package_logger.R",
    "R/package_validation.R",
    "R/packages_import.R",
    "pkg_builtin.txt"
  )
  
  expect_true(all(expected_prj_files %in% prj_files))
  expect_true(all(expected_pkg_files %in% pkg_files))
})
