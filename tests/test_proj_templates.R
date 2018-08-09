#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#----------------------------------------------------------------------------
context("Testing if creation of project/package using templates works properly")

library(RSuite)
library(testthat)

source("R/test_utils.R")
source("R/template_managment.R")
source("R/project_management.R")


test_that_template("Project creation from builtin template", {
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

test_that_template("Package creation from builtin template", {
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



test_that_template("Project creation using custom template defined in user's local path", {
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



test_that_template("Project creation using custom template (path as argument)", {
  # create project template not
  create_prj_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate")

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "project", "__ProjectName__.txt"))

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



test_that_template("Package creation using custom template defined in user's local directory", {
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



test_that_template("Package creation using custom template (path as argument)", {
  # create project template not
  create_pkg_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate")


  # check if template contains files from default template
  file.create(file.path(tmpl_path, "package", "__PackageName__.txt"))

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



test_that_template("Project creation using template not containing the PARAMETERS file", {
  # create project template
  create_prj_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate")
  unlink(file.path(tmpl_path, "project", "PARAMETERS"))

  expect_log_message(init_test_project(name = "TestProject", tmpl = tmpl_path),
               regexp = "does not contain required files: PARAMETERS")
})


test_that_template("Package creation using template not containing the DESCRIPTION file", {
  # create package template
  create_pkg_test_template(name = "TestTemplate")

  # remove required PARAMETERS file
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate")
  unlink(file.path(tmpl_path, "package", "DESCRIPTION"))

  prj <- init_test_project(name = "TestProject")

  expect_log_message(create_test_package(name = "TestPackage",
                                   prj = prj, tmpl = tmpl_path),
               regexp = "does not contain required files: DESCRIPTION")
})



test_that_template("Template priority during project/package creation", {
  # create custom builtin template
  wspace_dir <- get_wspace_dir()
  create_prj_test_template(name = "builtin", path = wspace_dir)
  success <- file.create(file.path(wspace_dir, "builtin", "project", "prj_builtin.txt"))
  stopifnot(success)

  create_pkg_test_template(name = "builtin", path = wspace_dir)
  success <- file.create(file.path(wspace_dir, "builtin", "package", "pkg_builtin.txt"))
  stopifnot(success)

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
    "tests/.Rprofile"
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
    "R/packages_import.R"
  )

  expect_true(all(expected_prj_files %in% prj_files))
  expect_true(all(expected_pkg_files %in% pkg_files))
})



test_that_template("Overwriting existing project files", {
  # create project template not
  create_prj_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate")

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "TestProjectInfo.txt"))

  # create project using custom template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = tmpl_path)
  params <- prj$load_params()

  # add changes to TestProjectInfo.txt file in the created project
  changed_file_path <- file.path(params$prj_path, "TestProjectInfo.txt")
  file_connection <- file(changed_file_path)
  new_file_content <- "My very important changes."
  writeLines(new_file_content, file_connection)
  close(file_connection)

  # create project again and check if files got overwritten
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = tmpl_path)
  file_connection <- file(changed_file_path)
  line <- readLines(file_connection, n = 1)
  close(file_connection)

  expect_equal(line, new_file_content)
})

test_that_template("Overwriting existing project files while renaming", {
  # create project template not
  create_prj_test_template(name = "TestTemplate")
  tmpl_path <- file.path(get_wspace_template_dir(), "TestTemplate")

  # check if template contains files from default template
  file.create(file.path(tmpl_path, "__ProjectName__Info.txt"))

  # create project using custom template
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = tmpl_path)
  params <- prj$load_params()

  # add changes to TestProjectInfo.txt file in the created project
  changed_file_path <- file.path(params$prj_path, "TestProjectInfo.txt")
  file_connection <- file(changed_file_path)
  new_file_content <- "My very important changes."
  writeLines(new_file_content, file_connection)
  close(file_connection)

  # create project again and check if files got overwritten
  prj <- init_test_project(repo_adapters = c("Dir"), tmpl = tmpl_path)
  file_connection <- file(changed_file_path)
  line <- readLines(file_connection, n = 1)
  close(file_connection)

  expect_equal(line, new_file_content)
})


test_that_template("Project creation from empty template", {
  wspace_dir <- get_wspace_dir()
  minimal_tmpl_path <- file.path(wspace_dir, "minimal")

  # create empty directory
  dir.create(file.path(minimal_tmpl_path, "project"), recursive = TRUE)

  expect_silent(init_test_project(tmpl = minimal_tmpl_path))
})


test_that_template("Package creation from empty template", {
  wspace_dir <- get_wspace_dir()
  minimal_tmpl_path <- file.path(wspace_dir, "minimal")

  # create empty directory
  dir.create(file.path(minimal_tmpl_path, "package"), recursive = TRUE)

  prj <- init_test_project()
  expect_silent(create_test_package("TestPackage", prj = prj, tmpl = minimal_tmpl_path))
})
