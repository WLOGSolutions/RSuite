#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'proj' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_mgr.R"), chdir = T)

sub_commands <- list(
  start = list(
    help = "Start new project or update structure of existing one.",
    options = list(
      make_option(c("-n", "--name"), dest = "name",
                  help="Name of the project to create. New project will be created in current directory. (required)"),
      make_option(c("--skip_rc"), dest = "skip_rc", action="store_true", default=FALSE,
                  help="Do not put newly created project under RC (default: %default)")
    ),
    run = function(opts) {
      if (is.null(opts$name) || is.na(opts$name)) {
        stop("Project name is required. Provide --name argument.")
      }
      RSuite::prj_start(name = opts$name, skip_rc = opts$skip_rc)
    }
  ),
  pkgadd = list(
    help = "Add package to the project.",
    options = list(
      make_option(c("-n", "--name"), dest = "name",
                  help="Name of the package to create. New package will be created in current R project. (required)"),
      make_option(c("--skip_rc"), dest = "skip_rc", action="store_true", default=FALSE,
                  help="Do not put newly created package under RC (default: %default)")
    ),
    run = function(opts) {
      if (is.na(opts$name)) {
        stop("Package name is required. Provide --name argument.")
      }
      RSuite::prj_start_package(name = opts$name, skip_rc = opts$skip_rc)
    }
  ),
  depsinst = list(
    help = "Install required dependencies into project local environment.",
    options = list(
      make_option(c("-c", "--clean"), dest = "clean", action="store_true", default=FALSE,
                  help="Clean local project environment before installing dependencies (default: %default)")
    ),
    run = function(opts) {
      RSuite::prj_install_deps(clean = opts$clean)
    }
  ),
  build = list(
    help = "Build the project.",
    options = list(),
    run = function(opts) {
      RSuite::prj_build()
    }
  ),
  test = list(
    help = "Run tests in tests folder.",
    options = list(
      make_option(c("-d", "--dir"), dest = "tests_dir",
                  help="Folder name relative to project base directory to look for tests. (default: tests)")
    ),
    run = function(opts) {
      if (is.null(opts$tests_dir) || is.na(opts$tests_dir)) {
        opts$tests_dir <- "tests"
      }

      prj <- RSuite::prj_init()
      tests_path <- file.path(prj$path, opts$tests_dir)
      if (!dir.exists(tests_path)) {
        stop(sprintf("Tests folder %s does not exists. Tests cannot be run.", tests_path))
      }
      RSuite::prj_load(prj = prj)
      test_res <- testthat::test_dir(tests_path)
      if (!testthat:::all_passed(test_res)) {
        stop('Tests failed')
      }
    }
  ),
  depsclean = list(
    help = "Uninstall unused dependencies from project local environment.",
    options = list(),
    run = function(opts) {
      RSuite::prj_clean_deps()
    }
  ),
  zip = list(
    help = "Build project deployment zip package.",
    options = list(
      make_option(c("-p", "--path"), dest = "path",
                  help="Directory to put built zip package into (default: current directory)"),
      make_option(c("--version"), dest = "version",
                  help="Version to use for zip package tagging (default: use ZipVersion form PARAMETERS and revision from RC)")
    ),
    run = function(opts) {
      if (is.null(opts$path) || is.na(opts$path)) {
        opts$path <- getwd()
      }
      if (is.null(opts$version) || is.na(opts$version)) {
        opts$version <- NULL
      }
      RSuite::prj_zip(path = opts$path, zip_ver = opts$version)
    }
  ),
  pack = list(
    help = "Build project sources pack.",
    options = list(
      make_option(c("-p", "--path"), dest = "path",
                  help="Directory to put built pack into (default: current directory)"),
      make_option(c("--version"), dest = "version",
                  help="Version to use for pack tagging (default: use ZipVersion form PARAMETERS and revision from RC)")
    ),
    run = function(opts) {
      if (is.null(opts$path) || is.na(opts$path)) {
        opts$path <- getwd()
      }
      if (is.null(opts$version) || is.na(opts$version)) {
        opts$version <- NULL
      }
      RSuite::prj_pack(path = opts$path, pack_ver = opts$version)
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you manage R projects."
)
