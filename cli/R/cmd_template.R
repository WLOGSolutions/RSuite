
#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'template' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_mgr.R"), chdir = T)

sub_commands <- list(
  get = list(
    help = "List all available project/package templates",
    options = list(
    ),
    run = function(opts) {
      RSuite::rsuite_get_templates()
    }
  ),
  pkgadd = list(
    help = "Create package template.",
    options = list(
      make_option(c("-n", "--name"), dest = "name",
                  help = "Name of the package template to create."),
      make_option(c("-p", "--path"), dest = "path",
                  help = paste("Path to the directory where the project template will be created.",
                               "All templates are created by default in the default template directory.",
                               "(default: NA)",
                               sep = "\n\t\t"))
    ),
    run = function(opts) {
      if (is.na(opts$name) || is.null(opts$name)) {
        stop("Package template name is required. Provide --name argument.")
      }
      if (is.null(opts$version) || is.na(opts$version)) {
        opts$version <- NA
      }

      RSuite::rsuite_start_pkg_template(opts$name, opts$path)
    }
  ),
  prjadd = list(
    help = "Create project template.",
    options = list(
      make_option(c("-n", "--name"), dest = "name",
                  help = "Name of the project template to create."),
      make_option(c("-p", "--path"), dest = "path",
                  help = paste("Path to the directory where the project template will be created.",
                               "All templates are created by default in the default template directory.",
                               "(default: NA)",
                               sep = "\n\t\t"))
    ),
    run = function(opts) {
      if (is.na(opts$name) || is.null(opts$name)) {
        stop("Project template name is required. Provide --name argument.")
      }

      if (is.null(opts$version) || is.na(opts$version)) {
        opts$version <- NA
      }

      RSuite::rsuite_start_prj_template(opts$name, opts$path)
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you manage R templates."
)
