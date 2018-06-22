
#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'tmpl' command of CLI utility.
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
      RSuite::tmpl_get_registered()
    }
  ),
  pkgadd = list(
    help = "Create package template.",
    options = list(
      make_option(c("-n", "--name"), dest = "name",
                  help = "Name of the package template to create."),
      make_option(c("-p", "--path"), dest = "path", default = NULL,
                  help = paste("Path to the directory where the project template will be created.",
                               "All templates are created by default in the default template directory.",
                               "(default: %default)",
                               sep = "\n\t\t"))
    ),
    run = function(opts) {
      if (is.na(opts$name) || is.null(opts$name)) {
        stop("Package template name is required. Provide --name argument.")
      }

      RSuite::tmpl_start_pkg(opts$name, opts$path)
    }
  ),
  prjadd = list(
    help = "Create project template.",
    options = list(
      make_option(c("-n", "--name"), dest = "name",
                  help = "Name of the project template to create."),
      make_option(c("-p", "--path"), dest = "path", default = NULL,
                  help = paste("Path to the directory where the project template will be created.",
                               "All templates are created by default in the default template directory.",
                               "(default: %default)",
                               sep = "\n\t\t"))
    ),
    run = function(opts) {
      if (is.na(opts$name) || is.null(opts$name)) {
        stop("Project template name is required. Provide --name argument.")
      }

      RSuite::tmpl_start_prj(opts$name, opts$path)
    }
  ),
  register = list(
    help = "Register template.",
    options = list(
      make_option(c("-p", "--path"), dest = "path", default = NULL,
                  help = paste("Path to the directory contaning the  template to register.",
                               "(default: %default)",
                               sep = "\n\t\t")),
      make_option(c("-g", "--global"), dest = "global", action = "store_true", default = FALSE,
                  help = paste("Register template in the global template directory", "
                               (default: %default"))
    ),
    run = function(opts) {
      RSuite::tmpl_register(opts$path, opts$global)
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you manage R templates."
)
