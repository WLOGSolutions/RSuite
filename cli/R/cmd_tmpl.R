
#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'tmpl' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
if (grepl("darwin", R.version$os)) {
  base <- gsub("~\\+~", " ", base) # on MacOS ~+~ in path denotes whitespace
}
source(file.path(base, "command_mgr.R"), chdir = T)

sub_commands <- list(
  list = list(
    help = "List all available project/package templates",
    options = list(
    ),
    run = function(opts) {
      tmpl_list <- RSuite::tmpl_list_registered()
      write.dcf(tmpl_list) # just print as dcf onto console
    }
  ),
  start = list(
    help = "Create template",
    options = list(
      make_option(c("-n", "--name"), dest = "name", default = NULL,
                  help = "Name of the package template to create."),
      make_option(c("-p", "--path"), dest = "path", default = NULL,
                  help = paste("Path to the directory where the project template will be created.",
                               "All templates are created by default in the working directory.",
                               "(default: %default)",
                               sep = "\n\t\t")),
      make_option("--prj", dest = "add_prj", action = "store_true", default = FALSE,
                  help = paste("Include project template. If non of --prj or --pkg are passed will create",
                               "template which includes both project and package template.",
                               "(default: %default)",
                               sep = "\n\t\t")),
      make_option("--pkg", dest = "add_pkg", action = "store_true", default = FALSE,
                  help = paste("Include package template. If non of --prj or --pkg are passed will create",
                               "template which includes both project and package template.",
                               "(default: %default)",
                               sep = "\n\t\t"))
      ),
      run = function(opts) {
        if (is.null(opts$name)) {
          stop("Template name is required. Please, provide --name argument.")
        }

        if (is.null(opts$path)) {
          opts$path <- getwd()
        }

        if (!opts$add_prj && !opts$add_pkg) {
          opts$add_prj <- TRUE
          opts$add_pkg <- TRUE
        }

        RSuite::tmpl_start(opts$name, opts$path, opts$add_prj, opts$add_pkg)
      }
  ),
  register = list(
    help = "Register template.",
    options = list(
      make_option(c("-p", "--path"), dest = "path", default = NULL,
                  help = paste("Path to the directory contaning the template to register.",
                               "(default: %default)",
                               sep = "\n\t\t")),
      make_option(c("-g", "--global"), dest = "global", action = "store_true", default = FALSE,
                  help = paste("If passed will register template in the global template",
                               "directory. (default: %default)",
                               sep = "\n\t\t"))
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
