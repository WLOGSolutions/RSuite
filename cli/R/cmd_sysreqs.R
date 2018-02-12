#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'sysreqs' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_mgr.R"), chdir = T)

sub_commands <- list(
  collect = list(
    help = "Collect system requirements over project and its dependencies and prints them.",
    options = list(
    ),
    run = function(opts) {
      sysreqs <- RSuite::sysreqs_collect()

      for(pkg in names(sysreqs)) {
        cat(sprintf("%s:\n", pkg))
        cat(sprintf("\t%s\n", sysreqs[[pkg]]))
      }
    }
  ),
  check = list(
    help = "Checks current system against required by project system libraries and tools.",
    options = list(
    ),
    run = function(opts) {
      RSuite::sysreqs_check()
    }
  ),
  install = list(
    help = "Installs system requirements required by project and its dependencies.",
    options = list(
    ),
    run = function(opts) {
      RSuite::sysreqs_install()
    }
  ),
  script = list(
    help = "Creates system specific script to install all system requirements for the project.",
    options = list(
    ),
    run = function(opts) {
      RSuite::sysreqs_script()
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you manage R projects system requirements like libraries and tools."
)
