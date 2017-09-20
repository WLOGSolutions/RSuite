#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Generic command management utility.
#----------------------------------------------------------------------------

.fatal_error <- function(messages) {
  for(m in messages) {
    write(sprintf("ERROR: %s", m), stderr())
  }
  quit(save = "no", status = 1, runLast = FALSE)
}
.base_dir <- normalizePath(".")
if (.Platform$OS.type == "windows") {
  .base_dir <- shortPathName(.base_dir)
}

tryCatch({
  suppressWarnings({
    suppressPackageStartupMessages({
      if (!require(optparse)) {
        c_url <- sprintf("file:///%s", contrib.url(file.path(.base_dir, "packages"), type = "source"))
        install.packages(c("optparse", "getopt"), contriburl = c_url, type = "source", quiet = T)
      }
      library(optparse)
    })
  })
}, error = function(e) {
  .fatal_error(c(geterrmessage(),
                 "optparse is required for RSuite CLI to work. Tried to install it but failed."))
})

tryCatch({
  suppressWarnings({
    suppressPackageStartupMessages({
      if (!require(logging)) {
        c_url <- sprintf("file:///%s", contrib.url(file.path(.base_dir, "packages"), type = "source"))
        install.packages(c("logging"), contriburl = c_url, type = "source", quiet = T)
      }
      library(optparse)
    })
  })
}, error = function(e) {
  .fatal_error(c(geterrmessage(),
                 "logging is required for RSuite CLI to work. Tried to install it but failed."))
})


handle_subcommands <- function(sub_commands, cmd_help) {
  args <- commandArgs(trailingOnly = T)

  cmd <- args[1]
  sub_cmd <- args[2]
  sub_cmd_args <- args[-(1:2)]

  if (is.na(sub_cmd) || sub_cmd %in% c("-h", "--help")) {
    sub_cmd <- "help"
  }

  sub_commands$help <- list(
    help = "Show this message and exit.",
    options = list(),
    run = function(opts) {
      cat(sprintf("%s\n", cmd_help))
      cat(sprintf("Usage: rsuite %s [subcmd] [subcmd args]\n\n", cmd))
      cat("Sub-commands:\n")
      for(subcmd in names(sub_commands)) {
        cat(sprintf("\t%s\n", subcmd))
        cat(sprintf("\t\t%s\n\n", sub_commands[[subcmd]]$help))
      }
      cat("\n")
      cat(sprintf("Call 'rsuite %s [subcmd] -h' to get more information on [subcmd args].\n", cmd))
    }
  )

  if (is.null(sub_commands[[sub_cmd]])) {
    .fatal_error(
       sprintf("Sub-command %s is unknown. Call 'rsuite %s help' to see acceptable sub-commands.",
               sub_cmd, cmd))
  }

  if (sub_cmd != "help") {
    rsuite_libloc <- Sys.getenv("RSUITE_LIBLOC")
    if (nchar(rsuite_libloc) == 0) {
      rsuite_libloc <- NULL
    }
    tryCatch({
      suppressWarnings({
        suppressPackageStartupMessages(library(RSuite, lib.loc = rsuite_libloc))
      })
    }, error = function(e) {
      .fatal_error(c(geterrmessage(),
                     "RSuite seems not to be available. Please run rsuite install to install it.",
                     sprintf("Failed to find it at: %s", rsuite_libloc)))
    })

    rsuite_ver <- packageVersion("RSuite", lib.loc = rsuite_libloc)
    cli_ver <- suppressWarnings(readLines(file.path(.base_dir, "..", "version.txt")))
    if (gsub("^(\\d+[.]\\d+)[.-]\\d+$", "\\1", rsuite_ver) != gsub("^(\\d+[.]\\d+)[.-]\\d+$", "\\1", cli_ver)) {
      .fatal_error(sprintf("RSuite v%s is not compatible with RSuite cli v%s", rsuite_ver, cli_ver))
    }

    sub_commands[[sub_cmd]]$options <- c(
      sub_commands[[sub_cmd]]$options,
      make_option(c("-v", "--verbose"), dest = "verbose", action="store_true", default=FALSE,
                  help="Show lots of messages (default: %default)"),
      make_option(c("-q", "--quiet"), dest = "quiet", action="store_true", default=FALSE,
                  help="Show only critical messages (default: %default)")
    )
  }
  tryCatch({
    opts <- parse_args(OptionParser(option_list=sub_commands[[sub_cmd]]$options,
                                    usage = sprintf("rsuite %s %s [options]", cmd, sub_cmd),
                                    description = cmd_help),
                       args = sub_cmd_args)
  }, error = function(e){
    .fatal_error(geterrmessage())
  })

  lev <- "INFO"
  if (!is.null(opts$quiet) && any(opts$quiet)) {
    lev <- "CRITICAL"
  } else if (!is.null(opts$verbose) && any(opts$verbose)) {
    lev <- "DEBUG"
  }
  logging::setLevel(lev)
  logging::setLevel(lev, logging::getLogger("rsuite"))

  tryCatch({
    sub_commands[[sub_cmd]]$run(opts)
  }, error = function(e) {
    .fatal_error(geterrmessage())
  })
}
