#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'install' command or CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = T)[-1]

if (any(args == "--help" | args == "-h")) {
  cat("Install RSuite package with all its required dependencies.\n")
  cat("Usage: rsuite install [options]\n\n")
  cat("Options:\n")
  cat("\t-u URL, --url=URL\n")
  cat("\t\tUse provided url to search for RSuite package.\n")
  cat("\t\t(default: http://wlog-rsuite.s3.amazonaws.com)\n\n")
  cat("\t-b BINARY, --binary=BINARY\n")
  cat("\t\tInstall from binary form (default: FALSE).\n\n")
  cat("\t-v, --verbose\n\n")
  cat("\t\tRun installation in verbose mode with lots of messages\n")
  cat("\t\twrittten to console.\n\n")
  cat("\t-h, --help\n")
  cat("\t\tShow this message and exit\n\n")
} else {
  # parsing arguments
  urls <- c()
  verbose <- FALSE
  binary <- FALSE

  ix = 1
  while(ix <= length(args)) {
    arg <- args[[ix]]
    if (grepl("^--url=", arg)) {
      urls <- c(urls, gsub("^--url=", "", arg))
      ix <- ix + 1
    } else if (arg == "-u") {
      if (ix == length(args)) {
        stop("-u requires argument. Call 'rsuite install --help' for mode information")
      }
      urls <- c(urls, args[[ix+1]])
      ix <- ix + 2
    } else if (grepl("^--binary=", arg)) {
      val <- as.logical(gsub("^--binary=", "", arg))
      if (!is.na(val)) {
        binary <- val
      }
      ix <- ix + 1
    } else if (arg == "-b") {
      if (ix == length(args)) {
        stop("-b requires argument. Call 'rsuite install --help' for mode information")
      }
      val <- as.logical(args[[ix+1]])
      if (!is.na(val)) {
        binary <- val
      }
      ix <- ix + 2
    } else if (arg == "-v" || arg == "--verbose") {
      verbose <- TRUE
      ix <- ix + 1
    } else {
      stop(sprintf("Unknown argument passed: '%s'. Use help option, please.", arg))
    }
  }
  if (!length(urls)) {
    urls <- 'http://wlog-rsuite.s3.amazonaws.com'
  }

  message("Detecting repositories ...")

  get_cran_path <- function() {
    official_repo_path <- getOption('repos') # This can point to CRAN mirror
    if ('@CRAN@' %in% official_repo_path) {
      official_repo_path <- getCRANmirrors()$URL[[1]]
    }
    return(official_repo_path)
  }
  all_repos <- c(CRAN = get_cran_path(), Other = urls)

  message("Will use repositories:")
  for(n in names(all_repos)) {
    message(sprintf("\t%10s = %s", n, all_repos[[n]]))
  }


  message("Installing RSuite package...")

  required_pkgs <- c("RSuite")
  wd <- setwd(.libPaths()[1]) # set wd to place there .Rprofile does not exist
  tryCatch({
    utils::install.packages(required_pkgs,
                            repos = all_repos,
                            quiet = !verbose,
                            verbose = verbose,
                            type = ifelse(binary, "binary", "source"))
  }, finally = { setwd(wd) })
  
  installed_pkgs <- installed.packages()[, "Package"]
  uninstalled <- setdiff(required_pkgs, installed_pkgs)
  if (length(uninstalled)) {
    stop(sprintf("Failed to install %s", paste(uninstalled, collapse = ", ")))
  }

  message("All done.")
}
