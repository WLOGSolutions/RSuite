#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# This script uploads project packages onto S3 repository.
#
# This script was generated authomaticaly by RSuite. Do not change it please.
#----------------------------------------------------------------------------

args <- commandArgs()
if (any(grepl("--usage", args))) {
  cat("This script builds project packages and synchronizes them into S3 repository.\n")
  cat("Call: Rscript 05_sync_s3_repo.R <args>\n")
  cat("\t--verbose          if passed print lots of messages\n")
  cat("\t--repo=<URL>       url to S3 repository to synchronize (required)\n")
  cat("\t--source           enforces synchronization of source or binary packages\n")
  cat("\t--binary              into S3 repository. If neither passed will synch\n")
  cat("\t                      system native type of packages. source option have\n")
  cat("\t                      priority over binary if both passed\n")
  cat("\t--usage            print this message and exit\n")
  stop("Noithing else to be done")
}

if (any(grepl("--verbose", args))) {
  logging::setLevel("DEBUG")
}

binary <- (.Platform$pkgType != "source")
if (any(grepl("--source", args))) {
  binary <- FALSE
} else if (any(grepl("--source", args))) {
  binary <- TRUE
}

ra_url <- args[startsWith(args, "--repo=")]
if (!length(ra_url)) {
  stop("Repository URL is required; please provide --repo parameter.")
}
ra_url <- gsub("^--repo=", "", ra_url[[1]])

# Check aws credentials
home_bkp <- Sys.getenv("HOME")

curr_dir <- shortPathName(normalizePath("."))
if (dir.exists(file.path(curr_dir, ".aws"))) {
  logging::loginfo("Setting HOME to %s", curr_dir)
  Sys.setenv(HOME=curr_dir)
} else if (!is.null(home_bkp) && !dir.exists(file.path(home_bkp, ".aws"))) {
  logging::loginfo("Unsetting HOME as it does not contain .aws credentials")
  Sys.unsetenv("HOME")
}

tryCatch({
  mgr <- RSuite::repo_mng_start("S3", url = ra_url)
  tryCatch({
    RSuite::repo_upload_prj_package("S3",
                                    pkg = NULL, # all project packages
                                    binary = binary)
  }, finally = {
    RSuite::repo_mng_stop(mgr)
  })
}, finally = {
  Sys.setenv(HOME = home_bkp)
})
