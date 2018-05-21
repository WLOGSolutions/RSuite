#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# This script builds local project environment.
#  It collects packages the project depends on and installs them in local project environment.
#  Dependencies are collected from packages and master scripts.
#
# This script was generated authomaticaly by RSuite. Do not change it please.
#----------------------------------------------------------------------------

args <- commandArgs()
if (any(grepl("--usage", args))) {
  cat("This script collects project dependencies to external packages and installs them to project lib folder.\n")
  cat("Call: Rscript 01_install_dependencies.R <args>\n")
  cat("\t--clean            cleanup local environment before proceeding\n")
  cat("\t--no-cran          do not use CRAN for dependencies detection (usefull for testing)\n")
  cat("\t--no-s3            do not use S3 repository for dependencies detection (usefull for testing)\n")
  cat("\t--no-local         do not use Local repository for dependencies detection (usefull for testing)\n")
  cat("\t--verbose          if passed print lots of messages\n")
  cat("\t--usage            print this message and exit\n")
  stop("Noithing else to be done")
}

if (any(grepl("--verbose", args))) {
  logging::setLevel("DEBUG")
}
RSuite::prj_install_deps(
  clean = any(grepl("--clean", args))
)
